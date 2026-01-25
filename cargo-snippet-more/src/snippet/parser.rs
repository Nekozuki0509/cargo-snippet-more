use anyhow::Context;
use lazy_static::lazy_static;
use proc_macro2::{Delimiter, TokenStream, TokenTree};
use quote::ToTokens;
use regex::{Captures, Regex};
use syn::visit::Visit;
use syn::{Attribute, File, Item, Macro, Meta, MetaList, NestedMeta, parse_file};

use std::collections::HashSet;
use std::{char, u32};

use crate::snippet::snippet::{Snippet, SnippetAttributes};

struct Visitor<'a> {
    source: &'a str,
    snippets: Vec<Snippet>,
}

impl<'a> Visit<'a> for Visitor<'a> {
    fn visit_macro(&mut self, mac: &'a Macro) {
        let path = mac.path.to_token_stream().to_string().replace(' ', "");

        if (path == "snippet_start" || path == "cargo_snippet_more::snippet_start") 
            && let Some(params) = parse_macro_params(mac) 
        {
            let re = Regex::new(dbg!(&format!(r#"(?s)(cargo_snippet_more :: )?snippet_start ! \(("{0}"|.*name="{0}".*)\) ;.+(cargo_snippet_more :: )?snippet_end ! \("{0}"\) ;"#, params.names.iter().next().unwrap()))).unwrap();
            let content = re.find(dbg!(self.source)).unwrap().as_str();
            let file = syn::parse_str::<TokenStream>(content).unwrap();

            self.snippets.push(Snippet {
                name: String::new(),
                content: stringify_tokens(file, params.doc_hidden),
                attrs: params,
            });
        }
    }
}

fn parse_macro_params(mac: &Macro) -> Option<SnippetAttributes> {
    let tokens = mac.tokens.clone().into_iter().collect::<Vec<_>>();

    if tokens.is_empty() {
        return None;
    }

    let mut attrs = SnippetAttributes::default();
    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i] {
            TokenTree::Literal(lit) => {
                let value = lit.to_string();
                if value.starts_with('"') {
                    attrs.names.insert(unquote(&value));
                }

                i += 1;
            }
            TokenTree::Ident(ident) => {
                let key = ident.to_string();

                if i + 1 < tokens.len() 
                    && let TokenTree::Punct(p) = &tokens[i + 1] 
                    && p.as_char() == '=' && i + 2 < tokens.len() 
                {
                    if let TokenTree::Literal(lit) = &tokens[i + 2] {
                        let value = unquote(&lit.to_string());
                        match key.as_str() {
                            "name" => {
                                attrs.names.insert(value);
                            }
                            "include" => {
                                for u in value.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
                                    attrs.uses.insert(u.to_string());
                                }
                            }
                            "prefix" => {
                                if !attrs.prefix.is_empty() {
                                    attrs.prefix.push('\n');
                                }
                                attrs.prefix.push_str(&value);
                            }
                            _ => {}
                        }
                    }

                    i += 3;
                    continue;
                } 

                if key.as_str() == "doc_hidden" {
                    attrs.doc_hidden = true;
                }

                i += 1;
            }
            TokenTree::Punct(_) => i += 1,
            _ => i += 1
        }
    }

    if attrs.names.is_empty() {
        return None;
    }

    for name in &attrs.names {
        attrs.not_library.insert(name.clone());
    }

    Some(attrs)
}

fn is_snippet_path(path: &str) -> bool {
    match path {
        "snippet" | "cargo_snippet_more :: snippet" => true,
        _ => false,
    }
}

macro_rules! get_attrs_impl {
    ($arg: expr, $($v: path), *) => {
        {
            match $arg {
                $(
                    &$v(ref x) => Some(&x.attrs),
                )*
                _ => None
            }
        }
    }
}

fn get_attrs(item: &Item) -> Option<&Vec<Attribute>> {
    // All Item variants except Item::Verbatim
    get_attrs_impl!(
        item,
        Item::ExternCrate,
        Item::Use,
        Item::Static,
        Item::Const,
        Item::Fn,
        Item::Mod,
        Item::ForeignMod,
        Item::Type,
        Item::Struct,
        Item::Enum,
        Item::Union,
        Item::Trait,
        Item::Impl,
        Item::Macro,
        Item::Macro2
    )
}

macro_rules! remove_snippet_attr_impl {
    ($arg: expr, $($v: path), *) => {
        {
            match $arg {
                $(
                    &mut $v(ref mut x) => {
                        x.attrs.retain(|attr| {
                            attr.parse_meta().map(|m| !is_snippet_path(m.path().to_token_stream().to_string().as_str())).unwrap_or(true)
                        });
                    },
                )*
                _ => ()
            }
        }
    }
}

fn remove_snippet_attr(item: &mut Item) {
    remove_snippet_attr_impl!(
        item,
        Item::ExternCrate,
        Item::Use,
        Item::Static,
        Item::Const,
        Item::Fn,
        Item::Mod,
        Item::ForeignMod,
        Item::Type,
        Item::Struct,
        Item::Enum,
        Item::Union,
        Item::Trait,
        Item::Impl,
        Item::Macro,
        Item::Macro2
    );

    if let Item::Mod(item_mod) = item {
        if let Some(&mut (_, ref mut items)) = item_mod.content.as_mut() {
            items.iter_mut().for_each(|item| remove_snippet_attr(item));
        }
    }
}

pub fn unquote(s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();

    if chars.len() >= 2 && chars.first() == Some(&'"') && chars.last() == Some(&'"') {
        chars[1..chars.len() - 1].iter().collect()
    } else {
        chars.iter().collect()
    }
}

macro_rules! get_default_snippet_name_impl {
    ($arg:expr, $($v: path), *) => {
        match $arg {
            $(
                &$v(ref x) => {
                    Some(x.ident.to_string())
                },
            )*
            &Item::Fn(ref x) => {
                Some(x.sig.ident.to_string())
            }
            _ => None
        }
    };
}

fn get_default_snippet_name(item: &Item) -> Option<String> {
    get_default_snippet_name_impl!(
        item,
        Item::Static,
        Item::Const,
        Item::Mod,
        Item::Struct,
        Item::Enum,
        Item::Union,
        Item::Trait
    )
}

fn get_snippet_name(attr: &Attribute) -> Option<String> {
    attr.parse_meta().ok().as_ref().and_then(get_snippet_name_from_meta)
}

fn get_snippet_name_from_meta(metaitem: &Meta) -> Option<String> {
    if !is_snippet_path(metaitem.path().to_token_stream().to_string().as_str()) {
        return None;
    }

    match metaitem {
        // #[snippet(name="..")]
        Meta::List(list) => list
            .nested
            .iter()
            .filter_map(|item| match item {
                NestedMeta::Meta(Meta::NameValue(nv)) => {
                    if nv.path.to_token_stream().to_string() == "name" {
                        Some(unquote(&nv.lit.clone().into_token_stream().to_string()))
                    } else {
                        None
                    }
                }
                NestedMeta::Lit(lit) => {
                    Some(unquote(lit.to_token_stream().to_string().as_str()))
                }
                _ => None,
            })
            .next(),
        // #[snippet=".."]
        Meta::NameValue(nv) => Some(unquote(&nv.lit.clone().into_token_stream().to_string())),
        _ => None,
    }
}

fn get_snippet_uses(attr: &Attribute) -> Option<Vec<String>> {
    attr.parse_meta().ok().and_then(|metaitem| {
        if !is_snippet_path(metaitem.path().to_token_stream().to_string().as_str()) {
            return None;
        }

        match metaitem {
            // #[snippet(include="..")]
            Meta::List(list) => list
                .nested
                .iter()
                .filter_map(|item| {
                    if let NestedMeta::Meta(Meta::NameValue(nv)) = item {
                        // It can't use "use" keyword here xD.
                        // It is reserved.
                        if nv.path.to_token_stream().to_string() == "include" {
                            let uses = unquote(&nv.lit.clone().into_token_stream().to_string());
                            Some(
                                uses.split(',')
                                    .map(|s| s.trim())
                                    .filter(|s| !s.is_empty())
                                    .map(|s| s.to_string())
                                    .collect(),
                            )
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .next(),
            _ => None,
        }
    })
}

fn get_simple_attr(attr: &Attribute, key: &str) -> Vec<String> {
    attr.parse_meta()
        .ok()
        .and_then(|metaitem| {
            if !is_snippet_path(metaitem.path().to_token_stream().to_string().as_str()) {
                return None;
            }

            match metaitem {
                // #[snippet(`key`="..")]
                Meta::List(list) => list
                    .nested
                    .iter()
                    .filter_map(|item| {
                        if let NestedMeta::Meta(Meta::NameValue(nv)) = item {
                            if nv.path.to_token_stream().to_string() == key {
                                let value = if let syn::Lit::Str(s) = &nv.lit.clone() {
                                    s.value()
                                } else {
                                    panic!("attribute must be string");
                                };
                                Some(value)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
                    .into(),
                _ => None,
            }
        })
        .unwrap_or(Vec::new())
}

fn parse_attrs(
    attrs: &[Attribute],
    default_snippet_name: Option<String>,
) -> Option<SnippetAttributes> {
    let meta_parsed = attrs
        .iter()
        .filter_map(|a| a.parse_meta().ok())
        .map(|m| {
            let is_snippet_path = is_snippet_path(m.path().to_token_stream().to_string().as_str());
            (m, is_snippet_path)
        })
        .collect::<Vec<_>>();

    if meta_parsed
        .iter()
        .all(|&(_, is_snippet_path)| !is_snippet_path)
    {
        return None;
    }

    let mut names = attrs
        .iter()
        .filter_map(get_snippet_name)
        .collect::<HashSet<_>>();

    let attr_snippet_without_value = meta_parsed.iter().any(|(meta, is_snippet_path)| {
        if !is_snippet_path {
            return false;
        }
        matches!(meta, Meta::Path(_))
    });

    if let Some(ref default) = default_snippet_name
        && attr_snippet_without_value
    {
        names.insert(default.clone());
    }

    if names.is_empty() {
        if let Some(default) = default_snippet_name {
            names.insert(default);
        } else {
            return None;
        }
    }

    let uses = attrs
        .iter()
        .filter_map(get_snippet_uses)
        .flat_map(|v| v.into_iter())
        .collect::<HashSet<_>>();

    let prefix = attrs
        .iter()
        .map(|attr| get_simple_attr(attr, "prefix").into_iter())
        .flatten()
        .collect::<Vec<_>>()
        .join("\n");

    let doc_hidden = meta_parsed.iter().any(|(meta, is_snippet_path)| {
        if !is_snippet_path {
            return false;
        }
        match meta {
            Meta::List(MetaList { nested, .. }) => nested.iter().any(|n|
                matches!(n, NestedMeta::Meta(Meta::Path(p)) if p.to_token_stream().to_string() == "doc_hidden")
            ),
            _ => false,
        }
    });

    let not_library = meta_parsed.iter().filter_map(|(meta, is_snippet_path)| {
        if !is_snippet_path {
            return None;
        }
        match meta {
            Meta::List(MetaList { nested, .. }) => {
                let has_not_library = nested.iter().any(|n|
                    matches!(n, NestedMeta::Meta(Meta::Path(p)) if p.to_token_stream().to_string() == "not_library")
                ); 

                if has_not_library {
                    get_snippet_name_from_meta(meta)
                } else {
                    None
                }
            },
            _ => None,
        }
    }).collect();

    Some(SnippetAttributes {
        names,
        uses,
        prefix,
        doc_hidden,
        not_library,
    })
}

fn next_token_is_doc(token: &TokenTree) -> bool {
    match token {
        TokenTree::Group(g) => g.to_string().starts_with("[doc = "),
        _ => false,
    }
}

fn unescape(s: impl Into<String>) -> String {
    lazy_static! {
        static ref ESCAPED_UNICODE: Regex = Regex::new(r"\\u\{([0-9a-fA-F]{1,6})\}").unwrap();
    }
    let s = s.into();
    let unicode_unescaped: Vec<char> = ESCAPED_UNICODE
        .replace_all(&s, |caps: &Captures| {
            caps.get(1)
                .and_then(|cap| u32::from_str_radix(cap.as_str(), 16).ok())
                .and_then(|u| char::from_u32(u))
                .map(|ch| ch.to_string())
                .unwrap_or(caps[0].to_string())
        })
        .chars()
        .collect();

    let mut ret = String::with_capacity(s.len());
    let mut iter = unicode_unescaped.iter().peekable();
    while let Some(&ch) = iter.next() {
        if ch == '\\' {
            match iter.peek() {
                Some(&next_ch) if *next_ch == '\\' => {
                    ret.push('\\');
                    iter.next();
                }
                Some(&next_ch) if *next_ch == '"' => {
                    ret.push('"');
                    iter.next();
                }
                Some(&next_ch) if *next_ch == 't' => {
                    ret.push('\t');
                    iter.next();
                }
                Some(&next_ch) if *next_ch == 'n' => {
                    ret.push('\n');
                    iter.next();
                }
                Some(&next_ch) if *next_ch == 'r' => {
                    ret.push('\r');
                    iter.next();
                }
                _ => unreachable!(),
            }
        } else {
            ret.push(ch);
        }
    }
    ret
}

fn format_doc_comment(doc_tt: TokenTree, is_inner: bool, doc_hidden: bool) -> Option<String> {
    lazy_static! {
        static ref DOC_RE: Regex = Regex::new(r#"^\[doc = "(?s)(.*)"\]$"#).unwrap();
    }
    if doc_hidden {
        return None;
    }

    let doc = unescape(doc_tt.to_string());
    DOC_RE
        .captures(doc.as_str())
        .and_then(|caps| caps.get(1))
        .map(|c| {
            c.as_str().lines().fold(String::new(), |mut acc, line| {
                let s = if is_inner {
                    format!("//!{}\n", line)
                } else {
                    format!("///{}\n", line)
                };
                acc.push_str(&s);
                acc
            })
        })
}

fn stringify_tokens(tokens: TokenStream, doc_hidden: bool) -> String {
    let mut res = String::new();
    let mut iter = tokens.into_iter().peekable();
    while let Some(tok) = iter.next() {
        match tok {
            TokenTree::Punct(ref punct) => {
                if punct.as_char() == '!' && iter.peek().map(next_token_is_doc).unwrap_or(false) {
                    // inner doc comment here.
                    // `res` already has a `#` character at the last, which is unnecessary, so remove it by calling pop.
                    if res.chars().last() == Some(' ') {
                        res.pop();
                    }
                    assert_eq!(res.pop(), Some('#'));
                    if let Some(doc) =
                        format_doc_comment(iter.next().unwrap(), true, doc_hidden).as_deref()
                    {
                        res.push_str(doc);
                    }
                } else if punct.as_char() == '#'
                    && iter.peek().map(next_token_is_doc).unwrap_or(false)
                {
                    // outer doc comment here.
                    if let Some(doc) =
                        format_doc_comment(iter.next().unwrap(), false, doc_hidden).as_deref()
                    {
                        res.push_str(doc);
                    }
                } else {
                    res.push_str(tok.to_string().as_str());
                    if punct.spacing() == proc_macro2::Spacing::Alone {
                        res.push(' ');
                    }
                }
            }
            TokenTree::Group(ref g) => {
                match g.delimiter() {
                    Delimiter::Parenthesis => res.push('('),
                    Delimiter::Brace => res.push('{'),
                    Delimiter::Bracket => res.push('['),
                    Delimiter::None => (),
                }
                res.push_str(stringify_tokens(g.stream(), doc_hidden).as_str());
                match g.delimiter() {
                    Delimiter::Parenthesis => res.push(')'),
                    Delimiter::Brace => res.push('}'),
                    Delimiter::Bracket => res.push(']'),
                    Delimiter::None => (),
                }
                res.push(' ');
            }
            _ => {
                res.push_str(tok.to_string().as_str());
                res.push(' ');
            }
        }
    }
    res
}

// Get snippet names and snippet code (not formatted)
fn get_snippet_from_item(mut item: Item) -> Option<Snippet> {
    let default_name = get_default_snippet_name(&item);
    let snip_attrs =
        get_attrs(&item).and_then(|attrs| parse_attrs(attrs.as_slice(), default_name.clone()));

    snip_attrs.map(|attrs| {
        remove_snippet_attr(&mut item);
        let doc_hidden = attrs.doc_hidden;
        Snippet {
            name: default_name.unwrap_or_default(),
            attrs,
            content: stringify_tokens(item.into_token_stream(), doc_hidden),
        }
    })
}

fn get_snippet_from_item_recursive(item: Item) -> Vec<Snippet> {
    let mut res = Vec::new();

    if let Some(pair) = get_snippet_from_item(item.clone()) {
        res.push(pair);
    }

    if let Item::Mod(mod_item) = item {
        res.extend(
            mod_item
                .content
                .into_iter()
                .flat_map(|(_, items)| items.into_iter().flat_map(get_snippet_from_item_recursive)),
        );
    }

    res
}

fn get_snippet_from_file(file: File) -> Vec<Snippet> {
    let mut res = Vec::new();
    // whole code is snippet
    if let Some(attrs) = parse_attrs(&file.attrs, None) {
        let mut file = file.clone();
        file.attrs.retain(|attr| {
            attr.parse_meta()
                .map(|m| !is_snippet_path(m.path().to_token_stream().to_string().as_str()))
                .unwrap_or(true)
        });
        file.items.iter_mut().for_each(|item| {
            remove_snippet_attr(item);
        });
        let doc_hidden = attrs.doc_hidden;
        res.push(Snippet {
            name: String::new(),
            attrs,
            content: stringify_tokens(file.into_token_stream(), doc_hidden),
        })
    }

    res.extend({
        let mut visitor = Visitor {
            source: &file.to_token_stream().to_string(),
            snippets: vec![],
        };
        visitor.visit_file(&file);

        visitor.snippets
    });

    res.extend(
        file.items
            .into_iter()
            .flat_map(get_snippet_from_item_recursive),
    );

    res
}

pub fn parse_snippet(src: &str) -> Result<Vec<Snippet>, anyhow::Error> {
    parse_file(src).map(get_snippet_from_file).context("")
}
