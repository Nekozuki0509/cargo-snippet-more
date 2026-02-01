use std::collections::{BTreeMap, BTreeSet, HashSet};

use lazy_static::lazy_static;
use regex::Regex;

use crate::bundle::data::Data;

lazy_static! {
    // This regex pattern is a compile-time constant and known to be valid
    static ref SNIPPET_MARKER_RE: Regex = Regex::new(r"(cargo_snippet_more :: )?snippet_(start|end) ! .+?;")
        .expect("Failed to compile snippet marker regex");
}

#[derive(Debug, Default)]
pub struct SnippetAttributes {
    // A snippet with multiple names is allowed but using dependency is recommended.
    pub names: HashSet<String>,
    // Dependencies
    pub uses: HashSet<String>,
    pub not_library: HashSet<String>,
    // Prefix for snippet. It's will be emitted prior to the snippet.
    pub prefix: String,
    // Whether doc comments associated with this snippet should be hidden or not.
    pub doc_hidden: bool,
}

#[derive(Debug)]
pub struct Snippet {
    pub name: String,
    pub attrs: SnippetAttributes,
    // Snippet content (Not formated)
    pub content: String,
}

#[derive(Debug, Default)]
pub struct Lib {
    pub name: String,
    pub path: Vec<String>,
    pub content: String,
}

pub fn process_snippets(
    snips: Vec<(Vec<String>, Vec<Snippet>)>,
) -> (Data, BTreeMap<String, String>) {
    #[derive(Default, Clone, Debug)]
    struct Snip {
        prefix: String,
        content: String,
    }

    let mut libs = BTreeMap::new();
    let mut pre: BTreeMap<String, Snip> = BTreeMap::new();
    let mut deps: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();

    for (path, snip_vec) in snips {
        for snip in snip_vec {
            let mut content = snip.content;
            content = SNIPPET_MARKER_RE.replace_all(&content, "").to_string();

            for name in &snip.attrs.names {
                if !snip.attrs.not_library.contains(name) {
                    if !libs.contains_key(name) {
                        libs.insert(
                            name.clone(),
                            Lib {
                                path: path.clone(),
                                name: snip.name.clone(),
                                content: content.clone(),
                            },
                        );
                    }

                    let l = libs.entry(name.clone()).or_default();
                    l.content += &snip.attrs.prefix;
                    l.content += &content;
                }

                let s = pre.entry(name.clone()).or_default();
                s.prefix += &snip.attrs.prefix;
                if s.content.is_empty() {
                    s.content += &format!("cargo_snippet_more::expanded!(\"{}\");", name);
                }
                s.content += &content;

                for dep in &snip.attrs.uses {
                    deps.entry(name.clone())
                        .or_insert_with(BTreeSet::new)
                        .insert(dep.clone());
                }
            }
        }
    }

    let mut data = Data::new();
    data.push(libs, &deps);
    let mut res: BTreeMap<String, Snip> = BTreeMap::new();

    for (name, uses) in &deps {
        let mut used = HashSet::new();
        used.insert(name.clone());
        let mut stack = uses.iter().cloned().collect::<Vec<_>>();

        while let Some(dep) = stack.pop() {
            if !used.contains(&dep) {
                used.insert(dep.clone());
                if let Some(c) = &pre.get(&dep) {
                    // *res.entry(name.clone()).or_insert_with(String::new) += c.as_str();
                    let s = res.entry(name.clone()).or_default();

                    s.prefix += &c.prefix;
                    s.content += &"#[rustfmt::skip]";
                    s.content += &c.content;

                    if let Some(ds) = deps.get(&dep) {
                        for d in ds {
                            if !used.contains(d) {
                                stack.push(d.clone());
                            }
                        }
                    }
                } else {
                    log::warn!("Dependency {} is missing", &dep);
                }
            }
        }
    }

    for (name, snip) in pre {
        // Dependency first
        let s = res.entry(name.clone()).or_default();
        s.prefix += snip.prefix.as_str();
        s.content += snip.content.as_str();
    }

    (
        data,
        res.into_iter()
            .map(|(k, v)| (k, v.prefix + v.content.as_str()))
            .collect(),
    )
}
