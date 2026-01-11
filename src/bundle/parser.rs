use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    iter::once,
};

use anyhow::Result;
use quote::ToTokens;
use syn::{Attribute, ItemUse, Meta, UseGroup, UsePath, UseTree, visit::Visit};

use crate::{
    bundle::data::{Data, UseType},
    snippet::parser::unquote,
};

#[derive(Default)]
struct Visitor {
    use_items: Vec<ItemUse>,
    expanded_names: BTreeSet<String>,
}

impl Visitor {
    fn extract_use(
        tree: &UseTree,
        path: &mut Vec<UseType>,
        results: &mut VecDeque<Vec<UseType>>,
        data: &BTreeMap<String, Data>,
    ) {
        match tree {
            UseTree::Path(UsePath { ident, tree, .. }) => {
                if path.is_empty() && !data.contains_key(&ident.to_string()) {
                    return;
                }

                path.push(UseType::Path(ident.to_string()));
                Self::extract_use(tree, path, results, data);
                path.pop();
            }
            UseTree::Name(name) => {
                results.push_back(
                    path.iter()
                        .cloned()
                        .chain(once(UseType::Name(name.ident.to_string())))
                        .collect(),
                );
            }
            UseTree::Glob(_) => {
                results.push_back(path.iter().cloned().chain(once(UseType::Glob)).collect());
            }
            UseTree::Group(UseGroup { items, .. }) => {
                for item in items {
                    Self::extract_use(item, path, results, data);
                }
            }
            _ => {}
        }
    }

    fn process_attributes(&mut self, attrs: &[Attribute]) {
        for attr in attrs {
            if let Ok(metaitem) = attr.parse_meta()
                && metaitem.path().to_token_stream().to_string().as_str()
                    == "cargo_snippet :: expanded"
                && let Meta::NameValue(nv) = metaitem
            {
                self.expanded_names
                    .insert(unquote(&nv.lit.into_token_stream().to_string()));
            }
        }
    }
}

impl<'ast> Visit<'ast> for Visitor {
    fn visit_item_use(&mut self, node: &'ast ItemUse) {
        self.use_items.push(node.clone());
        syn::visit::visit_item_use(self, node);
    }
}

pub fn get_should_bundle(content: &String, data: &BTreeMap<String, Data>) -> Result<String> {
    let syntax_tree = syn::parse_file(content)?;

    let mut visitor = Visitor::default();
    visitor.visit_file(&syntax_tree);

    let mut use_results = VecDeque::new();
    for use_item in &visitor.use_items {
        let mut path = vec![];
        Visitor::extract_use(&use_item.tree, &mut path, &mut use_results, data);
    }
    visitor.process_attributes(&syntax_tree.attrs);

    let mut needs_results = BTreeMap::new();
    extract_needs(use_results, &mut needs_results, data);

    Ok(make_content(&needs_results, &visitor.expanded_names))
}

fn extract_needs(
    mut q: VecDeque<Vec<UseType>>,
    results: &mut BTreeMap<String, String>,
    data: &BTreeMap<String, Data>,
) {
    while let Some(path) = q.pop_front() {
        let UseType::Path(ref root) = path[0] else {
            continue;
        };

        let mut lib;
        let Data { library, pathes } = &data[root];

        lib = library;
        for p in path[1..].iter() {
            match p {
                UseType::Name(name) => {
                    let lib = &lib.files[name];
                    if !results.contains_key(&lib.name) {
                        results.insert(lib.name.clone(), lib.content.clone());
                        for deps in &lib.dependencies {
                            q.push_back(
                                once(UseType::Path(root.clone()))
                                    .chain(pathes[deps].clone())
                                    .collect(),
                            );
                        }
                    }
                }
                UseType::Path(name) => {
                    lib = &*lib.childs[name];
                }
                UseType::Glob => {
                    let mut q2 = VecDeque::new();
                    q2.push_back(lib);
                    while let Some(library) = q2.pop_front() {
                        for (_, lib) in &library.files {
                            if !results.contains_key(&lib.name) {
                                results.insert(lib.name.clone(), lib.content.clone());
                                for deps in &lib.dependencies {
                                    q.push_back(
                                        once(UseType::Path(root.clone()))
                                            .chain(pathes[deps].clone())
                                            .collect(),
                                    );
                                }
                            }
                        }

                        for (_, child) in &library.childs {
                            q2.push_back(&*child);
                        }
                    }
                }
            }
        }
    }
}

fn make_content(needs: &BTreeMap<String, String>, expanded: &BTreeSet<String>) -> String {
    let mut content = String::new();
    for (name, lib) in needs {
        if !expanded.contains(name) {
            content += lib;
        }
    }

    content
}
