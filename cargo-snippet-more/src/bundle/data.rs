use std::{
    collections::{BTreeMap, BTreeSet},
    fs::{self, File},
    io::Write,
    iter::once,
};

use anyhow::{Context, Error};
use serde_derive::{Deserialize, Serialize};

use crate::snippet::snippet::Lib;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum UseType {
    Glob,
    Path(String),
    Name(String),
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Library {
    pub name: String,
    pub dependencies: BTreeSet<String>,
    pub content: String,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Libraries {
    pub childs: BTreeMap<String, Box<Libraries>>,
    pub files: BTreeMap<String, Library>,
}

impl Libraries {
    pub fn new() -> Self {
        Self {
            childs: BTreeMap::new(),
            files: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Data {
    pub library: Libraries,
    pub pathes: BTreeMap<String, Vec<UseType>>,
}

impl Data {
    pub fn new() -> Self {
        Self {
            library: Libraries::new(),
            pathes: BTreeMap::new(),
        }
    }

    pub fn push(&mut self, libs: BTreeMap<String, Lib>, deps: &BTreeMap<String, BTreeSet<String>>) {
        for (name, lib) in libs {
            self.pathes.insert(
                name.clone(),
                lib.path
                    .iter()
                    .map(|x| UseType::Path(x.clone()))
                    .chain(once(UseType::Name(lib.name.clone())))
                    .collect(),
            );
            let mut s: &mut Libraries = &mut self.library;
            for i in &lib.path {
                s = s
                    .childs
                    .entry(i.clone())
                    .or_insert_with(|| Box::new(Libraries::new()));
            }

            s.files.insert(
                lib.name.clone(),
                Library {
                    name: name.clone(),
                    dependencies: deps.get(&name).cloned().unwrap_or_default(),
                    content: lib.content.clone(),
                },
            );
        }
    }

    pub fn write(&self) -> Result<(), Error> {
        let mut file = File::create("libraries.toml")?;
        let toml = toml::to_string(self)
            .context("Failed to serialize data to TOML format")?;
        write!(file, "{}", toml)?;
        file.flush()?;

        Ok(())
    }

    pub fn read(path: &str) -> Result<Self, Error> {
        let file = fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {}", path))?;
        toml::from_str::<Self>(&file)
            .with_context(|| format!("Failed to parse TOML from file: {}", path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::snippet::snippet::Lib;

    #[test]
    fn test_libraries_new() {
        let libs = Libraries::new();
        assert!(libs.childs.is_empty());
        assert!(libs.files.is_empty());
    }

    #[test]
    fn test_data_new() {
        let data = Data::new();
        assert!(data.pathes.is_empty());
        assert!(data.library.childs.is_empty());
        assert!(data.library.files.is_empty());
    }

    #[test]
    fn test_use_type_ordering() {
        let glob = UseType::Glob;
        let path1 = UseType::Path("a".to_string());
        let path2 = UseType::Path("b".to_string());
        let name1 = UseType::Name("x".to_string());
        
        assert!(glob < path1);
        assert!(path1 < path2);
        assert!(path1 < name1);
    }

    #[test]
    fn test_data_push_simple() {
        let mut data = Data::new();
        let mut libs = BTreeMap::new();
        let deps = BTreeMap::new();
        
        let lib = Lib {
            name: "test_fn".to_string(),
            path: vec!["lib".to_string()],
            content: "fn test() {}".to_string(),
        };
        
        libs.insert("test_snippet".to_string(), lib);
        data.push(libs, &deps);
        
        assert!(data.pathes.contains_key("test_snippet"));
        assert!(!data.library.childs.is_empty());
    }

    #[test]
    fn test_data_push_with_dependencies() {
        let mut data = Data::new();
        let mut libs = BTreeMap::new();
        let mut deps = BTreeMap::new();
        
        // Base library
        let base_lib = Lib {
            name: "base_fn".to_string(),
            path: vec!["lib".to_string()],
            content: "fn base() {}".to_string(),
        };
        libs.insert("base".to_string(), base_lib);
        
        // Derived library with dependency
        let derived_lib = Lib {
            name: "derived_fn".to_string(),
            path: vec!["lib".to_string()],
            content: "fn derived() {}".to_string(),
        };
        libs.insert("derived".to_string(), derived_lib);
        
        let mut derived_deps = BTreeSet::new();
        derived_deps.insert("base".to_string());
        deps.insert("derived".to_string(), derived_deps);
        
        data.push(libs, &deps);
        
        assert!(data.pathes.contains_key("base"));
        assert!(data.pathes.contains_key("derived"));
        
        // Check that derived has base as dependency
        let lib_root = &data.library.childs["lib"];
        let derived_lib = &lib_root.files["derived_fn"];
        assert!(derived_lib.dependencies.contains("base"));
    }

    #[test]
    fn test_data_push_nested_paths() {
        let mut data = Data::new();
        let mut libs = BTreeMap::new();
        let deps = BTreeMap::new();
        
        let lib = Lib {
            name: "test_fn".to_string(),
            path: vec!["lib".to_string(), "sub".to_string(), "module".to_string()],
            content: "fn test() {}".to_string(),
        };
        
        libs.insert("test_snippet".to_string(), lib);
        data.push(libs, &deps);
        
        // Verify nested structure
        assert!(data.library.childs.contains_key("lib"));
        let lib_node = &data.library.childs["lib"];
        assert!(lib_node.childs.contains_key("sub"));
        let sub_node = &lib_node.childs["sub"];
        assert!(sub_node.childs.contains_key("module"));
        let module_node = &sub_node.childs["module"];
        assert!(module_node.files.contains_key("test_fn"));
    }

    #[test]
    fn test_library_ordering() {
        let lib1 = Library {
            name: "a".to_string(),
            dependencies: BTreeSet::new(),
            content: "fn a() {}".to_string(),
        };
        
        let lib2 = Library {
            name: "b".to_string(),
            dependencies: BTreeSet::new(),
            content: "fn b() {}".to_string(),
        };
        
        assert!(lib1 < lib2);
    }

    #[test]
    fn test_data_serialization() {
        let data = Data::new();
        let serialized = toml::to_string(&data);
        assert!(serialized.is_ok());
        
        let deserialized: Result<Data, _> = toml::from_str(&serialized.unwrap());
        assert!(deserialized.is_ok());
    }
}
