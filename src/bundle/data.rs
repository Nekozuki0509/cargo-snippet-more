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
    fn new() -> Self {
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
                    .cloned()
                    .map(|x| UseType::Path(x))
                    .chain(once(UseType::Name(lib.name.clone())))
                    .collect(),
            );
            let mut s: &mut Libraries = &mut self.library;
            for i in &lib.path {
                s = s
                    .childs
                    .entry(i.clone())
                    .or_insert(Box::new(Libraries::new()));
            }

            s.files.insert(
                lib.name.clone(),
                Library {
                    name: name.clone(),
                    dependencies: deps.get(&name).unwrap_or(&BTreeSet::new()).clone(),
                    content: lib.content.clone(),
                },
            );
        }
    }

    pub fn write(&self) -> Result<(), Error> {
        let mut file = File::create("libraries.toml")?;
        let toml = toml::to_string(self).unwrap();
        write!(file, "{}", toml)?;
        file.flush()?;

        Ok(())
    }

    pub fn read(path: &str) -> Result<Self, Error> {
        let file = fs::read_to_string(path)?;
        toml::from_str::<Self>(&file).context("")
    }
}
