use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{Error, Write},
};

use serde_derive::{Deserialize, Serialize};

use crate::snippet::Lib;

#[derive(Debug, Serialize, Deserialize)]
pub struct Library {
    dependencies: BTreeSet<String>,
    content: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Libraries {
    childs: BTreeMap<String, Box<Libraries>>,
    files: BTreeMap<String, Library>,
}

impl Libraries {
    pub fn new() -> Self {
        Self {
            childs: BTreeMap::new(),
            files: BTreeMap::new(),
        }
    }

    pub fn push(&mut self, libs: BTreeMap<String, Lib>, deps: &BTreeMap<String, BTreeSet<String>>) {
        for (name, lib) in libs {
            let mut s: &mut Libraries = self;
            for i in &lib.path {
                s = s
                    .childs
                    .entry(i.clone())
                    .or_insert(Box::new(Libraries::new()));
            }

            s.files.insert(
                lib.name.clone(),
                Library {
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
}
