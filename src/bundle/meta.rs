use std::{collections::BTreeMap, io::Error};

use anyhow::Result;
use cargo_metadata::MetadataCommand;

use crate::bundle::config::BundleConfig;

#[derive(Debug)]
pub struct Metadatas {
    pub bin: String,
    pub library: BTreeMap<String, String>,
}

pub fn get_data(config: &BundleConfig) -> Result<Metadatas> {
    let metadata = MetadataCommand::new().exec()?;

    let mut bin_result = None;
    let mut lib_path = BTreeMap::new();
    for package in &metadata.packages {
        for target in &package.targets {
            if target.name == config.target && target.kind.iter().any(|k| k.to_string() == "bin") {
                bin_result = Some(target.src_path.to_string());
            }
        }

        if let Some(pathes) = package
            .metadata
            .get("cargo-snippet-more")
            .and_then(|x| x.get("library-path"))
            .and_then(|x| x.as_array())
        {
            for path in pathes.iter() {
                if let Some(lib_obj) = path.as_object() {
                    for (name, value) in lib_obj {
                        if let Some(path) = value.as_str() {
                            lib_path.insert(name.clone(), path.to_string());
                        }
                    }
                }
            }
        }
    }

    let Some(bin) = bin_result else {
        return Err(anyhow::Error::new(Error::new(
            std::io::ErrorKind::NotFound,
            format!("could not find any bin file named {}", config.target),
        )));
    };

    Ok(Metadatas {
        bin,
        library: lib_path,
    })
}
