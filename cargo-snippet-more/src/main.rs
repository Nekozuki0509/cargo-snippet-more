mod bundle;
mod snippet;

use std::collections::BTreeMap;
use std::fs::{self, File, read_to_string, write};
use std::io::{self, Read, Write};

use anyhow::{Context, Error, Result};
use clap::{App, AppSettings, Arg, SubCommand, crate_authors, crate_version};
use log::error;
use regex::Regex;
use toml::Value;
use toml::map::Map;

use crate::bundle::config::BundleConfig;
use crate::bundle::data::Data;
use crate::bundle::meta;
use crate::bundle::parser::get_should_bundle;
use crate::snippet::config::SnippetConfig;
use crate::snippet::parser::parse_snippet;
use crate::snippet::snippet::process_snippets;

/// Report error and continue.
fn report_error<T>(result: Result<T, Error>) -> Option<T> {
    match result {
        Ok(x) => Some(x),
        Err(e) => {
            error!("{}", e);
            None
        }
    }
}

fn main() {
    env_logger::init();

    // Setup for cargo subcommand
    let matches = App::new("cargo-snippet")
        .version(crate_version!())
        .bin_name("cargo")
        .settings(&[AppSettings::GlobalVersion, AppSettings::SubcommandRequired])
        .subcommand(
            SubCommand::with_name("snippet")
                .author(crate_authors!())
                .about("Extract code snippet from cargo projects")
                .arg(Arg::with_name("PATH").multiple(true).help(
                    "The files or directories (including children) \
                     to extract snippet (defaults to <project_root>/src when omitted)",
                ))
                .arg(
                    Arg::with_name("output_type")
                        .long("type")
                        .short("t")
                        .default_value("neosnippet")
                        .possible_values(&["neosnippet", "vscode", "ultisnips"]),
                ),
        )
        .subcommand(
            SubCommand::with_name("bundle")
                .author(crate_authors!())
                .about("Bundle library for a specific binary")
                .arg(
                    Arg::with_name("bin")
                        .long("bin")
                        .value_name("BINNAME")
                        .help("The name of the binary to bundle")
                        .required(true)
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("init")
                .author(crate_authors!())
                .about("initialize current directory for bundling"),
        )
        .get_matches();

    match matches.subcommand_name() {
        Some("snippet") => snippet(SnippetConfig::from_matches(&matches)),
        Some("bundle") => report_error(bundle(BundleConfig::from_matches(&matches))).unwrap_or(()),
        Some("init") => report_error(init()).unwrap_or(()),
        _ => {}
    }
}

fn snippet(config: SnippetConfig) {
    // Alphabetical order
    let mut snippets = vec![];

    let mut buf = String::new();
    for path in config.target.iter_paths() {
        let components: Vec<_> = path.components().collect();

        let pos = components
            .iter()
            .position(|c| c.as_os_str() == "src")
            .unwrap_or(components.len());
        let result = if pos + 1 < components.len() {
            Ok(components[pos + 1..]
                .iter()
                .map(|x| {
                    x.as_os_str()
                        .to_string_lossy()
                        .to_string()
                        .replace(".rs", "")
                })
                .collect::<Vec<_>>())
        } else {
            Err(Error::new(io::Error::new(
                io::ErrorKind::NotFound,
                format!("could not find src directory in path: {}", &path.display()),
            )))
        };

        buf.clear();
        log::info!("Start read {:?}", &path);
        if let Some(use_path) = report_error(result)
            && let Some(mut file) = report_error(
                fs::File::open(&path)
                    .with_context(|| format!("Failed to open file: {}", path.display()))
            )
            && report_error(
                file.read_to_string(&mut buf)
                    .with_context(|| format!("Failed to read file: {}", path.display()))
            ).is_some()
            && let Some(parsed) = report_error(parse_snippet(&buf))
        {
            snippets.push((use_path, parsed));
        }
    }

    let (data, snips) = &process_snippets(snippets);
    config.output_type.write(snips);

    report_error(data.write());
}

fn bundle(config: BundleConfig) -> Result<()> {
    let metas = meta::get_data(&config)?;

    let mut data = BTreeMap::new();
    for (name, content) in metas.library {
        if let Some(x) = report_error(Data::read(&content)) {
            data.insert(name, x);
        }
    }

    let mut content = read_to_string(metas.bin.as_str())?;
    let bundle_content = get_should_bundle(&content, &data)?;
    for (name, _) in data {
        // Escape the module name to handle special regex characters
        let escaped_name = regex::escape(&name);
        let re = Regex::new(&format!("use {}.*;", escaped_name))
            .with_context(|| format!("Failed to create regex for module: {}", name))?;
        content = re.replace_all(&content, "/* $0 */").to_string();
    }

    let re = Regex::new("use cargo-snippet-more.*;")
        .context("Failed to create regex for cargo-snippet-more import removal")?;
    content = re.replace_all(&content, "/* $0 */").to_string();

    let re = Regex::new(r#"(cargo_snippet_more::)?expanded!\(".*"\)\"#)
        .context("Failed to create regex for expanded macro removal")?;
    content = re.replace_all(&content, "/* $0 */").to_string();

    if !bundle_content.is_empty() {
        content += "\n\n// The following code was expanded by `cargo-snippet-more`.\n\n";
        content += &bundle_content;
    }

    let _ = fs::create_dir_all("src/cargo-snippet-more");
    let mut file = File::create(&format!("src/cargo-snippet-more/{}.rs", config.target))?;

    write!(file, "{}", content)?;
    file.flush()?;

    Ok(())
}

fn init() -> Result<()> {
    let mut doc: Value = toml::from_str(&read_to_string("Cargo.toml")?)?;
    let newbins;

    {
        let bintable = doc
            .get_mut("package")
            .and_then(|x| x.get_mut("metadata"))
            .and_then(|x| x.get_mut("cargo-compete"))
            .and_then(|x| x.get_mut("bin"))
            .and_then(|x| x.as_table_mut())
            .context("Failed to find package.metadata.cargo-compete.bin table in Cargo.toml")?;

        newbins = bintable
            .iter()
            .filter_map(|(name, table)| {
                let table = table.as_table()?;
                let alias = table.get("alias")?;
                let problem = table.get("problem")?;
                Some((name.to_string(), alias.clone(), problem.clone()))
            })
            .collect::<Vec<_>>();

        for (name, alias, problem) in &newbins {
            let mut entry = Map::new();
            entry.insert("alias".to_string(), alias.clone());
            entry.insert("problem".to_string(), problem.clone());
            bintable.insert(format!("{}-more", name), Value::Table(entry));
        }
    }

    {
        let bins = doc
            .get_mut("bin")
            .context("Failed to find bin section in Cargo.toml")?
            .as_array_mut()
            .context("bin section in Cargo.toml is not an array")?;
        for (name, _, _) in newbins {
            let mut entry = Map::new();
            entry.insert("name".to_string(), Value::String(format!("{}-more", name)));
            entry.insert(
                "path".to_string(),
                Value::String(format!("src/cargo-snippet-more/{}.rs", name)),
            );

            bins.push(Value::Table(entry));
        }
    }

    write(
        "Cargo.toml",
        toml::to_string(&doc).context("Failed to serialize Cargo.toml to TOML format")?,
    )?;

    Ok(())
}
