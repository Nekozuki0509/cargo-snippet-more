mod config;
mod fsutil;
mod parser;
mod snippet;
mod writer;

use std::io::Read;
use std::{fs, path::PathBuf};

use clap::{App, AppSettings, Arg, SubCommand, crate_authors, crate_version};
use log::error;

use std::error::Error;

use crate::config::{BundleConfig, SnippetConfig};

/// Report error and continue.
fn report_error<T, E: Error>(result: Result<T, E>) -> Option<T> {
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
                .about("Bundle snippets for a specific binary")
                .arg(
                    Arg::with_name("bin")
                        .long("bin")
                        .value_name("BINNAME")
                        .help("The name of the binary to bundle")
                        .required(true)
                        .takes_value(true),
                ),
        )
        .get_matches();

    match matches.subcommand_name() {
        Some("snippet") => snippet(config::SnippetConfig::from_matches(&matches)),
        Some("bundle") => bundle(config::BundleConfig::from_matches(&matches)),
        _ => {}
    }
}

fn snippet(config: SnippetConfig) {
    // Alphabetical order
    let mut snippets = vec![];

    let mut buf = String::new();
    for path in config.target.iter_paths() {
        dbg!(&path);
        let components: Vec<_> = path.components().collect();

        let pos = components.iter().position(|c| c.as_os_str() == "src")?;
        let result = if pos + 1 < components.len() {
            Ok(components[pos + 1..].iter().collect::<PathBuf>())
        } else {
            Err(format!("could not find src directory in path: {}", &path))
        };

        buf.clear();
        log::info!("Start read {:?}", &path);
        if let Some(use_path) = report_error(result)
            && let Some(mut file) = report_error(fs::File::open(path))
            && report_error(file.read_to_string(&mut buf)).is_some()
            && let Some(mut parsed) = report_error(parser::parse_snippet(&buf))
        {
            snippets.push((use_path, parsed));
        }
    }

    config
        .output_type
        .write(&snippet::process_snippets(&snippets));
}

fn bundle(config: BundleConfig) {
    todo!()
}
