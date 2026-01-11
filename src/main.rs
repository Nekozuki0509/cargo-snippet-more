mod bundle;
mod snippet;

use std::collections::BTreeMap;
use std::fs::{self, File, read_to_string};
use std::io::{self, Read, Write};

use anyhow::{Context, Error};
use clap::{App, AppSettings, Arg, SubCommand, crate_authors, crate_version};
use log::error;
use regex::Regex;

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
        Some("snippet") => snippet(SnippetConfig::from_matches(&matches)),
        Some("bundle") => bundle(BundleConfig::from_matches(&matches)),
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
            && let Some(mut file) = report_error(fs::File::open(path).context(""))
            && report_error(file.read_to_string(&mut buf).context("")).is_some()
            && let Some(parsed) = report_error(parse_snippet(&buf))
        {
            snippets.push((use_path, parsed));
        }
    }

    let (data, snips) = &process_snippets(snippets);
    config.output_type.write(snips);

    report_error(data.write());
}

fn bundle(config: BundleConfig) {
    let Some(metas) = report_error(meta::get_data(&config)) else {
        return;
    };

    let mut data = BTreeMap::new();
    for (name, content) in metas.library {
        if let Some(x) = report_error(Data::read(&content)) {
            data.insert(name, x);
        }
    }

    let Some(mut content) = report_error(read_to_string(metas.bin.as_str()).context("")) else {
        return;
    };

    let Some(bundle_content) = report_error(get_should_bundle(&content, &data)) else {
        return;
    };

    for (name, _) in data {
        let re = Regex::new(&format!("use {}.*;", &name)).unwrap();
        content = re.replace_all(&content, "/* $0 */").to_string();
    }

    let re = Regex::new("#[cargo_snippet::expanded=\".*\"]").unwrap();
    content = re.replace_all(&content, "/* $0 */").to_string();

    content += "\n\n// The following code was expanded by `cargo-snippet-more`.\n\n";
    content += &bundle_content;

    let _ = fs::create_dir_all("src/cargo-snippet-more");
    let Some(mut file) = report_error(
        File::create(&format!("src/cargo-snippet-more/{}.rs", config.target)).context(""),
    ) else {
        return;
    };

    report_error(write!(file, "{}", content).context(""));
    report_error(file.flush().context(""));
}
