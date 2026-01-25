use clap::ArgMatches;

#[derive(Debug)]
pub struct BundleConfig<'a> {
    pub target: &'a str,
}

impl<'a> BundleConfig<'a> {
    pub fn from_matches(matches: &'a ArgMatches) -> Self {
        Self {
            target: matches
                .subcommand_matches("bundle")
                .and_then(|m| m.value_of("bin"))
                .unwrap(),
        }
    }
}
