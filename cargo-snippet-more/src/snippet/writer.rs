use serde_derive::Serialize;
use std::collections::BTreeMap;
use regex::Regex;
use lazy_static::lazy_static;

/// Convert p! macros to placeholder syntax
/// p!(0) → $0
/// p!(n) → ${n}
/// p!(n, content) → ${n:content}
/// p!(n, |a, b, c|) → ${n|a,b,c|}
fn convert_placeholders(src: &str) -> String {
    lazy_static! {
        // Match p!(...)  patterns
        // Captures: p ! ( number [, rest] )
        static ref P_MACRO_RE: Regex = Regex::new(r"p\s*!\s*\(\s*(\d+)\s*(?:,\s*([^)]+))?\s*\)").unwrap();
    }
    
    let mut result = src.to_string();
    
    // Process all p! macros
    loop {
        let before = result.clone();
        result = P_MACRO_RE.replace_all(&result, |caps: &regex::Captures| {
            let num = &caps[1];
            
            // Check if there's content after the number
            if let Some(content) = caps.get(2) {
                let content = content.as_str().trim();
                
                // Check if it's a choice pattern: | ... |
                if content.starts_with('|') && content.ends_with('|') {
                    // Extract choices between pipes
                    let choices_str = &content[1..content.len()-1];
                    // Split by comma and trim each choice
                    let choices: Vec<&str> = choices_str.split(',')
                        .map(|s| s.trim())
                        .filter(|s| !s.is_empty())
                        .collect();
                    
                    if !choices.is_empty() {
                        return format!("${{{}|{}|}}", num, choices.join(","));
                    }
                }
                
                // Otherwise it's p!(n, content) → ${n:content}
                return format!("${{{}:{}}}", num, content);
            }
            
            // p!(0) → $0 or p!(n) → ${n}
            if num == "0" {
                "$0".to_string()
            } else {
                format!("${{{}}}", num)
            }
        }).to_string();
        
        // If nothing changed, we're done
        if result == before {
            break;
        }
    }
    
    result
}

#[derive(Serialize)]
struct VScode {
    prefix: String,
    body: Vec<String>,
}

#[cfg(feature = "inner_rustfmt")]
pub fn format_src(src: &str) -> Option<String> {
    // No need to extract/restore placeholders anymore - p! macros are valid Rust syntax
    let src = format!("fn ___dummy___() {{{}}}", src);
    let mut rustfmt_config = rustfmt_nightly::Config::default();
    rustfmt_config
        .set()
        .emit_mode(rustfmt_nightly::EmitMode::Stdout);
    rustfmt_config
        .set()
        .verbose(rustfmt_nightly::Verbosity::Quiet);

    let mut out = Vec::with_capacity(src.len() * 2);
    let input = rustfmt_nightly::Input::Text(src.into());

    if rustfmt_nightly::Session::new(rustfmt_config, Some(&mut out))
        .format(input)
        .is_ok()
    {
        String::from_utf8(out).ok().map(|s| {
            let mut lines = s
                .replace("\r\n", "\n")
                .replace("#[rustfmt::skip]", "")
                .lines();

            lines.next();
            lines.next_back();
            lines.collect::<Vec<_>>().join("\n")
        })
    } else {
        None
    }
}

#[cfg(not(feature = "inner_rustfmt"))]
pub fn format_src(src: &str) -> Option<String> {
    // No need to extract/restore placeholders anymore - p! macros are valid Rust syntax
    let src = format!("fn ___dummy___() {{{}}}", src);

    use std::io::Write;
    use std::process;

    let command = process::Command::new("rustfmt")
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .spawn();
    
    let mut command = match command {
        Ok(cmd) => cmd,
        Err(e) => {
            log::error!("Failed to spawn rustfmt process: {}", e);
            return None;
        }
    };
    {
        let mut stdin = command.stdin.take()?;
        if let Err(e) = write!(stdin, "{}", src) {
            log::error!("Failed to write to rustfmt stdin: {}", e);
            return None;
        }
    }
    let out = command.wait_with_output().ok()?;

    if !out.status.success() {
        log::error!("rustfmt returns non-zero status");
        log::error!("[stdout]\n{}", String::from_utf8_lossy(&out.stdout));
        log::error!("[stderr]\n{}", String::from_utf8_lossy(&out.stderr));
        return None;
    }

    let stdout = out.stdout;
    let out = String::from_utf8(stdout).ok()?;
    let replaced = out.replace("\r\n", "\n").replace("#[rustfmt::skip]", "");
    let mut lines = replaced.lines();

    lines.next();
    lines.next_back();

    let formatted = lines.collect::<Vec<_>>().join("\n");
    Some(formatted)
}

// Escape $ characters that are NOT part of placeholder syntax
fn escape_non_placeholder_dollars(line: &str) -> String {
    use regex::Regex;
    lazy_static::lazy_static! {
        // Match placeholder patterns: $0, ${n}, ${n:...}, ${n|...|} 
        static ref PLACEHOLDER_RE: Regex = Regex::new(r"\$(?:0|\{\d+(?::[^}]*|\|[^}]*\|)?\})").unwrap();
    }
    
    let mut result = String::new();
    let mut last_end = 0;
    
    // Find all placeholders
    for mat in PLACEHOLDER_RE.find_iter(line) {
        // Escape dollars in the text before this placeholder
        let before = &line[last_end..mat.start()];
        result.push_str(&before.replace("$", "\\$"));
        
        // Add the placeholder as-is (don't escape)
        result.push_str(mat.as_str());
        
        last_end = mat.end();
    }
    
    // Escape dollars in the remaining text
    let after = &line[last_end..];
    result.push_str(&after.replace("$", "\\$"));
    
    result
}

pub fn write_neosnippet(snippets: &BTreeMap<String, String>) {
    for (name, content) in snippets.iter() {
        if let Some(formatted) = format_src(content) {
            // Convert p! macros to placeholders just before output
            let with_placeholders = convert_placeholders(&formatted);
            
            println!("snippet {}", name);
            for line in with_placeholders.lines() {
                // Neosnippet uses the same placeholder syntax as VSCode
                // No need to escape $ characters in placeholders
                println!("    {}", line);
            }
            println!();
        }
    }
}

pub fn write_vscode(snippets: &BTreeMap<String, String>) {
    let vscode: BTreeMap<String, VScode> = snippets
        .iter()
        .filter_map(|(name, content)| {
            format_src(content).map(|formatted| {
                // Convert p! macros to placeholders just before output
                let with_placeholders = convert_placeholders(&formatted);
                
                (
                    name.to_owned(),
                    VScode {
                        prefix: name.to_owned(),
                        body: with_placeholders
                            .lines()
                            .map(|l| escape_non_placeholder_dollars(l))
                            .collect(),
                    },
                )
            })
        })
        .collect();

    if let Ok(json) = serde_json::to_string_pretty(&vscode) {
        println!("{}", json);
    }
}

pub fn write_ultisnips(snippets: &BTreeMap<String, String>) {
    for (name, content) in snippets.iter() {
        if let Some(formatted) = format_src(content) {
            // Convert p! macros to placeholders just before output
            let with_placeholders = convert_placeholders(&formatted);
            
            println!("snippet {}", name);
            // Ultisnips uses ${n:default}, ${n|a,b|}, $0 syntax - same as our placeholders
            // No escaping needed
            print!("{}", with_placeholders);
            println!("endsnippet");
            println!();
        }
    }
}

#[test]
fn test_format_src() {
    assert_eq!(format_src("fn foo(){}"), Some("fn foo() {}\n".into()));

    assert_eq!(
        format_src("/// doc comment\n pub fn foo(){}"),
        Some("/// doc comment\npub fn foo() {}\n".into())
    );
}
