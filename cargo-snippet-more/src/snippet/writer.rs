use serde_derive::Serialize;
use std::collections::BTreeMap;
use regex::Regex;
use lazy_static::lazy_static;

/// Convert doc comment placeholders to placeholder syntax
/// Supports: #[doc = "ps!(n)"], #[doc = "pe!(n)"], #[doc = "p!(n)"]
/// Also supports: /// ps!(n), /// pe!(n), /// p!(n), //! ps!(n), etc.
fn convert_doc_comment_placeholders(src: &str) -> String {
    lazy_static! {
        // Match doc attribute placeholder patterns: #[doc = "ps!(...)"]
        static ref DOC_ATTR_PS_RE: Regex = Regex::new(r#"\s*#\s*\[\s*doc\s*=\s*"ps!\((\d+)(?:,\s*\|([^|"]*)\|)?\)"\s*\]"#).unwrap();
        static ref DOC_ATTR_PE_RE: Regex = Regex::new(r#"\s*#\s*\[\s*doc\s*=\s*"pe!\((\d+)\)"\s*\]"#).unwrap();
        static ref DOC_ATTR_P_RE: Regex = Regex::new(r#"\s*#\s*\[\s*doc\s*=\s*"p!\((\d+)\)"\s*\]"#).unwrap();
        
        // Match line doc comment placeholder patterns: /// ps!(n) or //! ps!(n)
        static ref DOC_PS_RE: Regex = Regex::new(r"(?m)^[ \t]*(///|//!)[ \t]*ps!\((\d+)(?:,\s*\|([^|]*)\|)?\)[ \t]*$").unwrap();
        static ref DOC_PE_RE: Regex = Regex::new(r"(?m)^[ \t]*(///|//!)[ \t]*pe!\((\d+)\)[ \t]*$").unwrap();
        static ref DOC_P_RE: Regex = Regex::new(r"(?m)^[ \t]*(///|//!)[ \t]*p!\((\d+)\)[ \t]*$").unwrap();
    }
    
    let mut result = src.to_string();
    
    // First, handle #[doc = "..."] format ps!/pe! pairs
    let ps_attr_matches: Vec<_> = DOC_ATTR_PS_RE.captures_iter(src).map(|cap| {
        (cap.get(0).unwrap().start(), cap.get(0).unwrap().end(), 
         cap.get(1).unwrap().as_str().to_string(),
         cap.get(2).map(|c| c.as_str().to_string()))
    }).collect();
    
    let pe_attr_matches: Vec<_> = DOC_ATTR_PE_RE.captures_iter(src).map(|cap| {
        (cap.get(0).unwrap().start(), cap.get(0).unwrap().end(),
         cap.get(1).unwrap().as_str().to_string())
    }).collect();
    
    // Match ps!/pe! pairs and collect replacements
    let mut replacements = Vec::new();
    
    for (ps_start, ps_end, ps_num, choices) in ps_attr_matches.iter() {
        // Find matching pe! marker
        if let Some((pe_start, pe_end, _)) = pe_attr_matches.iter()
            .find(|(start, _, pe_num)| pe_num == ps_num && start > ps_end) {
            
            // Extract content between markers
            let inner = &src[*ps_end..*pe_start];
            
            // Create placeholder
            let placeholder = if let Some(choice_str) = choices {
                let choices_formatted = choice_str
                    .split(',')
                    .map(|s| s.trim())
                    .collect::<Vec<_>>()
                    .join(",");
                format!("${{{}|{}|}}", ps_num, choices_formatted)
            } else {
                format!("${{{}:{}}}", ps_num, inner.trim())
            };
            
            // Store replacement: (full range including inner content, placeholder)
            replacements.push((*ps_start, *pe_end, placeholder));
        }
    }
    
    // Apply replacements in reverse order to preserve positions
    for (start, end, placeholder) in replacements.iter().rev() {
        result.replace_range(*start..*end, &placeholder);
    }
    
    // Handle standalone #[doc = "p!(...)"] markers
    let p_attr_matches: Vec<_> = DOC_ATTR_P_RE.captures_iter(&result).map(|cap| {
        (cap.get(0).unwrap().start(), cap.get(0).unwrap().end(),
         cap.get(1).unwrap().as_str().to_string())
    }).collect();
    
    for (start, end, num) in p_attr_matches.iter().rev() {
        let placeholder = if num == "0" {
            "$0".to_string()
        } else {
            format!("${{{}}}", num)
        };
        result.replace_range(*start..*end, &placeholder);
    }
    
    // Now handle line doc comment format (/// or //!)
    let ps_matches: Vec<_> = DOC_PS_RE.captures_iter(&result).map(|cap| {
        (cap.get(0).unwrap().start(), cap.get(0).unwrap().end(), 
         cap.get(2).unwrap().as_str().to_string(),
         cap.get(3).map(|c| c.as_str().to_string()))
    }).collect();
    
    let pe_matches: Vec<_> = DOC_PE_RE.captures_iter(&result).map(|cap| {
        (cap.get(0).unwrap().start(), cap.get(0).unwrap().end(),
         cap.get(2).unwrap().as_str().to_string())
    }).collect();
    
    // Match ps!/pe! pairs for line doc comments
    let mut line_replacements = Vec::new();
    
    for (ps_start, ps_end, ps_num, choices) in ps_matches.iter() {
        if let Some((pe_start, pe_end, _)) = pe_matches.iter()
            .find(|(start, _, pe_num)| pe_num == ps_num && start > ps_end) {
            
            let inner = &result[*ps_end..*pe_start];
            
            let placeholder = if let Some(choice_str) = choices {
                let choices_formatted = choice_str
                    .split(',')
                    .map(|s| s.trim())
                    .collect::<Vec<_>>()
                    .join(",");
                format!("${{{}|{}|}}", ps_num, choices_formatted)
            } else {
                format!("${{{}:{}}}", ps_num, inner.trim())
            };
            
            line_replacements.push((*ps_start, *pe_end, placeholder));
        }
    }
    
    for (start, end, placeholder) in line_replacements.iter().rev() {
        result.replace_range(*start..*end, &placeholder);
    }
    
    // Handle standalone /// p!(...) or //! p!(...) markers
    let p_matches: Vec<_> = DOC_P_RE.captures_iter(&result).map(|cap| {
        (cap.get(0).unwrap().start(), cap.get(0).unwrap().end(),
         cap.get(2).unwrap().as_str().to_string())
    }).collect();
    
    for (start, end, num) in p_matches.iter().rev() {
        let placeholder = if num == "0" {
            "$0".to_string()
        } else {
            format!("${{{}}}", num)
        };
        result.replace_range(*start..*end, &placeholder);
    }
    
    result
}

/// Extract placeholders from source, replace with markers, return both
fn extract_placeholders_for_formatting(src: &str) -> (String, Vec<(String, String)>) {
    lazy_static! {
        // Match all placeholder patterns: $0, ${n}, ${n:...}, ${n|...|} 
        static ref PLACEHOLDER_RE: Regex = Regex::new(r"\$(?:0|\{\d+(?::[^}]*|\|[^}]*\|)?\})").unwrap();
    }
    
    let mut placeholder_map = Vec::new();
    let mut result = src.to_string();
    let mut counter = 0;
    
    // Replace each placeholder with a unique marker
    for mat in PLACEHOLDER_RE.find_iter(src) {
        let placeholder = mat.as_str();
        let marker = format!("__PLACEHOLDER_{}__", counter);
        placeholder_map.push((marker.clone(), placeholder.to_string()));
        counter += 1;
    }
    
    // Apply replacements in reverse order to preserve positions
    for (marker, placeholder) in placeholder_map.iter().rev() {
        result = result.replace(placeholder, marker);
    }
    
    // Reverse the map so we can restore in forward order
    placeholder_map.reverse();
    
    (result, placeholder_map)
}

/// Restore placeholders after formatting
fn restore_placeholders_after_formatting(formatted: &str, placeholder_map: &[(String, String)]) -> String {
    let mut result = formatted.to_string();
    
    // Replace markers back with placeholders
    for (marker, placeholder) in placeholder_map {
        result = result.replace(marker, placeholder);
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
    // First, convert doc comment placeholders to placeholder syntax
    let src_with_placeholders = convert_doc_comment_placeholders(src);
    
    // Extract placeholders before formatting, format, then restore them
    let (src_without_placeholders, placeholder_map) = extract_placeholders_for_formatting(&src_with_placeholders);
    
    let src = format!("fn ___dummy___() {{{}}}", src_without_placeholders);
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
            let formatted = lines.collect::<Vec<_>>().join("\n");
            restore_placeholders_after_formatting(&formatted, &placeholder_map)
        })
    } else {
        None
    }
}

#[cfg(not(feature = "inner_rustfmt"))]
pub fn format_src(src: &str) -> Option<String> {
    // First, convert doc comment placeholders to placeholder syntax
    let src_with_placeholders = convert_doc_comment_placeholders(src);
    
    // Extract placeholders before formatting, format, then restore them
    let (src_without_placeholders, placeholder_map) = extract_placeholders_for_formatting(&src_with_placeholders);
    
    let src = format!("fn ___dummy___() {{{}}}", src_without_placeholders);

    use std::io::Write;
    use std::process;

    let mut command = process::Command::new("rustfmt")
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
    Some(restore_placeholders_after_formatting(&formatted, &placeholder_map))
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
            println!("snippet {}", name);
            for line in formatted.lines() {
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
                (
                    name.to_owned(),
                    VScode {
                        prefix: name.to_owned(),
                        body: formatted
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
            println!("snippet {}", name);
            print!("{}", formatted);
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
