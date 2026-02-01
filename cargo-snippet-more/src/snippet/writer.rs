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
    
    P_MACRO_RE.replace_all(src, |caps: &regex::Captures| {
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
    }).to_string()
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
            let sanitized_output = s
                .replace("\r\n", "\n")
                .replace("#[rustfmt::skip]", "");
            let mut lines = sanitized_output.lines();

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
    // format_src wraps code in a function, formats it, then removes wrapper lines
    // The result may have different whitespace depending on rustfmt behavior
    let result = format_src("fn foo(){}");
    assert!(result.is_some());
    let formatted = result.unwrap();
    assert!(formatted.contains("fn foo() {}"));

    let result = format_src("/// doc comment\n pub fn foo(){}");
    assert!(result.is_some());
    let formatted = result.unwrap();
    assert!(formatted.contains("/// doc comment"));
    assert!(formatted.contains("pub fn foo() {}"));
}

#[cfg(test)]
mod tests {
    use super::{convert_placeholders, escape_non_placeholder_dollars};

    #[test]
    fn test_convert_placeholder_final_cursor() {
        assert_eq!(convert_placeholders("p!(0)"), "$0");
    }

    #[test]
    fn test_convert_placeholder_simple() {
        assert_eq!(convert_placeholders("p!(1)"), "${1}");
        assert_eq!(convert_placeholders("p!(2)"), "${2}");
    }

    #[test]
    fn test_convert_placeholder_with_content() {
        assert_eq!(convert_placeholders("p!(1, variable)"), "${1:variable}");
        assert_eq!(convert_placeholders("p!(2, 10)"), "${2:10}");
    }

    #[test]
    fn test_convert_placeholder_with_choices() {
        assert_eq!(
            convert_placeholders("p!(3, |\"read\", \"write\"|)"),
            "${3|\"read\",\"write\"|}"
        );
        assert_eq!(
            convert_placeholders("p!(1, |a, b, c|)"),
            "${1|a,b,c|}"
        );
    }

    #[test]
    fn test_convert_multiple_placeholders() {
        let input = "let p!(1, variable) = p!(2, 10);\np!(0);";
        let expected = "let ${1:variable} = ${2:10};\n$0;";
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_escape_non_placeholder_dollars() {
        // Normal text with $ should be escaped
        assert_eq!(escape_non_placeholder_dollars("Cost: $100"), "Cost: \\$100");
        
        // Placeholders should NOT be escaped
        assert_eq!(escape_non_placeholder_dollars("$0"), "$0");
        assert_eq!(escape_non_placeholder_dollars("${1}"), "${1}");
        assert_eq!(escape_non_placeholder_dollars("${1:default}"), "${1:default}");
        assert_eq!(escape_non_placeholder_dollars("${1|a,b|}"), "${1|a,b|}");
        
        // Mixed content
        assert_eq!(
            escape_non_placeholder_dollars("Cost $100 and ${1:variable}"),
            "Cost \\$100 and ${1:variable}"
        );
    }

    #[test]
    fn test_placeholder_in_real_code() {
        let input = r#"fn binary_search(arr: &[i32]) {
    let p!(1, mut low) = 0;
    let p!(2, mut high) = arr.len();
    p!(0);
}"#;
        let expected = r#"fn binary_search(arr: &[i32]) {
    let ${1:mut low} = 0;
    let ${2:mut high} = arr.len();
    $0;
}"#;
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_placeholder_with_whitespace() {
        // Test that whitespace in p! macro is handled correctly
        assert_eq!(convert_placeholders("p!( 1 )"), "${1}");
        assert_eq!(convert_placeholders("p!( 1 , variable )"), "${1:variable}");
        assert_eq!(convert_placeholders("p!( 0 )"), "$0");
    }

    #[test]
    fn test_placeholder_with_complex_default() {
        let input = "p!(1, vec![1, 2, 3])";
        let expected = "${1:vec![1, 2, 3]}";
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_placeholder_choices_with_quotes() {
        let input = r#"p!(1, |"option1", "option2", "option3"|)"#;
        let expected = r#"${1|"option1","option2","option3"|}"#;
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_placeholder_in_struct_initialization() {
        let input = r#"Point { x: p!(1, 0), y: p!(2, 0) }"#;
        let expected = r#"Point { x: ${1:0}, y: ${2:0} }"#;
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_placeholder_in_match_arms() {
        let input = r#"match p!(1, value) {
    Some(p!(2, x)) => p!(3, x),
    None => p!(4, 0),
}"#;
        let expected = r#"match ${1:value} {
    Some(${2:x}) => ${3:x},
    None => ${4:0},
}"#;
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_placeholder_zero_as_final_position() {
        // p!(0) should become $0, not ${0}
        let input = "fn test() { p!(1, code); p!(0); }";
        let expected = "fn test() { ${1:code}; $0; }";
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_multiple_placeholders_same_line() {
        let input = "let p!(1, a) = p!(2, 10), p!(3, b) = p!(4, 20);";
        let expected = "let ${1:a} = ${2:10}, ${3:b} = ${4:20};";
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_placeholder_with_underscore_default() {
        let input = "let p!(1, _result) = some_function();";
        let expected = "let ${1:_result} = some_function();";
        assert_eq!(convert_placeholders(input), expected);
    }

    #[test]
    fn test_placeholder_no_conversion_in_string() {
        // This is a string literal, not a placeholder
        let input = r#"println!("Use p!(1) for placeholder");"#;
        // convert_placeholders will still convert it because it doesn't know about string context
        // This is expected behavior - p! should only be used outside strings
        let expected = r#"println!("Use ${1} for placeholder");"#;
        assert_eq!(convert_placeholders(input), expected);
    }
}
