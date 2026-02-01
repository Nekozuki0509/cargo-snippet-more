# cargo-snippet-more

[![crates.io](https://img.shields.io/crates/v/cargo-snippet-more.svg)](https://crates.io/crates/cargo-snippet-more)

A powerful snippet extractor for competitive programmers, forked from [cargo-snippet](https://github.com/hatoo/cargo-snippet) with enhanced features.

**New Features in cargo-snippet-more:**
- 🎯 **Bundle functionality** - Bundle library snippets into executable binaries for competitive programming platforms
- 📦 **Range-based snippets** with `snippet_start!`/`snippet_end!` macros
- ✨ **Interactive placeholders** with the `p!()` macro for VSCode, Ultisnips, and Neosnippet
- 🔧 **Fine-grained control** with `not_library` attribute to separate snippet types

## Table of Contents

- [Installation](#installation)
- [Basic Usage](#basic-usage)
- [Snippet Attributes](#snippet-attributes)
- [Range-Based Snippets](#range-based-snippets)
- [Interactive Placeholders](#interactive-placeholders)
- [Bundle Functionality](#bundle-functionality)
- [Advanced Features](#advanced-features)

## Installation

You need to install `rustfmt` to run `cargo-snippet-more`.

```bash
$ rustup component add rustfmt
```

Install `cargo-snippet-more`:

```bash
$ cargo install cargo-snippet-more --features="binaries"
```

## Basic Usage

Create a project for snippets:

```bash
$ cargo new --lib mysnippet
```

Add dependency to `Cargo.toml`:

```toml
[dependencies]
cargo-snippet-more = "0.1"
```

Write snippet code with tests:

```rust
use cargo_snippet_more::snippet;

// Annotate snippet name
#[snippet("mymath")]
#[snippet("gcd")]
fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

// Also works
#[snippet(name = "mymath")]
// Equivalent to #[snippet("lcm")]
#[snippet]
fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
}

#[test]
fn test_gcd() {
    assert_eq!(gcd(57, 3), 3);
}
```

Test your code:

```bash
$ cargo test
```

Extract snippets:

```bash
$ cargo snippet
```

Specify output format (neosnippet, vscode, or ultisnips):

```bash
$ cargo snippet -t vscode
```

## Snippet Attributes

### Basic Attributes

```rust
// Multiple names for the same snippet
#[snippet("name1")]
#[snippet("name2")]
fn my_function() {}

// Named parameter style
#[snippet(name = "my_snippet")]
fn my_function() {}

// Include dependencies
#[snippet(include = "gcd")]
fn gcd_list(list: &[u64]) -> u64 {
    list.iter().fold(list[0], |a, &b| gcd(a, b))
}

// Add prefix code (useful for imports)
#[snippet(prefix = "use std::io::{self,Read};")]
#[snippet(prefix = "use std::str::FromStr;")]
fn foo() {}

// Hide doc comments in output
#[snippet(doc_hidden)]
/// This doc comment won't appear in the snippet
fn documented() {}
```

### The `not_library` Attribute

The `not_library` attribute prevents a snippet from being included in the library bundle. This is useful when you have:
- Snippets with multiple names that should only appear as standalone snippets
- Range-based snippets that shouldn't be mixed with regular library snippets
- Testing snippets that don't need to be bundled

Without `not_library`, snippets can be accidentally included in library bundles when using different names, causing duplication or unwanted mixing.

```rust
// This snippet will NOT be included in library bundles
#[snippet(name = "test_snippet", not_library)]
fn test_function() {}

// This snippet WILL be included in library bundles
#[snippet(name = "lib_snippet")]
fn library_function() {}
```

## Range-Based Snippets

Use `snippet_start!` and `snippet_end!` macros to define snippet ranges, allowing multiple functions or items in a single snippet without attribute annotations on each item.

### Basic Range Snippet

```rust
use cargo_snippet_more::{snippet_start, snippet_end};

snippet_start!("algorithms");

fn binary_search(arr: &[i32], target: i32) -> Option<usize> {
    // implementation
}

fn quick_sort(arr: &mut [i32]) {
    // implementation
}

snippet_end!("algorithms");
```

### Range Snippet with Parameters

```rust
// With name parameter
snippet_start!(name = "data_structures");

struct UnionFind {
    parent: Vec<usize>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind {
            parent: (0..n).collect()
        }
    }
}

snippet_end!("data_structures");

// With dependencies
snippet_start!(name = "advanced_algo", include = "gcd");

fn extended_gcd(a: i64, b: i64) -> (i64, i64, i64) {
    // uses gcd internally
}

snippet_end!("advanced_algo");

// With doc_hidden
snippet_start!(name = "hidden_docs", doc_hidden);

/// This documentation will not appear in the snippet
fn documented_function() {}

snippet_end!("hidden_docs");
```

### Setting Library Name for Range Snippets

To include range snippets in library bundles, use the `library` parameter:

```rust
snippet_start!(name = "my_snippet", library = "my_library");

fn helper_function() {}
fn main_function() {}

snippet_end!("my_snippet");
```

This creates a snippet named "my_snippet" that will be bundled under the library name "my_library".

Without the `library` parameter, the snippet is automatically marked as `not_library` and won't be included in bundles.

## Interactive Placeholders

The `p!()` macro creates interactive placeholders that are converted to editor-specific syntax. Placeholders work with any valid Rust code and are syntactically valid, so they work with rustfmt and rust-analyzer.

### Placeholder Types

```rust
use cargo_snippet_more::p;

// Final cursor position
p!(0);  // → $0

// Simple placeholder
p!(1);  // → ${1}

// Placeholder with default value
p!(1, variable);  // → ${1:variable}

// Choice placeholder (dropdown in editor)
p!(1, |"option1", "option2", "option3"|);  // → ${1|option1,option2,option3|}
```

### Real-World Example

```rust
use cargo_snippet_more::{snippet, p};

#[snippet("binary_search")]
fn binary_search<T: Ord>(arr: &[T], target: &T) -> Option<usize> {
    let p!(1, mut low) = 0;
    let p!(2, mut high) = arr.len();
    
    while low < high {
        let p!(3, mid) = low + (high - low) / 2;
        match arr[mid].cmp(target) {
            std::cmp::Ordering::Less => low = mid + 1,
            std::cmp::Ordering::Equal => return Some(mid),
            std::cmp::Ordering::Greater => high = mid,
        }
    }
    
    p!(0);
    None
}
```

When this snippet is inserted in VSCode:
- `${1:mut low}` is the first placeholder with default "mut low"
- `${2:mut high}` is the second placeholder
- `${3:mid}` is the third placeholder
- `$0` is the final cursor position

### Placeholders in Different Contexts

```rust
// In struct initialization
let point = Point { 
    x: p!(1, 0), 
    y: p!(2, 0) 
};

// In match expressions
match p!(1, value) {
    Some(p!(2, x)) => p!(3, x),
    None => p!(4, 0),
}

// Choice placeholders for modes/options
let mode = p!(1, |"read", "write", "append"|);
```

## Bundle Functionality

Bundle functionality allows you to combine library snippets into executable binaries for competitive programming platforms that require single-file submissions.

### Setup for Bundling

#### 1. Configure Cargo.toml

Add metadata for cargo-compete integration:

```toml
[package]
name = "my-contest-project"
version = "0.1.0"
edition = "2021"

[dependencies]
cargo-snippet-more = "0.1"

# Define your binaries
[[bin]]
name = "a"
path = "src/bin/a.rs"

[[bin]]
name = "b"
path = "src/bin/b.rs"

# Metadata for bundling (compatible with cargo-compete)
[package.metadata.cargo-compete.bin]
a = { alias = "a", problem = "https://..." }
b = { alias = "b", problem = "https://..." }
```

#### 2. Initialize for Bundling

Run the init command to set up bundle configuration:

```bash
$ cargo snippet-more init
```

This creates `-more` variants of your binaries in the metadata, which will contain the bundled code.

#### 3. Create Library Snippets

In `src/lib.rs` or library modules:

```rust
use cargo_snippet_more::snippet;

// This will be available for bundling
#[snippet("gcd")]
fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 { a } else { gcd(b, a % b) }
}

#[snippet("lcm")]
#[snippet(include = "gcd")]
fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
}

// Range snippet for library
snippet_start!(name = "math_utils", library = "math");

fn mod_pow(base: u64, exp: u64, modulo: u64) -> u64 {
    // implementation
}

fn mod_inv(a: u64, modulo: u64) -> u64 {
    // implementation
}

snippet_end!("math_utils");
```

#### 4. Extract Snippets

Generate the library metadata:

```bash
$ cargo snippet
```

This creates `libraries.toml` containing all library snippets.

#### 5. Use Libraries in Binary

In your binary (e.g., `src/bin/a.rs`):

```rust
// Import library modules
use my_contest_project::gcd;
use my_contest_project::math::mod_pow;

// Mark which snippets are expanded (included)
cargo_snippet_more::expanded!("gcd");
cargo_snippet_more::expanded!("math_utils");

fn main() {
    let result = gcd(48, 18);
    println!("{}", result);
}
```

#### 6. Bundle the Binary

Create the bundled version:

```bash
$ cargo snippet-more bundle --bin a
```

This generates `src/cargo-snippet-more/a.rs` with all required library code inlined. The `use` statements are commented out and the library code is appended.

### Bundle Workflow Summary

```bash
# 1. Initialize project
cargo new --lib my-contest
cd my-contest

# 2. Write library snippets in src/lib.rs
# 3. Write binary in src/bin/a.rs with use statements
# 4. Initialize bundling
cargo snippet-more init

# 5. Extract snippets to libraries.toml
cargo snippet

# 6. Bundle specific binary
cargo snippet-more bundle --bin a

# 7. The bundled file is at src/cargo-snippet-more/a.rs
# Submit this file to the contest platform
```

## Advanced Features

### Combining Multiple Features

```rust
use cargo_snippet_more::{snippet, snippet_start, snippet_end, p};

// Range snippet with placeholders for library
snippet_start!(name = "interactive_search", library = "algorithms", include = "binary_search");

fn find_element<T: Ord>(arr: &[T]) -> Option<T> {
    let target = p!(1, &arr[0]);
    let p!(2, result) = binary_search(arr, target);
    p!(0);
    result
}

snippet_end!("interactive_search");

// Regular snippet that won't be in library
#[snippet(name = "test_helper", not_library)]
fn create_test_data() -> Vec<i32> {
    vec![p!(1, 1), p!(2, 2), p!(3, 3)]
}
```

### Output Format Examples

#### Neosnippet (default)
```
snippet binary_search
    fn binary_search<T: Ord>(arr: &[T], target: &T) -> Option<usize> {
        let ${1:mut low} = 0;
        let ${2:mut high} = arr.len();
        $0
    }
```

#### VSCode
```json
{
  "binary_search": {
    "prefix": "binary_search",
    "body": [
      "fn binary_search<T: Ord>(arr: &[T], target: &T) -> Option<usize> {",
      "    let ${1:mut low} = 0;",
      "    let ${2:mut high} = arr.len();",
      "    $0",
      "}"
    ]
  }
}
```

#### Ultisnips
```
snippet binary_search
fn binary_search<T: Ord>(arr: &[T], target: &T) -> Option<usize> {
    let ${1:mut low} = 0;
    let ${2:mut high} = arr.len();
    $0
}
endsnippet
```

## Migration from cargo-snippet

If you're migrating from cargo-snippet, all existing snippets continue to work. Simply:

1. Change dependency from `cargo-snippet` to `cargo-snippet-more`
2. Update import: `use cargo_snippet_more::snippet;`
3. Optionally adopt new features like placeholders and bundling

## Examples

See the test suite in the repository for comprehensive examples of all features.

## License

MIT License - Same as the original cargo-snippet project.

## Credits

Forked from [cargo-snippet](https://github.com/hatoo/cargo-snippet) by hatoo.

Additional features:
- Bundle functionality
- Range-based snippets
- Interactive placeholders
- Enhanced attribute system
