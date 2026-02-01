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
rustup component add rustfmt
```

Install `cargo-snippet-more`:

```bash
cargo install cargo-snippet-more --features="binaries"
```

**Note:** The command is `cargo-snippet-more` (with hyphen), not `cargo snippet-more` (with space).

## Basic Usage

Create a project for snippets:

```bash
cargo new --lib mysnippet
```

Add dependency to `Cargo.toml`:

```toml
[dependencies]
cargo-snippet-more = "0.2"
```

Write snippet code with tests:

```rust
use cargo_snippet_more::snippet;

// Annotate snippet name
#[snippet(name = "mymath", not_library)]
#[snippet("gcd")]
fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

// Also works
#[snippet(name = "mymath", not_library)]
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
cargo test
```

Extract snippets:

```bash
cargo-snippet-more snippet
```

Specify output format (neosnippet, vscode, or ultisnips):

```bash
cargo-snippet-more snippet -t vscode
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

The `not_library` attribute is crucial for understanding how bundling works.

**How Bundling Works:**
Bundle functionality examines `use` statements in your binary to determine which snippets to include. It looks up the path in `libraries.toml` (e.g., `use mylib::UnionFind` looks for a library item named `UnionFind`).

**When to Use `not_library`:**

The `not_library` attribute should be applied to **any snippet where the function or struct name cannot be uniquely determined**. This happens when:

1. **Multiple items in one snippet**: If you create a snippet named "math" that contains both `gcd` and `lcm` functions, you cannot write `use mylib::math` (there's no single item called "math"). This snippet must have `not_library`.

2. **Snippets with multiple names**: When a snippet has multiple different names that don't correspond to actual function/struct names.

```rust
// This should have not_library because "math" isn't a single function/struct
#[snippet(name = "math", not_library)]
fn gcd(a: u64, b: u64) -> u64 { /* ... */ }

#[snippet(name = "math", not_library)]
fn lcm(a: u64, b: u64) -> u64 { /* ... */ }

// This can be a library because UnionFind is the actual struct name
#[snippet(name = "UnionFind")]
struct UnionFind { /* ... */ }
```

**Range Snippets and Library:**

Range snippets (using `snippet_start!`/`snippet_end!`) have library mode **OFF by default**. To make them available for bundling, use the `library` parameter with a name that **exactly matches** the function or struct name that will appear in `use` statements:

```rust
// Library mode OFF by default - cannot be bundled
snippet_start!(name = "algorithms");
fn helper1() {}
fn helper2() {}
snippet_end!("algorithms");

// Library mode ON - can be bundled with "use mylib::UnionFind"
snippet_start!(name = "union_find", library = "UnionFind");
struct UnionFind { /* ... */ }
impl UnionFind { /* ... */ }
snippet_end!("union_find");
```

**Important:** The `library` parameter must exactly match what appears in `use` statements. If you write `use mylib::UnionFind`, the library name must be `"UnionFind"`.

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

To include range snippets in library bundles, use the `library` parameter. **Important**: The `library` parameter is only appropriate when the range snippet contains a **single struct or function** that will be imported. The library name must **exactly match** the name used in `use` statements.

```rust
// Correct: Single struct with library parameter matching the struct name
snippet_start!(name = "union_find", library = "UnionFind");

struct UnionFind {
    parent: Vec<usize>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind { parent: (0..n).collect() }
    }
}

snippet_end!("union_find");

// This can be used with: use my_library::UnionFind;
```

**Key Points:**

- Use `library` parameter **only** when the snippet defines a single struct or function
- The `library` name must exactly match what appears in `use` statements (e.g., `library = "UnionFind"` for `use lib::UnionFind`)
- Without the `library` parameter, range snippets are automatically marked as `not_library` and won't be included in bundles

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

Add dependencies and metadata to your `Cargo.toml`:

```toml
[package]
name = "my-contest-project"
version = "0.1.0"
edition = "2021"

[dependencies]
cargo-snippet-more = "0.2"
# Add your custom library (adjust path as needed)
my-library = { path = "../my-library" }

# Define your binaries
[[bin]]
name = "a"
path = "src/bin/a.rs"

[[bin]]
name = "b"
path = "src/bin/b.rs"

# Metadata for cargo-snippet-more to find libraries.toml
[[package.metadata.cargo-snippet-more.library-path]]
libs = "../my-library/libraries.toml"

# Note: cargo-compete metadata will be automatically added by `cargo-snippet-more init`
```

**Important Configuration Details:**

- `[[package.metadata.cargo-snippet-more.library-path]]`: This tells cargo-snippet-more where to find the `libraries.toml` file when bundling. The path is relative to the directory containing `Cargo.toml`.
- You need to add your custom library to dependencies so it can be imported in your binary files.
- The `cargo-compete` metadata will be automatically created when you run `cargo-snippet-more init` - you don't need to add it manually.

#### 2. Initialize for Bundling

Run the init command to set up bundle configuration:

```bash
cargo-snippet-more init
```

This command:

- Creates `-more` variants of your binaries in the `cargo-compete` metadata
- Sets up the necessary configuration for bundling
- Automatically integrates with cargo-compete if you're using it

**cargo-compete Integration:**

If you're using [cargo-compete](https://github.com/qryxip/cargo-compete) for competitive programming:

1. After running `cargo-compete new <contest>`, your `Cargo.toml` will have the `cargo-compete` metadata
2. Run `cargo-snippet-more init` to create bundled versions of each binary
3. The init command adds entries like `a-more`, `b-more` etc. to the metadata
4. These bundled versions will be available for submission

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
snippet_start!(name = "math_utils");

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
cargo-snippet-more snippet
```

This creates `libraries.toml` containing all library snippets.

#### 5. Use Libraries in Binary

In your binary (e.g., `src/bin/a.rs`), you can either:

**Option A: Bundle custom library code** (recommended for most cases):

```rust
// Import from your custom library
use my_library::gcd;
use my_library::UnionFind;

fn main() {
    // These will be bundled into the final file
    let result = gcd(48, 18);
    println!("GCD: {}", result);
    
    let uf = UnionFind::new(10);
    // ... use UnionFind
}
```

**Option B: Editing a Dependency While Keeping Dependents Unchanged**

When you have library functions that depend on each other and want to customize one without re-bundling everything:

```rust
// In your binary (e.g., src/bin/problem.rs)
use my_library::gcd_list;

// Expand and edit the gcd function inline
cargo_snippet_more::expanded!("gcd");

// Custom implementation of gcd for this specific problem
fn gcd(a: u64, b: u64) -> u64 {
    // Add custom processing here
    println!("Computing GCD of {} and {}", a, b);
    if b == 0 { a } else { gcd(b, a % b) }
}

fn main() {
    // gcd_list will be bundled and will call YOUR custom gcd above
    let result = gcd_list(&[48, 18, 12]);
    println!("{}", result);
}
```

In this example:

- `gcd` is expanded inline (marked with `expanded!()`) and customized
- `gcd_list` is bundled automatically from your library
- When bundled, `gcd_list` calls your customized `gcd` function, not the library version

**Option C: Replacing Custom Library Dependencies with Platform Libraries**

When your custom library depends on another custom library, but you want to use a platform library instead:

```rust
use superslice::*;
use my_library::get_lower_and_upper_bound;  // This depends on a custom lower, upper bound

cargo_snippet_more::expanded!("lower_bound");
cargo_snippet_more::expanded!("upper_bound");

fn main() {
  let b = [1, 3];

  let (l, w) = get_lower_and_upper_bound(b);
  println!("lower_bound: {}, upper_bound: {}", l, w);
}
```

By marking your custom library component as `expanded!()`, you tell cargo-snippet-more to skip bundling it, allowing the platform library to be used instead.

#### 6. Bundle the Binary

Create the bundled version:

```bash
cargo-snippet-more bundle --bin a
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
cargo-snippet-more init

# 5. Extract snippets to libraries.toml
cargo-snippet-more snippet

# 6. Bundle specific binary
cargo-snippet-more bundle --bin a

# 7. The bundled file is at src/cargo-snippet-more/a.rs
# Submit this file to the contest platform
```

## Advanced Features

### Combining Multiple Features

```rust
use cargo_snippet_more::{snippet, snippet_start, snippet_end, p};

// Range snippet with placeholders (no library parameter - won't be bundled)
snippet_start!(name = "interactive_search", include = "binary_search");

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

### Real-World Examples

**Snippet Library Example:**
**[Nekozuki-library](https://github.com/Nekozuki0509/Nekozuki-library)** - A competitive programming snippet library demonstrating:

- Properly structured library snippets
- Range-based snippets with `library` parameter
- Use of `not_library` attribute where appropriate
- Complete library organization for competitive programming

**cargo-compete Integration Example:**
**[atcoder_rust](https://github.com/Nekozuki0509/atcoder_rust)** - A complete cargo-compete project showing:

- Full Cargo.toml configuration with metadata
- Integration with cargo-snippet-more for bundling
- How to use library snippets in contest binaries
- Complete workflow from contest setup to submission

See also the test suite in this repository for additional examples of all features.

## License

MIT License - Same as the original cargo-snippet project.

## Credits

Forked from [cargo-snippet](https://github.com/hatoo/cargo-snippet) by hatoo.

Additional features:

- Bundle functionality
- Range-based snippets
- Interactive placeholders
- Enhanced attribute system
