pub use cargo_snippet_more_macros::*;

#[macro_export]
macro_rules! snippet_start {
    ($($tt:tt)*) => {};
}

#[macro_export]
macro_rules! snippet_end {
    ($name:literal) => {};
}

#[macro_export]
macro_rules! expanded {
    ($name:literal) => {};
}

/// Placeholder macro for defining interactive placeholders in snippets.
///
/// This macro creates placeholders that will be converted to editor-specific
/// placeholder syntax in the generated snippets.
///
/// # Variants
///
/// - `p!(0)` - Final cursor position (converts to `$0`)
/// - `p!(n)` - Placeholder without default value (converts to `${n}`)
/// - `p!(n, |a, b, c|)` - Choice placeholder (converts to `${n|a,b,c|}`)
/// - `p!(n, content)` - Placeholder with default value (converts to `${n:content}`)
///
/// # Examples
///
/// ```
/// use cargo_snippet_more::p;
///
/// let p!(1, variable) = p!(2, 10);
/// let mode = p!(3, |"read", "write"|);
/// p!(0);
/// ```
///
/// This generates:
/// ```text
/// let ${1:variable} = ${2:10};
/// let mode = ${3|"read","write"|};
/// $0
/// ```
#[macro_export]
macro_rules! p {
    // p!(0) → $0 (final cursor position)
    (0) => {};
    // p!(n) → ${n} (no default value)
    ($n:literal) => {};
    // p!(n, |a, b, c|) → ${n|a,b,c|} (choices, expands to first choice at runtime)
    ($n:literal, |$first:tt $(,$rest:tt)*|) => { $first };
    // p!(n, content) → ${n:content}
    ($n:literal, $($t:tt)*) => { $($t)* };
}
