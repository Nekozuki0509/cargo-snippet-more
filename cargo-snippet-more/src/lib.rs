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

#[macro_export]
macro_rules! p {
    // p!(0) → $0 (final cursor position)
    (0) => {};
    // p!(n) → ${n} (no default value)
    ($n:literal) => {};
    // p!(n, |a, b, c|) → ${n|a,b,c|} (choices, expands to first choice at runtime)
    ($n:literal, | $first:tt $(, $rest:tt)* |) => { $first };
    // p!(n, content) → ${n:content}
    ($n:literal, $($t:tt)*) => { $($t)* };
}
