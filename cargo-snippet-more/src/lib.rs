pub use cargo_snippet_more_macros::*;

#[macro_export]
macro_rules! snippet_start {
    () => {};
    ($($tt:tt)*) => {};
}

#[macro_export]
macro_rules! snippet_end {
    ($name:literal) => {};
}

