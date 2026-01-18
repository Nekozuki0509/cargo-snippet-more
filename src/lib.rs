extern crate proc_macro;

use crate::proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn snippet(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Just register `snippet` attribute.
    // We do nothing here.
    item
}

#[proc_macro_attribute]
pub fn expanded(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

#[macro_export]
macro_rules! snippet_start {
    () => {};
    ($($tt:tt)*) => {};
}

#[macro_export]
macro_rules! snippet_end {
    () => {};
    ($($tt:tt)*) => {};
}
