#![allow(uncommon_codepoints)]
#![cfg_attr(doc_nightly, feature(decl_macro, rustc_attrs))]

use macro_vis::macro_vis;

/// Macro at the crate root.
#[macro_vis(pub)]
macro_rules! at_crate_root {
    (foo) => {
        pub struct Defined;
    };
}

at_crate_root!(foo);
// Check that mixed-site hygiene is correctly used
pub fn should_work() -> Defined {
    Defined
}

pub mod inner {
    use macro_vis::macro_vis;

    /// Macro in a module, and re-exported at the crate root.
    #[macro_vis(pub)]
    macro_rules! in_module {
        (foo) => {
            bar
        };
    }
}

#[doc(inline)]
pub use inner::in_module;
