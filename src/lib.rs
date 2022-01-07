//! This crate provides an attribute for defining `macro_rules!` macros that have proper visibility
//! and scoping.
//!
//! The default scoping and publicity rules of `macro_rules!` macros are arcane and confusing:
//! they behave like no other item in the Rust programming language,
//! and introduce several frustrating limitations that are hard to work around.
//! This problem will be eventually fixed by a new kind of macro known as [declarative macros 2.0],
//! but that feature has been stuck in limbo for several years now
//! and likely won't be seen on stable Rust for several years more.
//!
//! So that's where this crate comes in.
//! It allows you to place `#[macro_vis]` or `#[macro_vis(VISIBILITY)]` on any `macro_rules!` macro
//! and have it be treated exactly like any other item that supports a visibility modifier -
//! structs, enums, functions, et cetera.
//! It works with both crate-local and public macros,
//! effectively superseding both `#[macro_use]` and `#[macro_export]`.
//!
//! See [the documentation of `#[macro_vis]`][attribute doc] for examples and usage.
//!
//! # The `uncommon_codepoints` warning
//!
//! You will get the `uncommon_codepoints` warning if you use this library,
//! so you will probably want to place this in your crate root:
//!
//! ```
//! #![allow(uncommon_codepoints)]
//! ```
//!
//! # Documenting public macros
//!
//! The documentation of public macros can be slightly improved if run on a Nightly compiler.
//! To enable this, you must first add this attribute to your crate root:
//!
//! ```
//! #![cfg_attr(doc_nightly, feature(decl_macro, rustc_attrs))]
//! ```
//!
//! Then you can build with the `doc_nightly` cfg set,
//! either locally with `RUSTDOCFLAGS="--cfg doc_nightly" cargo +nightly doc`
//! or on docs.rs by adding this to your `Cargo.toml`:
//!
//! ```toml
//! [package.metadata.docs.rs]
//! rustdoc-args = ["--cfg", "doc_nightly"]
//! ```
//!
//! # How it works
//!
//! The trick to get non-`pub` macros working is simple;
//! we just `use` the macro after its definition to get it to be treated like an item.
//! The original macro is renamed to a randomly-generated identifier
//! so it can't be accessed by regular code.
//! This code:
//!
//! ```
//! # use macro_vis::macro_vis;
//! #[macro_vis(pub(crate))]
//! macro_rules! example_macro { () => {}; }
//! ```
//!
//! Gets expanded to something like:
//!
//! ```
//! macro_rules! __example_macro_2994407750278293171 { () => {}; }
//! pub(crate) use __example_macro_2994407750278293171 as example_macro;
//! ```
//!
//! `pub` macros work the same, but apply `#[macro_export]` to the macro and ensure it doesn't show
//! up in the documentation:
//!
//! ```
//! #[doc(hidden)]
//! #[macro_export]
//! macro_rules! __example_macro_2994407750278293171 { () => {}; }
//! pub use __example_macro_2994407750278293171 as example_macro;
//! ```
//!
//! But because a re-export of a `#[doc(hidden)]` item is itself `#[doc(hidden)]`,
//! the macro doesn't show up in the documentation at all.
//! To solve this, the library employs two solutions depending on whether Nightly is available or
//! not:
//!
//! - When `doc_nightly` is not enabled,
//! the library emits a public function whose name is the macro name
//! concatenated with LATIN LETTER RETROFLEX CLICK (ǃ),
//! a character that looks nearly identical to the exclamation mark used to invoke macros.
//! This is done to avoid name collisions between other functions of the same name
//! and the macro's documentation double.
//! However,
//! it has the flaw of causing the macro to appear as a function in the docs even though it isn't,
//! and it doesn't work well with re-exports.
//!
//! - When `doc_nightly` _is_ enabled,
//! the library instead changes the macro to be a macros 2.0-style `pub macro`
//! which obeys proper visibility rules by default.
//! Unlike the previous solution,
//! this one displays the macro in the correct documentation section and
//! with the correct text color,
//! as well as it working properly with inlined re-exports.
//! The drawback is that it doesn't work on stable,
//! and so has a greater risk of breaking in future.
//!
//! # MSRV
//!
//! This crate's minimum supported Rust version is 1.53,
//! the first version to stabilize `non_ascii_idents`.
//! It is currently considered a breaking change to increase this.
//!
//! # Credit
//!
//! Most of the ideas in this crate were discovered and shown to me by
//! [Daniel Henry-Mantilla](https://github.com/danielhenrymantilla),
//! so much of the credit goes to them.
//!
//! [attribute doc]: https://docs.rs/macro-vis/latest/macro_vis/macro.macro_vis.html
//! [declarative macros 2.0]: https://github.com/rust-lang/rust/issues/39412

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, quote_spanned};
use std::{
    collections::hash_map::RandomState,
    convert::identity,
    hash::{BuildHasher, Hasher},
};

/// Attribute that applies a visibility to a `macro_rules!` macro.
///
/// By default, placing `#[macro_vis]` on a macro causes it to have a private visibility
/// like functions, structs and enums do by default.
/// In comparison to regular `macro_rules!` macros, this means two things:
/// - It can be used before it is declared.
/// - It can't be used in submodules declared after the macro without importing it first.
///
/// ```
/// # use macro_vis::macro_vis;
/// // Use before declaration:
/// private_macro!();
///
/// #[macro_vis]
/// macro_rules! private_macro { () => {}; }
///
/// mod inner {
///     // Doesn't work like with a regular macro, because it's not in scope:
///     // private_macro!();
///     // Does work:
///     super::private_macro!();
///     // or:
///     crate::private_macro!();
///     // You can also `use crate::private_macro;` just like any other item.
/// }
/// # // Force the code to be placed at the crate root
/// # fn main() {}
/// ```
///
/// You can also supply a custom visibility to `#[macro_vis]`.
/// For example, to make a macro visible anywhere within the current crate:
///
/// ```
/// inner::example_macro!();
///
/// // No `#[macro_use]` needed!
/// mod inner {
///     # use macro_vis::macro_vis;
///     #[macro_vis(pub(crate))]
///     macro_rules! example_macro { () => {}; }
/// }
/// ```
///
/// Public macros will be exported at the current module path
/// instead of at the crate root as with `#[macro_export]`:
///
/// ```
/// # #![allow(uncommon_codepoints)]
/// pub mod inner {
///     # use macro_vis::macro_vis;
///     #[macro_vis(pub)]
///     macro_rules! public_macro { () => {}; }
/// }
///
/// // Doesn't work like with a `#[macro_export]`ed macro:
/// // crate::public_macro!();
///
/// // Does work:
/// crate::inner::public_macro!();
/// # // Force the code to be placed at the crate root
/// # fn main() {}
/// ```
#[proc_macro_attribute]
pub fn macro_vis(attr: TokenStream1, item: TokenStream1) -> TokenStream1 {
    macro_vis_inner(attr.into(), item.into())
        .unwrap_or_else(identity)
        .into()
}

fn macro_vis_inner(attr: TokenStream, item: TokenStream) -> Result<TokenStream, TokenStream> {
    let vis = parse_vis(attr)?;
    let Macro {
        attrs,
        macro_rules,
        bang,
        name,
        arms,
        rules,
        semi,
    } = parse_macro(item)?;

    let real_name = format_ident!("__{}_{}", name, RandomState::new().build_hasher().finish());

    Ok(match vis {
        Vis::Local { pub_token, scope } => {
            quote! {
                #attrs
                #macro_rules #bang #real_name #arms #semi
                #pub_token #scope use #real_name as #name;
            }
        }
        Vis::Public { pub_token } => {
            let macro_token = Ident::new("macro", macro_rules.span());
            let mut arms_2_0 = Group::new(Delimiter::Brace, macro_2_0_arms(&rules));
            arms_2_0.set_span(arms.span());

            // Concatenate it with LATIN LETTER RETROFLEX CLICK which is valid in identifers
            let display_name = format_ident!("{}ǃ", name);

            quote! {
                #[cfg(not(doc_nightly))]
                #[doc(hidden)]
                #[macro_export]
                #macro_rules #bang #real_name #arms #semi

                #[cfg(not(doc_nightly))]
                #[doc(hidden)]
                #pub_token use #real_name as #name;

                #[cfg(all(doc, not(doc_nightly)))]
                #[doc = "<sup>**\\[macro\\]**</sup>"]
                #attrs
                #pub_token fn #display_name() {}

                #[cfg(doc_nightly)]
                // Force the macro to use `macro_rules!`-like mixed-site hygiene instead of the
                // default definition-site hygiene.
                #[rustc_macro_transparency = "semitransparent"]
                #attrs
                #pub_token #macro_token #name #arms_2_0
            }
        }
    })
}

#[derive(Debug)]
enum Vis {
    Public {
        pub_token: Ident,
    },
    Local {
        pub_token: Option<Ident>,
        scope: Option<Group>,
    },
}

#[derive(Debug)]
struct Macro {
    attrs: TokenStream,
    macro_rules: Ident,
    bang: Punct,
    name: Ident,
    arms: Group,
    rules: Vec<MacroRule>,
    semi: Option<Punct>,
}

#[derive(Debug)]
struct MacroRule {
    matcher: Group,
    equals: Punct,
    greater_than: Punct,
    transcriber: Group,
    semi: Option<Punct>,
}

fn parse_vis(vis: TokenStream) -> Result<Vis, TokenStream> {
    let mut vis = vis.into_iter();

    let pub_token = match vis.next() {
        Some(TokenTree::Ident(pub_token)) if pub_token == "pub" => pub_token,
        Some(token) => {
            return Err(error(token.span(), "expected visibility"));
        }
        None => {
            return Ok(Vis::Local {
                pub_token: None,
                scope: None,
            })
        }
    };

    let scope = match vis.next() {
        Some(TokenTree::Group(scope)) if scope.delimiter() == Delimiter::Parenthesis => scope,
        Some(token) => {
            return Err(error(token.span(), "expected parenthesis"));
        }
        None => return Ok(Vis::Public { pub_token }),
    };

    if let Some(trailing) = vis.next() {
        return Err(error(trailing.span(), "trailing tokens"));
    }

    Ok(Vis::Local {
        pub_token: Some(pub_token),
        scope: Some(scope),
    })
}

fn parse_macro(item: TokenStream) -> Result<Macro, TokenStream> {
    let mut item = item.into_iter();

    let mut attrs = TokenStream::new();

    let macro_rules = loop {
        match item.next() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => {
                let next = item.next().expect("unexpected EOF in attribute");
                if !matches!(&next, TokenTree::Group(group) if group.delimiter() == Delimiter::Bracket)
                {
                    unreachable!("attribute without square brackets");
                }
                attrs.extend([TokenTree::Punct(punct), next]);
            }
            Some(TokenTree::Ident(macro_rules)) if macro_rules == "macro_rules" => {
                break macro_rules;
            }
            token => {
                return Err(error(opt_span(&token), "expected macro_rules! macro"));
            }
        }
    };

    let bang = match item.next() {
        Some(TokenTree::Punct(p)) if p.as_char() == '!' => p,
        token => {
            return Err(error(opt_span(&token), "expected exclamation mark"));
        }
    };

    let name = match item.next() {
        Some(TokenTree::Ident(ident)) => ident,
        token => {
            return Err(error(opt_span(&token), "expected identifier"));
        }
    };

    let arms = match item.next() {
        Some(TokenTree::Group(group)) => group,
        token => {
            return Err(error(opt_span(&token), "expected macro arms"));
        }
    };

    let mut rule_tokens = arms.stream().into_iter();
    let mut rules = Vec::new();

    // The macro arms need proper validation because the following is actually accepted by rustc:
    // #[macro_vis]
    // macro_rules! some_macro { invalid tokens }
    loop {
        let matcher = match rule_tokens.next() {
            Some(TokenTree::Group(group)) => group,
            Some(token) => {
                return Err(error(token.span(), "expected macro matcher"));
            }
            None if rules.is_empty() => {
                return Err(error(arms.span(), "expected macro rules"));
            }
            None => break,
        };
        let equals = match rule_tokens.next() {
            Some(TokenTree::Punct(equals)) if equals.as_char() == '=' => equals,
            token => return Err(error(opt_span(&token), "expected =>")),
        };
        let greater_than = match rule_tokens.next() {
            Some(TokenTree::Punct(greater_than)) if greater_than.as_char() == '>' => greater_than,
            _ => return Err(error(equals.span(), "expected =>")),
        };
        let transcriber = match rule_tokens.next() {
            Some(TokenTree::Group(group)) => group,
            token => return Err(error(opt_span(&token), "expected macro transcriber")),
        };
        let mut rule = MacroRule {
            matcher,
            equals,
            greater_than,
            transcriber,
            semi: None,
        };
        match rule_tokens.next() {
            Some(TokenTree::Punct(semi)) if semi.as_char() == ';' => {
                rule.semi = Some(semi);
                rules.push(rule);
            }
            None => {
                rules.push(rule);
                break;
            }
            Some(token) => {
                return Err(error(token.span(), "expected semicolon"));
            }
        }
    }

    let semi = if arms.delimiter() != Delimiter::Brace {
        Some(match item.next() {
            Some(TokenTree::Punct(semi)) if semi.as_char() == ';' => semi,
            _ => unreachable!("no semicolon after () or []-delimited macro"),
        })
    } else {
        None
    };

    if item.next().is_some() {
        unreachable!("trailing tokens after macro_rules! macro");
    }

    Ok(Macro {
        attrs,
        macro_rules,
        bang,
        name,
        arms,
        rules,
        semi,
    })
}

fn opt_span(token: &Option<TokenTree>) -> Span {
    token
        .as_ref()
        .map(|token| token.span())
        .unwrap_or_else(Span::call_site)
}

fn macro_2_0_arms(rules: &[MacroRule]) -> TokenStream {
    rules
        .iter()
        .map(
            |MacroRule {
                 matcher,
                 equals,
                 greater_than,
                 transcriber,
                 semi,
             }| {
                let comma = semi.as_ref().map(|semi| {
                    let mut comma = Punct::new(',', semi.spacing());
                    comma.set_span(semi.span());
                    comma
                });
                quote!(#matcher #equals #greater_than #transcriber #comma)
            },
        )
        .collect()
}

fn error(span: Span, msg: &str) -> TokenStream {
    quote_spanned!(span=> ::core::compile_error!(#msg))
}

#[cfg(test)]
mod tests {
    use crate::{parse_macro, parse_vis, Macro, Vis};
    use proc_macro2::TokenStream;
    use quote::quote;

    #[test]
    fn vis_parse() {
        assert!(matches!(
            parse_vis(TokenStream::new()),
            Ok(Vis::Local {
                pub_token: None,
                scope: None
            })
        ));
        assert!(matches!(
            parse_vis(quote!(pub)),
            Ok(Vis::Public { pub_token }) if pub_token == "pub"
        ));
        assert!(matches!(
            parse_vis(quote!(pub(crate))),
            Ok(Vis::Local { pub_token: Some(pub_token), scope: Some(scope) })
            if pub_token == "pub" && scope.to_string() == quote!((crate)).to_string()
        ));
        assert!(matches!(
            parse_vis(quote!(pub(foo bar))),
            Ok(Vis::Local { pub_token: Some(pub_token), scope: Some(scope) })
            if pub_token == "pub" && scope.to_string() == quote!((foo bar)).to_string()
        ));
    }

    #[test]
    fn vis_error() {
        macro_rules! assert_err {
            (($($input:tt)*) -> $e:literal) => {
                assert_eq!(
                    parse_vis(quote!($($input)*)).unwrap_err().to_string(),
                    quote!(::core::compile_error!($e)).to_string(),
                );
            };
        }
        assert_err!((priv) -> "expected visibility");
        assert_err!((pub[crate]) -> "expected parenthesis");
        assert_err!((pub() trailing) -> "trailing tokens");
    }

    #[test]
    fn macro_parse() {
        assert!(matches!(
            parse_macro(quote!(macro_rules! foo { (m) => { t } })),
            Ok(Macro { attrs, macro_rules, bang, name, arms, rules, semi: None })
            if attrs.is_empty()
                && macro_rules == "macro_rules"
                && bang.as_char() == '!'
                && name == "foo"
                && arms.to_string() == quote!({ (m) => { t } }).to_string()
                && rules.len() == 1
                && rules[0].matcher.to_string() == quote!((m)).to_string()
                && rules[0].equals.as_char() == '='
                && rules[0].greater_than.as_char() == '>'
                && rules[0].transcriber.to_string() == quote!({ t }).to_string()
                && rules[0].semi.is_none()
        ));
        assert!(matches!(
            parse_macro(quote! {
                #[attr1]
                #[attr2 = "foo"]
                macro_rules! foo [
                    {} => ();
                    [$] => [[]];
                ];
            }),
            Ok(Macro { attrs, arms, rules, semi: Some(semi), .. })
            if attrs.to_string() == quote!(#[attr1] #[attr2 = "foo"]).to_string()
                && arms.to_string() == quote!([{} => (); [$] => [[]];]).to_string()
                && semi.as_char() == ';'
                && rules.len() == 2
                && rules[0].matcher.to_string() == quote!({}).to_string()
                && rules[0].transcriber.to_string() == quote!(()).to_string()
                && rules[0].semi.as_ref().map_or(false, |semi| semi.as_char() == ';')
                && rules[1].matcher.to_string() == quote!([$]).to_string()
                && rules[1].transcriber.to_string() == quote!([[]]).to_string()
                && rules[1].semi.as_ref().map_or(false, |semi| semi.as_char() == ';')
        ));
    }

    #[test]
    fn macro_error() {
        macro_rules! assert_err {
            (($($input:tt)*) -> $e:literal) => {
                assert_eq!(
                    parse_macro(quote!($($input)*)).unwrap_err().to_string(),
                    quote!(::core::compile_error!($e)).to_string(),
                );
            }
        }
        assert_err!(() -> "expected macro_rules! macro");
        assert_err!((const _: () = {};) -> "expected macro_rules! macro");
        assert_err!((macro_rules x {}) -> "expected exclamation mark");
        assert_err!((macro_rules! { () => {} }) -> "expected identifier");
        assert_err!((macro_rules! foo) -> "expected macro arms");
        assert_err!((macro_rules! foo { }) -> "expected macro rules");
        assert_err!((macro_rules! foo { # }) -> "expected macro matcher");
        assert_err!((macro_rules! foo { () }) -> "expected =>");
        assert_err!((macro_rules! foo { () = }) -> "expected =>");
        assert_err!((macro_rules! foo { () => }) -> "expected macro transcriber");
        assert_err!((macro_rules! foo { () => {} () => {} }) -> "expected semicolon");
    }
}
