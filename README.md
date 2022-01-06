# macro-vis

This crate provides an attribute for defining `macro_rules!` macros that have proper visibility
and scoping.

The default scoping and publicity rules of `macro_rules!` macros are arcane and confusing:
they behave like no other item in the Rust programming language,
and introduce several frustrating limitations that are hard to work around.
This problem will be eventually fixed by a new kind of macro known as [declarative macros 2.0],
but that feature has been stuck in limbo for several years now
and likely won't be seen on stable Rust for several years more.

So that's where this crate comes in.
It allows you to place `#[macro_vis]` or `#[macro_vis(VISIBILITY)]` on any `macro_rules!` macro
and have it be treated exactly like any other item that supports a visibility modifier -
structs, enums, functions, et cetera.
It works with both crate-local and public macros,
effectively superseding both `#[macro_use]` and `#[macro_export]`.

See [the documentation of `#[macro_vis]`][attribute doc] for examples and usage.

## The `uncommon_codepoints` warning

You will get the `uncommon_codepoints` warning if you use this library,
so you will probably want to place this in your crate root:

```rust
#![allow(uncommon_codepoints)]
```

## Documenting public macros

The documentation of public macros can be slightly improved if run on a Nightly compiler.
To enable this, you must first add this attribute to your crate root:

```rust
#![cfg_attr(doc_nightly, feature(decl_macro, rustc_attrs))]
```

Then you can build with the `doc_nightly` cfg set,
either locally with `RUSTDOCFLAGS="--cfg doc_nightly" cargo +nightly doc`
or on docs.rs by adding this to your `Cargo.toml`:

```toml
[package.metadata.docs.rs]
rustdoc-args = ["--cfg", "doc_nightly"]
```

## How it works

The trick to get non-`pub` macros working is simple;
we just `use` the macro after its definition to get it to be treated like an item.
The original macro is renamed to a randomly-generated identifier
so it can't be accessed by regular code.
This code:

```rust
#[macro_vis(pub(crate))]
macro_rules! example_macro { () => {}; }
```

Gets expanded to something like:

```rust
macro_rules! __example_macro_2994407750278293171 { () => {}; }
pub(crate) use __example_macro_2994407750278293171 as example_macro;
```

`pub` macros work the same, but apply `#[macro_export]` to the macro and ensure it doesn't show
up in the documentation:

```rust
#[doc(hidden)]
#[macro_export]
macro_rules! __example_macro_2994407750278293171 { () => {}; }
pub use __example_macro_2994407750278293171 as example_macro;
```

But because a re-export of a `#[doc(hidden)]` item is itself `#[doc(hidden)]`,
the macro doesn't show up in the documentation at all.
To solve this, the library employs two solutions depending on whether Nightly is available or
not:

- When `doc_nightly` is not enabled,
the library emits a public function whose name is the macro name
concatenated with LATIN LETTER RETROFLEX CLICK (ǃ),
a character that looks nearly identical to the exclamation mark used to invoke macros.
This is done to avoid name collisions between other functions of the same name
and the macro's documentation double.
However,
it has the flaw of causing the macro to appear as a function in the docs even though it isn't,
and it doesn't work well with re-exports.

- When `doc_nightly` _is_ enabled,
the library instead changes the macro to be a macros 2.0-style `pub macro`
which obeys proper visibility rules by default.
Unlike the previous solution,
this one displays the macro in the correct documentation section and
with the correct text color,
as well as it working properly with inlined re-exports.
The drawback is that it doesn't work on stable,
and so has a greater risk of breaking in future.

## Credit

Most of the ideas in this crate were discovered and shown to me by
[Daniel Henry-Mantilla](https://github.com/danielhenrymantilla),
so much of the credit goes to them.

[attribute doc]: https://docs.rs/macro-vis/latest/macro_vis/macro.macro_vis.html
[declarative macros 2.0]: https://github.com/rust-lang/rust/issues/39412

License: MIT
