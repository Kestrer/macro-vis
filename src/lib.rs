use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, quote_spanned};
use std::{
    collections::hash_map::RandomState,
    convert::identity,
    hash::{BuildHasher, Hasher},
};

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
        semi,
    } = parse_macro(item)?;

    Ok(match vis {
        Vis::Local { pub_token, scope } => {
            quote! {
                #attrs
                #macro_rules #bang #name #arms #semi
                #pub_token #scope use #name;
            }
        }
        Vis::Public { pub_token } => {
            let real_name =
                format_ident!("__{}_{}", name, RandomState::new().build_hasher().finish());

            let mut braced_arms = Group::new(Delimiter::Brace, arms.stream());
            braced_arms.set_span(arms.span());

            quote! {
                #[cfg(not(doc_nightly))]
                #[doc(hidden)]
                #[macro_export]
                #attrs
                #macro_rules #bang #real_name #arms #semi

                #[cfg(not(doc_nightly))]
                #pub_token use crate::#real_name as #name;

                #[cfg(doc_nightly)]
                // Force the macro to use `macro_rules!`-like mixed-site hygiene instead of the
                // default definition-site hygiene.
                #[rustc_macro_transparency = "semitransparent"]
                #attrs
                pub macro #name #braced_arms
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
    semi: Option<Punct>,
}

fn parse_vis(vis: TokenStream) -> Result<Vis, TokenStream> {
    let mut vis = vis.into_iter();

    let pub_token = match vis.next() {
        Some(TokenTree::Ident(pub_token)) if pub_token == "pub" => pub_token,
        Some(token) => {
            return Err(quote_spanned!(token.span()=> compile_error!("expected visibility")))
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
            return Err(quote_spanned!(token.span()=> compile_error!("expected parenthesis")))
        }
        None => return Ok(Vis::Public { pub_token }),
    };

    if let Some(trailing) = vis.next() {
        return Err(quote_spanned!(trailing.span()=> compile_error!("trailing tokens")));
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
                let next = item
                    .next()
                    .ok_or_else(|| quote!(compile_error!("unexpected EOF")))?;
                if !matches!(&next, TokenTree::Group(group) if group.delimiter() == Delimiter::Bracket)
                {
                    return Err(
                        quote_spanned!(next.span()=> compile_error!("expected square brackets")),
                    );
                }
                attrs.extend([TokenTree::Punct(punct), next]);
            }
            Some(TokenTree::Ident(macro_rules)) if macro_rules == "macro_rules" => {
                break macro_rules;
            }
            token => {
                return Err(
                    quote_spanned!(opt_span(&token)=> compile_error!("expected macro_rules! macro")),
                );
            }
        }
    };

    let bang = match item.next() {
        Some(TokenTree::Punct(p)) if p.as_char() == '!' => p,
        token => {
            return Err(
                quote_spanned!(opt_span(&token)=> compile_error!("expected exclamation mark")),
            );
        }
    };

    let name = match item.next() {
        Some(TokenTree::Ident(ident)) => ident,
        token => {
            return Err(quote_spanned!(opt_span(&token)=> compile_error!("expected identifier")));
        }
    };

    let arms = match item.next() {
        Some(TokenTree::Group(group)) => group,
        token => {
            return Err(quote_spanned!(opt_span(&token)=> compile_error!("expected macro arms")));
        }
    };

    let semi = if arms.delimiter() != Delimiter::Brace {
        Some(match item.next() {
            Some(TokenTree::Punct(semi)) if semi.as_char() == ';' => semi,
            token => {
                return Err(
                    quote_spanned!(opt_span(&token)=> compile_error!("expected semicolon")),
                );
            }
        })
    } else {
        None
    };

    if let Some(token) = item.next() {
        return Err(quote_spanned!(token.span()=> compile_error!("trailing tokens")));
    }

    Ok(Macro {
        attrs,
        macro_rules,
        bang,
        name,
        arms,
        semi,
    })
}

fn opt_span(token: &Option<TokenTree>) -> Span {
    token
        .as_ref()
        .map(|token| token.span())
        .unwrap_or_else(Span::call_site)
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
                    quote!(compile_error!($e)).to_string(),
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
            parse_macro(quote!(macro_rules! foo {})),
            Ok(Macro { attrs, macro_rules, bang, name, arms, semi: None })
            if attrs.is_empty()
                && macro_rules == "macro_rules"
                && bang.as_char() == '!'
                && name == "foo"
                && arms.to_string() == quote!({}).to_string()
        ));
        assert!(matches!(
            parse_macro(quote!(#[attr1] #[attr2 = "foo"] macro_rules! foo[];)),
            Ok(Macro { attrs, arms, semi: Some(semi), .. })
            if attrs.to_string() == quote!(#[attr1] #[attr2 = "foo"]).to_string()
                && arms.to_string() == quote!([]).to_string()
                && semi.as_char() == ';'
        ));
    }

    #[test]
    fn macro_error() {
        macro_rules! assert_err {
            (($($input:tt)*) -> $e:literal) => {
                assert_eq!(
                    parse_macro(quote!($($input)*)).unwrap_err().to_string(),
                    quote!(compile_error!($e)).to_string(),
                );
            }
        }
        assert_err!(() -> "expected macro_rules! macro");
        assert_err!((const _: () = {};) -> "expected macro_rules! macro");
        assert_err!((#) -> "unexpected EOF");
        assert_err!((#{}) -> "expected square brackets");
        assert_err!((macro_rules x {}) -> "expected exclamation mark");
        assert_err!((macro_rules! {}) -> "expected identifier");
        assert_err!((macro_rules! foo) -> "expected macro arms");
        assert_err!((macro_rules! foo ()) -> "expected semicolon");
        assert_err!((macro_rules! foo []) -> "expected semicolon");
        assert_err!((macro_rules! foo (); trailing) -> "trailing tokens");
        assert_err!((macro_rules! foo {};) -> "trailing tokens");
    }
}
