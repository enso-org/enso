/// This module contains a utility macro that transforms tokens of form `foo [<[bar]>] baz` to
/// `foo <bar> baz`. There are two versions of the macro: `anglify_shallow`, which only does a
/// single pass over the input and `anglify`, which recursively transforms tokens.

/// `anglify` transforms tokens of form `foo [<[bar]>] baz` to `foo <bar> baz`. Works recursively,
/// so `[<[foo [<[bar]>]]>] baz` will produce `<foo <bar>> baz`.
///
/// The macro works by traversing the input based on `tt` tokens, which means it consumes
/// either single token or a token enclosed in matching `[]`/`()` (see
/// https://doc.rust-lang.org/reference/macros-by-example.html#metavariables).
///
/// The syntax was chosen because it shows the intention and it is very unusual that normal Rust
/// code would use it anywhere.
#[macro_export]
macro_rules! anglify {
    ($($ts:tt)*) => {
        $crate::anglify_internal!{[[$crate::rebuild_tts!]] [] $($ts)*}
    };
}

/// Internal helper for [`anglify`].
#[macro_export(local_inner_macros)]
macro_rules! anglify_internal {
    ([ [$($f:tt)*] $($args:tt)* ] [$($out:tt)*])        => { $($f)* {$($args)* $($out)*} };
    ($f:tt [$($out:tt)*] [<[ $($xs:tt)* ]>] $($ts:tt)*) => { anglify_internal!{ $f [$($out)* <] $($xs)* > $($ts)*} };
    ($f:tt [$($out:tt)*] {   $($xs:tt)*   } $($ts:tt)*) => { anglify_internal!{ $f [$($out)* <#1] $($xs)* 1#> $($ts)*} };
    ($f:tt [$($out:tt)*] (   $($xs:tt)*   ) $($ts:tt)*) => { anglify_internal!{ $f [$($out)* <#2] $($xs)* 2#> $($ts)*} };
    ($f:tt [$($out:tt)*] [   $($xs:tt)*   ] $($ts:tt)*) => { anglify_internal!{ $f [$($out)* <#3] $($xs)* 3#> $($ts)*} };
    ($f:tt [$($out:tt)*] $t:tt              $($ts:tt)*) => { anglify_internal!{ $f [$($out)* $t]              $($ts)*} };
}

/// Flatten the token-tree structure. Finds patterns `{ ... }`, `( ... )`, and `[ ... ]`, and
/// replaces them with `<#1 ... 1#>`, `<#2 ... 2#>`, and `<#3 ... 3#>` respectively.
///
/// Because token trees `{ ... }`, `( ... )`, and `[ ... ]` are parsed as a single tt-token, it is
/// hard to write tt-munchers which recursively traverse the token tree. This macro allows you to
/// first flatten the token tree, then apply your target macro, and then rebuild the token tree by
/// using the [`rebuild_tts`] macro.
#[macro_export(local_inner_macros)]
macro_rules! flatten_tts {
    ($f:tt $($ts:tt)*) => {
        $crate::flatten_tts_internal!{$f [] $($ts)*}
    };
}

/// Internal helper for [`flatten_tts`].
#[macro_export(local_inner_macros)]
macro_rules! flatten_tts_internal {
    ([ [$($f:tt)*] $($args:tt)* ] [$($out:tt)*])  => { $($f)* {$($args)* $($out)*} };
    ($f:tt [$($out:tt)*] { $($xs:tt)* } $($ts:tt)*) => { flatten_tts_internal!{ $f [$($out)* <#1] $($xs)* 1#> $($ts)*} };
    ($f:tt [$($out:tt)*] ( $($xs:tt)* ) $($ts:tt)*) => { flatten_tts_internal!{ $f [$($out)* <#2] $($xs)* 2#> $($ts)*} };
    ($f:tt [$($out:tt)*] [ $($xs:tt)* ] $($ts:tt)*) => { flatten_tts_internal!{ $f [$($out)* <#3] $($xs)* 3#> $($ts)*} };
    ($f:tt [$($out:tt)*] $t:tt          $($ts:tt)*) => { flatten_tts_internal!{ $f [$($out)* $t]              $($ts)*} };
}


/// Rebuilds the token-tree structure. Finds patterns `<#1 ... 1#>`, `<#2 ... 2#>`, and
/// `<#3 ... 3#>`, and replaces them with `{ ... }`, `( ... )`, and `[ ... ]` respectively.
///
/// To learn more, see the documentation of [`flatten_tts`].
#[macro_export(local_inner_macros)]
macro_rules! rebuild_tts {
    ($($ts:tt)*) => {
        $crate::rebuild_tts_internal!{[[]] $($ts)*}
    };
}

/// Internal helper for [`rebuild_tts`].
///
/// The macro operates on the `[ [nesting depth 0] [nesting depth 1] ... ] unparsed tokens` layout.
/// Every time `<#1` is found, a new nesting depth is added. Every time `1#>` is found, the first
/// nesting depth is enclosed in brackets and merged with the previous nesting depth. For example:
///
/// Step  1: `[[]] let x = <#1 & <#3 7 3#> 1#>`
/// Step  2: `[[l]] et x = <#1 & <#3 7 3#> 1#>`
/// Step  3: `[[le]] t x = <#1 & <#3 7 3#> 1#>`
/// Step  4: `[[let]]  x = <#1 & <#3 7 3#> 1#>`
/// Step  5: `[[let ]] x = <#1 & <#3 7 3#> 1#>`
/// Step  6: `[[let x]]  = <#1 & <#3 7 3#> 1#>`
/// Step  7: `[[let x ]] = <#1 & <#3 7 3#> 1#>`
/// Step  8: `[[let x =]]  <#1 & <#3 7 3#> 1#>`
/// Step  9: `[[let x = ]] <#1 & <#3 7 3#> 1#>`
/// Step 10: `[[] [let x = ]] & <#3 7 3#> 1#>`
/// Step 11: `[[&] [let x = ]]  <#3 7 3#> 1#>`
/// Step 12: `[[& ] [let x = ]] <#3 7 3#> 1#>`
/// Step 13: `[[] [& ] [let x = ]] 7 3#> 1#>`
/// Step 14: `[[7] [& ] [let x = ]]  3#> 1#>`
/// Step 15: `[[7 ] [& ] [let x = ]] 3#> 1#>`
/// Step 16: `[[& [7 ]] [let x = ]] 1#>`
/// Step 17: `[[let x = {& [7 ]}]]`
/// Step 18: `let x = {& [7 ]}`
#[macro_export(local_inner_macros)]
macro_rules! rebuild_tts_internal {
    ([[$($xs:tt)*]])                                          => {$($xs)*};
    ([                          $($as:tt)*] <#1   $($ts:tt)*) => { rebuild_tts_internal!{ [[]                  $($as)*] $($ts)*} };
    ([                          $($as:tt)*] <#2   $($ts:tt)*) => { rebuild_tts_internal!{ [[]                  $($as)*] $($ts)*} };
    ([                          $($as:tt)*] <#3   $($ts:tt)*) => { rebuild_tts_internal!{ [[]                  $($as)*] $($ts)*} };
    ([[$($cs:tt)*] [$($bs:tt)*] $($as:tt)*] 1#>   $($ts:tt)*) => { rebuild_tts_internal!{ [[$($bs)* {$($cs)*}] $($as)*] $($ts)*} };
    ([[$($cs:tt)*] [$($bs:tt)*] $($as:tt)*] 2#>   $($ts:tt)*) => { rebuild_tts_internal!{ [[$($bs)* ($($cs)*)] $($as)*] $($ts)*} };
    ([[$($cs:tt)*] [$($bs:tt)*] $($as:tt)*] 3#>   $($ts:tt)*) => { rebuild_tts_internal!{ [[$($bs)* [$($cs)*]] $($as)*] $($ts)*} };
    ([             [$($bs:tt)*] $($as:tt)*] $t:tt $($ts:tt)*) => { rebuild_tts_internal!{ [[$($bs)* $t]        $($as)*] $($ts)*} };
}


#[cfg(test)]
mod tests {
    use super::*;

    trace_macros!(true);

    // Required to be detected as test.
    #[test]
    fn test_anglify_expansion() {
        anglify! {
            mod bar {
                use super::*;
                #[derive(Debug)]
                pub struct Foo [<[]>]{
                    baz: Box<Foo [<[  ]>]>
                }


                impl Foo [<[]>] {
                    fn new() -> Foo [<[]>] {
                        unimplemented!()
                    }
                }
            }

        }
    }
}
