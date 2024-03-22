/// Computes a cartesian product of the provided input.
///
/// The following expression:
/// ```text
/// cartesian!([macro_path::f [args]] [a b c] [x y z]);
/// ```
///
/// ... expands to:
/// ```text
/// macro_path::f! { [args] [ [a x] [a y] [a z] [b x] [b y] [b z] [c x] [c y] [c z] ] }
/// ```
///
/// The `[args]` part is optional. The following expression:
///
/// ```text
/// cartesian!([macro_path::f] [a b c] [x y z]);
/// ```
///
/// ... expands to:
/// ```text
/// macro_path::f! { [ [a x] [a y] [a z] [b x] [b y] [b z] [c x] [c y] [c z] ] }
/// ```
#[macro_export]
macro_rules! cartesian {
    ($f:tt [$($a:tt)*] [$($b:tt)*]) => {
        $crate::_cartesian_impl!{ $f [] [$($a)*] [$($b)*] [$($b)*] }
    };
}

/// Internal helper for `cartesian` macro.
#[macro_export]
macro_rules! _cartesian_impl {
    ([$f:path] $out:tt [] $b:tt $init_b:tt) => {
        $f!{ $out }
    };
    ([$f:path [$($args:tt)*]] $out:tt [] $b:tt $init_b:tt) => {
        $f!{ [$($args)*] $out }
    };
    ($f:tt $out:tt [$a:tt $($at:tt)*] [] $init_b:tt) => {
        $crate::_cartesian_impl!{ $f $out [$($at)*] $init_b $init_b }
    };
    ($f:tt [$($out:tt)*] [$a:tt $($at:tt)*] [$b:tt $($bt:tt)*] $init_b:tt) => {
        $crate::_cartesian_impl!{ $f [$($out)* [$a $b]] [$a $($at)*] [$($bt)*] $init_b }
    };
}
