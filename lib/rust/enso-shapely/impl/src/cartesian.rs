/// Computes a cartesian product of the provided input.
///
/// For the following expression:
/// ```compile_fail
/// cartesian!(f [g] [a b c] [x y z]);
/// ```
///
/// It expands to:
/// ```compile_fail
/// f! { [g] [ [a x] [a y] [a z] [b x] [b y] [b z] [c x] [c y] [c z] ] }
/// ```
///
/// If you provide underscore as second argument, it is skipped in the ouput macro:
///
/// ```compile_fail
/// cartesian!(f _ [a b c] [x y z]);
/// ```
///
/// Expands to:
/// ```compile_fail
/// f! { [ [a x] [a y] [a z] [b x] [b y] [b z] [c x] [c y] [c z] ] }
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
    ([[$f:path]] $out:tt [] $b:tt $init_b:tt) => {
        $f!{ $out }
    };
    ([[$f:path] $args:tt] $out:tt [] $b:tt $init_b:tt) => {
        $f!{ $args $out }
    };
    ($f:tt $out:tt [$a:tt $($at:tt)*] [] $init_b:tt) => {
        $crate::_cartesian_impl!{ $f $out [$($at)*] $init_b $init_b }
    };
    ($f:tt [$($out:tt)*] [$a:tt $($at:tt)*] [$b:tt $($bt:tt)*] $init_b:tt) => {
        $crate::_cartesian_impl!{ $f [$($out)* [$a $b]] [$a $($at)*] [$($bt)*] $init_b }
    };
}
