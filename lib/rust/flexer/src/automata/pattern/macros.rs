//! Useful macros for defining operators over patterns.

/// Generates versions of an operator taking various combinations of by-reference and by-value.
#[macro_export]
macro_rules! gen_ref_versions {
    ($ty_name:ty,$opr_name:ident,$fn_name:ident) => (
        impl $opr_name<&$ty_name> for &$ty_name {
            type Output = $ty_name;
            fn $fn_name(self, rhs:&$ty_name) -> Self::Output {
                self.clone().$fn_name(rhs.clone())
            }
        }

        impl $opr_name<&$ty_name> for $ty_name {
            type Output = $ty_name;
            fn $fn_name(self, rhs:&$ty_name) -> Self::Output {
                self.$fn_name(rhs.clone())
            }
        }

        impl $opr_name<$ty_name> for &$ty_name {
            type Output = $ty_name;
            fn $fn_name(self, rhs:$ty_name) -> Self::Output {
                self.clone().$fn_name(rhs)
            }
        }
    )
}
