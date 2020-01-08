/// This module implements the `shared` macro, an utility allowing for easy definition of
/// `Rc<RefCell<...>>` wrappers.


/// This macro provides an easy way to define secure `Rc<RefCell<...>>` wrappers for a given struct.
///
/// This macro accepts a body which is very similar to normal struct definition. There are a few
/// notable differences:
///   - The first token this macro accepts should be the name of the wrapped structure.
///   - The implementation block does not have a name. It is always implemented for the struct.
///     You are allowed to provide multiple impl blocks.
///
/// This macro traverses the definition and for each function, it generates a borrowing counterpart.
/// It also handles the `new` function in a special way. Please note, that this macro generates
/// only safe bindings. If your original function returns a reference, the generated code will fail.
/// If you want to return references with some custom guard system, implement that outside of this
/// macro usage.
///
/// For the given input:
/// ```compile_fail
/// shared! { Uniform
///
/// #[derive(Clone,Debug)]
/// pub struct UniformData<Value> {
///     value: Value,
///     dirty: bool,
/// }
///
/// impl<Value:UniformValue> {
///     /// Constructor.
///     pub fn new(value:Value) -> Self {
///         let dirty = false;
///         Self {value,dirty}
///     }
///
///     /// Checks whether the uniform was changed and not yet updated.
///     pub fn check_dirty(&self) -> bool {
///         self.dirty
///     }
///
///     /// Modifies the value stored by the uniform.
///     pub fn modify<F:FnOnce(&mut Value)>(&mut self, f:F) {
///         self.set_dirty();
///         f(&mut self.value);
///     }
/// }}
/// ```
///
/// The following output will be generated:
///
/// ```compile_fail
/// #[derive(Clone,Debug)]
/// pub struct UniformData<Value> {
///     value: Value,
///     dirty: bool,
/// }
///
/// impl<Value:UniformValue> for UniformData<Value> {
///     #[doc = r###"Constructor."###]
///     pub fn new(value:Value) -> Self {
///         let dirty = false;
///         Self {value,dirty}
///     }
///
///     #[doc = r###"Checks whether the uniform was changed and not yet updated."###]
///     pub fn check_dirty(&self) -> bool {
///         self.dirty
///     }
///
///     #[doc = r###"Modifies the value stored by the uniform."###]
///     pub fn modify<F:FnOnce(&mut Value)>(&mut self, f:F) {
///         self.set_dirty();
///         f(&mut self.value);
///     }
/// }
///
/// #[derive(Clone,Debug)]
/// pub struct Uniform<Value> {
///     rc: Rc<RefCell<UniformData<Value>>>
/// }
///
/// impl<Value:UniformValue> for UniformData<Value> {
///     #[doc = r###"Constructor."###]
///     pub fn new(value:Value) -> Self {
///         let rc = Rc::new(RefCell::new(UniformData::new(value)));
///         Self {rc}
///     }
///
///     #[doc = r###"Checks whether the uniform was changed and not yet updated."###]
///     pub fn check_dirty(&self) -> bool {
///         self.rc.borrow.check_dirty()
///     }
///
///     #[doc = r###"Modifies the value stored by the uniform."###]
///     pub fn modify<F:FnOnce(&mut Value)>(&self, f:F) {
///         self.borrow_mut().modify(f)
///     }
/// }
/// ```
///
/// **Note**
/// Both the implementation as well as usage syntax of this macro will be nicer if it was
/// implemented as procedural macro. However, no IDE supports expansion of procedural macros
/// currently, so it was implemented as macro rules instead.
#[macro_export]
macro_rules! shared {
    ($name:ident $($in:tt)*) => {
        $crate::angles_to_brackets_shallow! { shared_bracket [$name] $($in)* }
    }
}

#[macro_export]
macro_rules! shared_bracket_impl {
    ([impl [$($impl_params:tt)*] $name:ident $name_mut:ident $([$($params:tt)*])?] [
        $(
            $(#[$($meta:tt)*])*
            pub fn $fn_name:ident $([$($fn_params:tt)*])? ($($fn_args:tt)*) $(-> $fn_type:ty)? {
                $($fn_body:tt)*
        })*
    ]) => {
        impl <$($impl_params)*> $name_mut $(<$($params)*>)? {
            $(
                $(#[$($meta)*])*
                pub fn $fn_name $(<$($fn_params)*>)* ($($fn_args)*) $(-> $fn_type)? {$($fn_body)*}
            )*
        }

        impl <$($impl_params)*> $name $(<$($params)*>)? {
            $($crate::shared_bracket_fn! {
                $name_mut :: $(#[$($meta)*])*
                pub fn $fn_name [$($($fn_params)*)*] ($($fn_args)*) $(-> $fn_type)?
            })*
        }
    };
}

#[macro_export]
macro_rules! shared_bracket_fn {
    ( $base:ident :: $(#[$($meta:tt)*])* pub fn new $([$($params:tt)*])?
      ($($arg:ident : $arg_type:ty),*) $(-> $type:ty)? ) => {
        $(#[$($meta)*])*
        pub fn new $(<$($params)*>)* ($($arg : $arg_type),*) $(-> $type)? {
            Self { rc: Rc::new(RefCell::new($base::new($($arg),*))) }
        }
    };
    ( $base:ident :: $(#[$($meta:tt)*])* pub fn $name:ident $([$($params:tt)*])?
      (&self $(,$($arg:ident : $arg_type:ty),+)?) $(-> $type:ty)? ) => {
        $(#[$($meta)*])*
        pub fn $name $(<$($params)*>)* (&self $(,$($arg : $arg_type),*)?) $(-> $type)? {
            self.rc.borrow().$name($($($arg),*)?)
        }
    };
    ( $base:ident :: $(#[$($meta:tt)*])* pub fn $name:ident $([$($params:tt)*])?
      (&mut self $(,$($arg:ident : $arg_type:ty),+)?) $(-> $type:ty)? ) => {
        $(#[$($meta)*])*
        pub fn $name $(<$($params)*>)* (&self $(,$($arg : $arg_type),*)?) $(-> $type)? {
            self.rc.borrow_mut().$name($($($arg),*)?)
        }
    };
}

#[macro_export]
macro_rules! shared_bracket_normalized {
    ( [$name:ident] [
        $(#[$($meta:tt)*])*
        pub struct $name_mut:ident $params:tt {
            $($field:ident : $field_type:ty),* $(,)?
        }

        $(impl $([$($impl_params:tt)*])? {$($impl_body:tt)*})*
    ]) => {
        $crate::shared_struct! {
            $(#[$($meta)*])*
            pub struct $name $name_mut $params {
                $($field : $field_type),*
            }
        }

        $($crate::angles_to_brackets_shallow! {shared_bracket_impl
            [impl [$($($impl_params)*)?] $name $name_mut $params] $($impl_body)*
        })*
    };
}

#[macro_export]
macro_rules! shared_struct {
    (
        $(#[$($meta:tt)*])*
        pub struct $name:ident $name_mut:ident [$($params:tt)*] {
            $($field:ident : $field_type:ty),* $(,)?
        }
    ) => {
        $(#[$($meta)*])*
        pub struct $name <$($params)*> { rc: Rc<RefCell<$name_mut<$($params)*>>> }

        $(#[$($meta)*])*
        pub struct $name_mut <$($params)*> { $($field : $field_type),* }
    };
}

#[macro_export]
macro_rules! angles_to_brackets_shallow {
    ($f:ident $f_arg:tt $($in:tt)*) => {
        $crate::_angles_to_brackets_shallow! { $f $f_arg [] [] [] $($in)* }
    }
}

#[macro_export]
macro_rules! _angles_to_brackets_shallow {
    ( $f:ident $f_arg:tt []                        [$($out:tt)*] []                                ) => { $crate::$f! { $f_arg [$($out)*] } };
    ( $f:ident $f_arg:tt []                        [$($out:tt)*] [$($cout:tt)*]                    ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg []                     [$($out)* $($cout)*]        []                          } };
    ( $f:ident $f_arg:tt []                        [$($out:tt)*] [$($cout:tt)*] <     $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [.]                    [$($out)* $($cout)*]        []                $($rest)* } };
    ( $f:ident $f_arg:tt []                        $out:tt       [$($cout:tt)*] <<    $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [. .]                  $out                        [$($cout)* <]     $($rest)* } };
    ( $f:ident $f_arg:tt []                        $out:tt       [$($cout:tt)*] <<<   $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [. . .]                $out                        [$($cout)* <<]    $($rest)* } };
    ( $f:ident $f_arg:tt []                        $out:tt       [$($cout:tt)*] <<<<  $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [. . . .]              $out                        [$($cout)* <<<]   $($rest)* } };
    ( $f:ident $f_arg:tt []                        $out:tt       [$($cout:tt)*] <<<<< $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [. . . . .]            $out                        [$($cout)* <<<<]  $($rest)* } };
    ( $f:ident $f_arg:tt [$($depth:tt)*]           $out:tt       [$($cout:tt)*] <     $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)* .]         $out                        [$($cout)* <]     $($rest)* } };
    ( $f:ident $f_arg:tt [$($depth:tt)*]           $out:tt       [$($cout:tt)*] <<    $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)* . .]       $out                        [$($cout)* <<]    $($rest)* } };
    ( $f:ident $f_arg:tt [$($depth:tt)*]           $out:tt       [$($cout:tt)*] <<<   $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)* . . .]     $out                        [$($cout)* <<<]   $($rest)* } };
    ( $f:ident $f_arg:tt [$($depth:tt)*]           $out:tt       [$($cout:tt)*] <<<<  $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)* . . . .]   $out                        [$($cout)* <<<<]  $($rest)* } };
    ( $f:ident $f_arg:tt [$($depth:tt)*]           $out:tt       [$($cout:tt)*] <<<<< $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)* . . . . .] $out                        [$($cout)* <<<<<] $($rest)* } };
    ( $f:ident $f_arg:tt [.]                       [$($out:tt)*] $cout:tt       >     $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg []                     [$($out)* $cout]            []                $($rest)* } };
    ( $f:ident $f_arg:tt [. .]                     [$($out:tt)*] [$($cout:tt)*] >>    $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg []                     [$($out)* [$($cout)* >]]    []                $($rest)* } };
    ( $f:ident $f_arg:tt [. . .]                   [$($out:tt)*] [$($cout:tt)*] >>>   $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg []                     [$($out)* [$($cout)* >>]]   []                $($rest)* } };
    ( $f:ident $f_arg:tt [. . . .]                 [$($out:tt)*] [$($cout:tt)*] >>>>  $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg []                     [$($out)* [$($cout)* >>>]]  []                $($rest)* } };
    ( $f:ident $f_arg:tt [. . . . .]               [$($out:tt)*] [$($cout:tt)*] >>>>> $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg []                     [$($out)* [$($cout)* >>>>]] []                $($rest)* } };
    ( $f:ident $f_arg:tt [. $($depth:tt)*]         $out:tt       [$($cout:tt)*] >     $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)*]           $out                        [$($cout)* >]     $($rest)* } };
    ( $f:ident $f_arg:tt [. . $($depth:tt)*]       $out:tt       [$($cout:tt)*] >>    $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)*]           $out                        [$($cout)* >>]    $($rest)* } };
    ( $f:ident $f_arg:tt [. . . $($depth:tt)*]     $out:tt       [$($cout:tt)*] >>>   $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)*]           $out                        [$($cout)* >>>]   $($rest)* } };
    ( $f:ident $f_arg:tt [. . . . $($depth:tt)*]   $out:tt       [$($cout:tt)*] >>>>  $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)*]           $out                        [$($cout)* >>>>]  $($rest)* } };
    ( $f:ident $f_arg:tt [. . . . . $($depth:tt)*] $out:tt       [$($cout:tt)*] >>>>> $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg [$($depth)*]           $out                        [$($cout)* >>>>>] $($rest)* } };

    // Function output handling
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt {$($b:tt)*} $($rest:tt)* )                                                         => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 {$($b)*}]                                 $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt {$($b:tt)*} $($rest:tt)* )                                                  => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 {$($b)*}]                             $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt $t3:tt {$($b:tt)*} $($rest:tt)* )                                           => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 $t3 {$($b)*}]                         $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt $t3:tt $t4:tt {$($b:tt)*} $($rest:tt)* )                                    => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 $t3 $t4 {$($b)*}]                     $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt {$($b:tt)*} $($rest:tt)* )                             => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 $t3 $t4 $t5 {$($b)*}]                 $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt {$($b:tt)*} $($rest:tt)* )                      => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 $t3 $t4 $t5 $t6 {$($b)*}]             $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt {$($b:tt)*} $($rest:tt)* )               => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 $t3 $t4 $t5 $t6 $t7 {$($b)*}]         $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt {$($b:tt)*} $($rest:tt)* )        => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8 {$($b)*}]     $($rest)* } };
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] -> $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt $t9:tt {$($b:tt)*} $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* -> $t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8 $t9 {$($b)*}] $($rest)* } };

    // Any token handling
    ( $f:ident $f_arg:tt $depth:tt $out:tt [$($cout:tt)*] $t:tt $($rest:tt)* ) => { $crate::_angles_to_brackets_shallow! { $f $f_arg $depth $out [$($cout)* $t] $($rest)* } };
}

#[macro_export]
macro_rules! shared_bracket {
    ([$name:ident] [$($in:tt)*]) => {
        $crate::normalize_input! { shared_bracket_normalized [$name] $($in)* }
    }
}

#[macro_export]
macro_rules! normalize_input {
    ($f:ident $f_args:tt $($in:tt)*) => {
        $crate::_normalize_input! { $f $f_args [] $($in)* }
    }
}

#[macro_export]
macro_rules! _normalize_input {
    // Finish.
    ( $f:ident $f_args:tt $out:tt ) => {
        $crate::$f! { $f_args $out }
    };

    // Structs.
    ( $f:ident $f_args:tt [$($out:tt)*]
      $(#[$($meta:tt)*])*
      pub struct $name:tt $([$($params:tt)*])? {$($body:tt)*}
      $($rest:tt)*
    ) => {
        $crate::_normalize_input! { $f $f_args
        [$($out)*
        $(#[$($meta)*])*
        pub struct $name [$($($params)*)?] {$($body)*}
        ] $($rest)* }
    };

    // Any token.
    ( $f:ident $f_args:tt [$($out:tt)*] $in:tt $($rest:tt)* ) => {
        $crate::_normalize_input! { $f $f_args [$($out)* $in] $($rest)* }
    };
}
