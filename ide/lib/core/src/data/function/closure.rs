// ===============
// === Closure ===
// ===============

/// Two implementations are provided, you can switch between them by setting the
/// `no_unboxed_callbacks` flag. We should use unboxed closures whenever
/// possible, however there is a bug in Rust which prevents this version from
/// compilation: https://github.com/rust-lang/rust/issues/65918
#[macro_export]
macro_rules! closure {
    (fn $name:ident <$($param:ident : $param_type:ty),*>
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            |$($larg:ident : $larg_type:ty),*| $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <$($param:$param_type),*>
            ($( $arg  : $arg_type  ),*)
            ($( $larg : $larg_type ),*)
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident <$($param:ident : $param_type:ty),*>
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            || $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <$($param:$param_type),*>
            ($($arg:$arg_type),*) ()
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            |$($larg:ident : $larg_type:ty),*| $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <>
            ($( $arg  : $arg_type  ),*)
            ($( $larg : $larg_type ),*)
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            || $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <>
            ($($arg:$arg_type),*) ()
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident
        <$($param:ident : $param_type:ty),*>
        ($($arg:ident   : $arg_type:ty),*)
        ($($larg:ident  : $larg_type:ty),*)
        -> $type:ident
        $body:tt
    ) => { paste::item! {
        #[cfg(not(feature = "no_unboxed_callbacks"))]
        pub type $type<$($param),*> =
            impl Fn($($larg_type),*) + Clone;

        #[cfg(not(feature = "no_unboxed_callbacks"))]
        pub fn $name<$($param:$param_type),*>
        ($($arg:$arg_type),*) -> $type<$($param),*> {
            move |$($larg),*| $body
        }

        #[cfg(feature = "no_unboxed_callbacks")]
        pub type $type<$($param),*> =
            WithPhantom<Rc<dyn Fn($($larg_type),*)>, $($param),*>;

        #[cfg(feature = "no_unboxed_callbacks")]
        pub fn $name<$($param:$param_type),*>
        ($($arg:$arg_type),*)
        -> WithPhantom<Rc<dyn Fn($($larg_type),*)>, $($param),*> {
            WithPhantom::new(Rc::new(move |$($larg),*| $body))
        }
    }};
}


// ===============
// === Promote ===
// ===============

/// Promotion of closures is a complex topic. Consider the following code:
///
/// ```compile_fail
/// pub type Buffer<T,OnResize> = Observable<Vec<T>, BufferOnSet<OnSet>>;
///
/// closure! {
///     fn buffer_on_set<C:Callback0> (dirty: ResizeDirty<C>) ->
///         BufferOnSet { || dirty.set() }
/// }
/// ```
///
/// It defines an unboxed closure with type `BufferOnSet<C>`, where `C` is
/// potentially another closure which is called when the buffer was set for the
/// first time (this is how the `dirty` flag behaves).
///
/// In another file we've got:
///
/// ```compile_fail
/// closure! {
///     fn attribute_on_set<C:Callback0> (dirty:AttributeDirty<C>, ix: usize) ->
///         AttributeOnSet { || dirty.set(ix) }
/// }
/// ```
///
/// And we would like to promote the `Buffer` type:
///
/// ```compile_fail
/// pub type Buffer<T,C> = file1::Buffer<T,AttributeOnSet<C>>;
/// ```
///
/// This macro automates such promotion. See its usages to learn more.
#[macro_export]
macro_rules! promote {

    // === Final expansion, closure names provided in double braces. ===

    ([[$($closure:ident),*]] $module:ident [$name:ident<$($param:ident),*>]) =>{
        pub type $name<$($param),*> =
            $module::$name <$($param),*,$($closure),*>;
    };
    ([[$($closure:ident),*]] $module:ident [$name:ident]) => {
        pub type $name = $module::$name <$($closure),*>;
    };

    // === Intermediate expansion. ===

    ([$($closure:ident),*] $module:ident [$name:ident<$($param:ident),*>]) => {
        pub type $name<$($param),*,Callback> =
            $module::$name <$($param),*,$($closure<Callback>),*>;
    };
    ([$($closure:ident),*] $module:ident [$name:ident]) => {
        pub type $name<Callback> =
            $module::$name <$($closure<Callback>),*>;
    };

    // === Mapped promotion ===

    ($gens:tt $module:ident [$($targets:tt)*]) => {
        eval_tt::eval!{ promote_all($gens,$module,split_comma([$($targets)*])) }
    };
}

#[macro_export]
macro_rules! promote_all {
    ([$gens:tt] [$module:ident] [$($target:tt)*]) => {
        $(promote!{$gens $module $target})*
    };
}
