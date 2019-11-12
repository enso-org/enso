// #[macro_export]
// macro_rules! closure {
//     ($name:ident 
//         <$($param:ident : $param_type:ty),*> 
//         ($($arg:ident   : $arg_type:ty),*)
//         |$($larg:ident  : $larg_type:ty),*|
//         $body:tt
//     ) => { 
//         closure!( $name<$($param:$param_type),*>
//             ($($arg:$arg_type),*)
//             ($($larg:$larg_type)*)
//             $body
//         );
//     };
//     ($name:ident 
//         <$($param:ident : $param_type:ty),*> 
//         ($($arg:ident   : $arg_type:ty),*)
//         || $body:tt) => {
//         closure!($name<$($param:$param_type),*>($($arg:$arg_type),*)()$body);
//     };
//     ($name:ident 
//         <$($param:ident : $param_type:ty),*> 
//         ($($arg:ident   : $arg_type:ty),*)
//         ($($larg:ident  : $larg_type:ty),*)
//         $body:tt
//     ) => { paste::item! {
        // pub type [<Closure_ $name>]<$($param),*> = 
        //     impl Fn($($larg_type),*) + Clone;
        // pub fn $name<$($param:$param_type),*>
        // ($($arg:$arg_type),*) -> [<Closure_ $name>]<$($param),*> {
        //     move |$($larg),*| $body
        // }
//     }};
// }

#[macro_export]
macro_rules! closure {
    ($name:ident 
        <$($param:ident : $param_type:ty),*> 
        ($($arg:ident   : $arg_type:ty),*)
        |$($larg:ident  : $larg_type:ty),*|
        $body:tt
    ) => { 
        closure!( $name<$($param:$param_type),*>
            ($($arg:$arg_type),*)
            ($($larg:$larg_type)*)
            $body
        );
    };
    ($name:ident 
        <$($param:ident : $param_type:ty),*> 
        ($($arg:ident   : $arg_type:ty),*)
        || $body:tt) => {
        closure!($name<$($param:$param_type),*>($($arg:$arg_type),*)()$body);
    };
    ($name:ident 
        <$($param:ident : $param_type:ty),*> 
        ($($arg:ident   : $arg_type:ty),*)
        ($($larg:ident  : $larg_type:ty),*)
        $body:tt
    ) => { paste::item! {
        #[cfg(not(feature = "no_unboxed_callbacks"))]        
        pub type [<Closure_ $name>]<$($param),*> = 
            impl Fn($($larg_type),*) + Clone;

        #[cfg(not(feature = "no_unboxed_callbacks"))]        
        pub fn $name<$($param:$param_type),*>
        ($($arg:$arg_type),*) -> [<Closure_ $name>]<$($param),*> {
            move |$($larg),*| $body
        }

        #[cfg(feature = "no_unboxed_callbacks")]
        pub type [<Closure_ $name>]<$($param),*> = 
            WithPhantomType<Rc<dyn Fn($($larg_type),*)>, $($param),*>;

        #[cfg(feature = "no_unboxed_callbacks")]        
        pub fn $name<$($param:$param_type),*>
        ($($arg:$arg_type),*) 
        -> WithPhantomType<Rc<dyn Fn($($larg_type),*)>, $($param),*> {
            WithPhantomType::new(Rc::new(move |$($larg),*| $body))
        }
    }};
}