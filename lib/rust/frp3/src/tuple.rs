macro_rules! _impl_tuples {
    (f=[[$f:tt]] args=[[$($arg:tt)*]]) => ();
    (f=[[$f:tt]] args=[[$($arg:tt)*]] $head:tt $($tail:tt)*) => {
        $crate::tuple::_impl_tuples! { f=[[$f]] args=[[$($arg)*]] $($tail)* }
        $f!($($arg)* $head $($tail)*);
    };
}

macro_rules! _impl_tuples_both_ways {
    (
        f=[[$f:tt]]
        args=[[$($arg:tt)*]]
        forward=$forward:tt
        reverse=$reverse:tt
    ) => {
        $f! {
            $($($arg:tt)*)?
            forward=$forward
            reverse=$reverse
        }
    };
    (
        f=$f:tt args=$args:tt
        forward=[[$($forward:tt)*]]
        reverse=[[$($reverse:tt)*]]
        $head:tt $($tail:tt)*
    ) => {
        _impl_tuples_both_ways! {
            f=$f
            args=$args
            forward=[[ $($forward)* $head ]]
            reverse=[[ $head $($reverse)* ]]
            $($tail)*
        }
    };
    (
        f=$f:tt
        $(args=[[$args:tt]])?
        $(forward=[[$forward:tt]])?
        $(reverse=[[$reverse:tt]])?
        $($tail:tt)*
    ) => {
        _impl_tuples_both_ways! {
            f=$f
            args=[[$($args)?]]
            forward=[[$($forward)?]]
            reverse=[[$($reverse)?]]
            $($tail)*
        }
    };
}

macro_rules! impl_tuples {
    (f=[[$f:tt]] $(args=[[$($arg:tt)*]])?) => {
        $crate::tuple::_impl_tuples! {
            f=[[$f]] args=[[$($($arg)*)?]]
            A B C D E F G H
        }
    };
}

pub trait TupleAsArray {
    type Item;
    const COUNT: usize;
    type Array: IntoIterator<Item = Self::Item>;
    fn as_array(self) -> Self::Array;
    fn as_vec(self) -> Vec<Self::Item>;
}

macro_rules! impl_tuple_as_array {
    ($t:ident $($n:tt $c:tt $letters:tt)*) => {
        impl<$t> TupleAsArray for ($($n,)*) {
            type Item = $t;
            const COUNT: usize = { 0 $(+ $c)* };
            type Array = [Self::Item; { 0 $(+ $c)* }];
            #[allow(non_snake_case)]
            fn as_array(self) -> Self::Array {
                let ($($letters,)*) = self;
                [$($letters),*]
            }
            fn as_vec(self) -> Vec<Self::Item> {
                self.as_array().into_iter().collect()
            }
        }
    };
}

macro_rules! _impl_tuple_as_array {
    ($t:tt $($n:tt)*) => {
        impl_tuple_as_array!( $t $($t 1 $n)* );
    };
}

impl_tuples! {
    f=[[_impl_tuple_as_array]]
    args=[[T]]
}

pub(crate) use _impl_tuples;
pub(crate) use _impl_tuples_both_ways;

pub(crate) use impl_tuples;
