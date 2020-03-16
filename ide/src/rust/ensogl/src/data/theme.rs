#![allow(missing_docs)]

//// NOTE
//// THIS FILE IS IN WORK-IN-PROGRESS STATE. IT WILL BE COMPLETELY REFACTORED ON NEXT PR.
//
//use crate::prelude::*;
//use crate::data::color::*;
//
//
//// =============
//// === Theme ===
//// =============
//
//#[derive(Debug,Default)]
//struct Theme {
//    pub children : HashMap<String,Theme>,
//    pub data     : Option<Data>,
//}
//
//impl Theme {
//    pub fn new() -> Self {
//        default()
//    }
//
//    pub fn set<S:Into<Selector>,D:Into<Data>>(&mut self, _selector:S, _data:D) {
//
//    }
//
//    pub fn with<S:Into<Selector>,D:Into<Data>>(mut self, selector:S, data:D) -> Self {
//        self.set(selector,data);
//        self
//    }
//}
//
//
//
//// ============
//// === Data ===
//// ============
//
///// Defines compound canvas shapes.
//macro_rules! into_enum {
//    ( $(#$meta:tt)* pub enum $name:ident { $($field:ident ($($content:tt)*)),* $(,)? } ) => {
//        $(#$meta)*
//        pub enum $name {
//            $($field ($($content)*)),*
//        }
//
//        $(
//            impl From<$($content)*> for $name {
//                fn from(t:$($content)*) -> Self {
//                    Self::$field(t)
//                }
//            }
//        )*
//    }
//}
//
//into_enum! {
//#[derive(Debug)]
//pub enum Data {
//    Int   (i32),
//    Float (f32),
//    Srgba (Srgba),
//}}
//
//impls! { From<Srgb> for Data { |t| Self::Srgba(t.into()) }}
//
//
//
//// =============
//// === Class ===
//// =============
//
//pub struct Class {
//    label : Rc<String>
//}
//
//impl Class {
//    pub fn new<S:Str>(label:S) -> Self {
//        let label = Rc::new(label.into());
//        Self {label}
//    }
//}
//
//impls! { [T:Str] From<T> for Class { |t| Self::new(t) }}
//
//
//
//// ================
//// === Selector ===
//// ================
//
//#[repr(transparent)]
//pub struct Selector {
//    path : List<Class>
//}
//
//impl Selector {
//    #[allow(unsafe_code)]
//    #[allow(trivial_casts)]
//    pub fn split(&self) -> Option<(&Class,&Selector)> {
//        match self.path.split() {
//            None          => None,
//            Some((s,lst)) => {
//                let l = unsafe { &*(lst as *const List<Class> as *const Selector) };
//                Some((s,l))
//            }
//        }
//    }
//}
//
//impls! { From<&str> for Selector { |v| { v.split('.').collect::<Vec<_>>().into()  }}}
//
//impls! { [T:Into<Class>] From<Vec<T>> for Selector { |v| {
//    let path = v.into_iter().map(|t| t.into()).collect();
//    Self {path}
//}}}
//
//
//
//// ===============
//// === Manager ===
//// ===============
//
////pub struct Manager {
////    registry      : HashMap<i32,ThemePath>,
////    active_themes : Theme
////}
//
//#[derive(Debug,Derivative)]
//#[derivative(Clone   (bound=""))]
//#[derivative(Default (bound=""))]
//pub struct List<T> {
//    rc: Rc<ListData<T>>
//}
//
//#[derive(Debug)]
//enum ListData<T> {
//    Empty,
//    Cons {head:T, tail:List<T>}
//}
//
//impl<T> Default for ListData<T> {
//    fn default() -> Self {
//        Self::Empty
//    }
//}
//
//impl<T> List<T> {
//    pub fn new() -> Self {
//        default()
//    }
//
//    pub fn split(&self) -> Option<(&T,&List<T>)> {
//        match *self.rc {
//            ListData::Cons {ref head, ref tail} => Some((head,tail)),
//            _ => None
//        }
//    }
//
//    pub fn push_front(&self, head:T) -> Self {
//        let tail = self.clone();
//        let rc   = Rc::new(ListData::Cons{head,tail});
//        Self {rc}
//    }
//}
//
//impl<T> From<Vec<T>> for List<T> {
//    fn from(mut v:Vec<T>) -> Self {
//        v.reverse();
//        let mut list = List::new();
//        for elem in v {
//            list = list.push_front(elem);
//        }
//        list
//    }
//}
//
//impl<T> FromIterator<T> for List<T> {
//    fn from_iter<I:IntoIterator<Item=T>>(iter:I) -> Self {
//        iter.into_iter().collect::<Vec<T>>().into()
//    }
//}
//
//
//
//
//
//#[cfg(test)]
//mod tests {
//    use super::*;
//
//    #[test]
//    fn test() {
//        let mut theme = Theme::new()
//            .with("node.background",Srgb::new(0.97,0.96,0.95))
//            .with("background",Srgb::new(1.0,1.0,1.0));
//        assert_eq!(1, 1);
//    }
//}