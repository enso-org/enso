//! This module defines several useful string variants, including copy-on-write and immutable
//! implementations.

use std::borrow::Cow;

use crate::impls;
use crate::clone::*;
use std::ops::Deref;
use std::rc::Rc;
use derive_more::*;



// ===========
// === Str ===
// ===========

/// Abstraction for any kind of string as an argument. Functions defined as
/// `fn test<S:Str>(s: Str) { ... }` can be called with `String`, `&String`, and `&str` without
/// requiring caller to know the implementation details. Moreover, the definition can decide if it
/// needs allocation or not. Calling `s.as_ref()` will never allocate, while `s.into()` will
/// allocate only when necessary.
pub trait Str = Into<String> + AsRef<str>;



// =================
// === CowString ===
// =================

// === Definition ===

/// A copy-on-write String implementation. It is a newtype wrapper for `Cow<'static,str>` and
/// provides many useful impls for efficient workflow. Use it whenever you want to store a string
/// but you are not sure if the string will be allocated or not. This way you can store a static
/// slice as long as you can and switch to allocated String on demand.
#[derive(Clone,Debug,Default,Display)]
pub struct CowString(Cow<'static,str>);


// === Conversions From CowString ===

impls!{ From <&CowString> for String { |t| t.clone().into() } }
impls!{ From <CowString>  for String { |t| t.0.into()       } }


// === Conversions To CowString ===

impls!{ From <Cow<'static,str>>  for CowString { |t| Self(t)              } }
impls!{ From <&Cow<'static,str>> for CowString { |t| Self(t.clone())      } }
impls!{ From <&'static str>      for CowString { |t| Self(t.into())       } }
impls!{ From <String>            for CowString { |t| Self(t.into())       } }
impls!{ From <&String>           for CowString { |t| t.to_string().into() } }
impls!{ From <&CowString>        for CowString { |t| t.clone()            } }


// === Instances ===

impl Deref for CowString {
    type Target = str;
    fn deref(&self) -> &str {
        self.0.deref()
    }
}

impl AsRef<str> for CowString {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}



// ================
// === ImString ===
// ================

/// Immutable string implementation with a fast clone implementation.
#[derive(Clone,CloneRef,Debug,Default,Eq,Hash,PartialEq)]
pub struct ImString {
    content : Rc<String>
}

impl ImString {
    /// Constructor.
    pub fn new(content:impl Into<String>) -> Self {
        let content = Rc::new(content.into());
        Self {content}
    }
}

impl std::fmt::Display for ImString {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.content)
    }
}

impl Deref for ImString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl AsRef<ImString> for ImString {
    fn as_ref(&self) -> &ImString {
        self
    }
}

impl AsRef<String> for ImString {
    fn as_ref(&self) -> &String {
        self.content.as_ref()
    }
}

impl AsRef<str> for ImString {
    fn as_ref(&self) -> &str {
        self.content.as_ref()
    }
}

impl From<String> for ImString {
    fn from(t:String) -> Self {
        Self::new(t)
    }
}

impl From<&String> for ImString {
    fn from(t:&String) -> Self {
        Self::new(t)
    }
}

impl From<&&String> for ImString {
    fn from(t:&&String) -> Self {
        Self::new(*t)
    }
}

impl From<&str> for ImString {
    fn from(t:&str) -> Self {
        Self::new(t)
    }
}

impl From<&&str> for ImString {
    fn from(t:&&str) -> Self {
        Self::new(*t)
    }
}

impl PartialEq<String> for ImString {
    fn eq(&self, other:&String) -> bool {
        self.content.as_ref().eq(other)
    }
}

impl PartialEq<ImString> for String {
    fn eq(&self, other:&ImString) -> bool {
        self.eq(other.content.as_ref())
    }
}


// === Macros ===

/// Defines a newtype for `ImString`.
#[macro_export]
macro_rules! im_string_newtype {
    ($($(#$meta:tt)* $name:ident),* $(,)?) => {$(
        $(#$meta)*
        #[derive(Clone,CloneRef,Debug,Default,Eq,Hash,PartialEq)]
        pub struct $name {
            content : ImString
        }

        impl $name {
            /// Constructor.
            pub fn new(content:impl Into<ImString>) -> Self {
                let content = content.into();
                Self {content}
            }
        }

        impl Deref for $name {
            type Target = str;
            fn deref(&self) -> &Self::Target {
                &self.content
            }
        }

        impl AsRef<$name> for $name {
            fn as_ref(&self) -> &$name {
                self
            }
        }

        impl AsRef<ImString> for $name {
            fn as_ref(&self) -> &ImString {
                self.content.as_ref()
            }
        }

        impl AsRef<String> for $name {
            fn as_ref(&self) -> &String {
                self.content.as_ref()
            }
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                self.content.as_ref()
            }
        }

        impl From<String> for $name {
            fn from(t:String) -> Self {
                Self::new(t)
            }
        }

        impl From<&String> for $name {
            fn from(t:&String) -> Self {
                Self::new(t)
            }
        }

        impl From<&&String> for $name {
            fn from(t:&&String) -> Self {
                Self::new(t)
            }
        }

        impl From<&str> for $name {
            fn from(t:&str) -> Self {
                Self::new(t)
            }
        }

        impl From<&&str> for $name {
            fn from(t:&&str) -> Self {
                Self::new(t)
            }
        }
    )*};
}
