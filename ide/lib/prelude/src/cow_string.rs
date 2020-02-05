//! This module defines copy-on-write String implementation.

use std::borrow::Cow;

use crate::impls;
use std::ops::Deref;



// =================
// === CowString ===
// =================

// === Definition ===

/// A copy-on-write String implementation. It is a newtype wrapper for `Cow<'static,str>` and
/// provides many useful impls for efficient workflow. Use it whenever you want to store a string
/// but you are not sure if the string will be allocated or not. This way you can store a static
/// slice as long as you can and switch to allocated String on demand.
#[derive(Clone,Debug,Default)]
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
