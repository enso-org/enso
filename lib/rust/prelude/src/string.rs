//! This module defines several useful string variants, including copy-on-write and immutable
//! implementations.

use derive_more::*;
use enso_shapely::clone_ref::*;
use itertools::*;

use crate::impls;

use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::ops::Deref;
use std::rc::Rc;
use std::str::pattern;



// =================
// === StringOps ===
// =================

pub trait StringOps {
    fn is_enclosed(&self, first_char: char, last_char: char) -> bool;

    /// Splits `self` twice. Once at the first occurrence of `start_marker` and once at the first
    /// occurence of `end_marker`. Returns a triple containing the split `self` as a prefix, middle,
    /// and suffix. If `self` could not be split twice, returns [`None`].
    ///
    /// [`None`]: ::std::option::Option::None
    fn split_twice<'a, P>(
        &'a self,
        start_marker: P,
        end_marker: P,
    ) -> Option<(&'a str, &'a str, &'a str)>
    where
        P: pattern::Pattern<'a>;
}

impl<T: AsRef<str>> StringOps for T {
    /// Check if given string starts and ends with given characters.
    ///
    /// Optimized to be O(1) if both characters are within ASCII range.
    fn is_enclosed(&self, first_char: char, last_char: char) -> bool {
        let text = self.as_ref();
        if first_char.is_ascii() && last_char.is_ascii() {
            let bytes = text.as_bytes();
            bytes.first() == Some(&(first_char as u8)) && bytes.last() == Some(&(last_char as u8))
        } else {
            let mut chars = text.chars();
            let first = chars.next();
            let last = chars.last().or(first);
            first == Some(first_char) && last == Some(last_char)
        }
    }

    fn split_twice<'a, P>(
        &'a self,
        start_marker: P,
        end_marker: P,
    ) -> Option<(&'a str, &'a str, &'a str)>
    where
        P: pattern::Pattern<'a>,
    {
        let text = self.as_ref();
        let (prefix, rest) = text.split_once(start_marker)?;
        let (mid, suffix) = rest.split_once(end_marker)?;
        Some((prefix, mid, suffix))
    }
}

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
#[derive(Clone, Debug, Default, Display)]
pub struct CowString(Cow<'static, str>);

// === Conversions From CowString ===

impls! { From <&CowString> for String { |t| t.clone().into() } }
impls! { From <CowString>  for String { |t| t.0.into()       } }

// === Conversions To CowString ===

impls! { From <Cow<'static,str>>  for CowString { |t| Self(t)              } }
impls! { From <&Cow<'static,str>> for CowString { |t| Self(t.clone())      } }
impls! { From <&'static str>      for CowString { |t| Self(t.into())       } }
impls! { From <String>            for CowString { |t| Self(t.into())       } }
impls! { From <&String>           for CowString { |t| t.to_string().into() } }
impls! { From <&CowString>        for CowString { |t| t.clone()            } }

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
#[derive(Clone, CloneRef, Default, Eq, Hash, PartialEq, Ord, PartialOrd)]
#[derive(Deserialize, Serialize)]
pub struct ImString {
    content: Rc<String>,
}

impl ImString {
    /// Constructor.
    pub fn new(content: impl Into<String>) -> Self {
        let content = Rc::new(content.into());
        Self { content }
    }

    /// Extract a string slice containing the entire string.
    pub fn as_str(&self) -> &str {
        &self.content
    }
}

impl std::fmt::Display for ImString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.content, f)
    }
}

impl std::fmt::Debug for ImString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.content, f)
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
    fn from(t: String) -> Self {
        Self::new(t)
    }
}

impl From<&String> for ImString {
    fn from(t: &String) -> Self {
        Self::new(t)
    }
}

impl From<&&String> for ImString {
    fn from(t: &&String) -> Self {
        Self::new(*t)
    }
}

impl From<&str> for ImString {
    fn from(t: &str) -> Self {
        Self::new(t)
    }
}

impl From<&&str> for ImString {
    fn from(t: &&str) -> Self {
        Self::new(*t)
    }
}

impl From<ImString> for String {
    fn from(value: ImString) -> Self {
        match Rc::try_unwrap(value.content) {
            Ok(str) => str,
            Err(rc) => rc.deref().clone(),
        }
    }
}

impl PartialEq<&str> for ImString {
    fn eq(&self, other: &&str) -> bool {
        self.content.as_ref().eq(other)
    }
}

impl PartialEq<str> for ImString {
    fn eq(&self, other: &str) -> bool {
        self.content.as_ref().eq(other)
    }
}

impl PartialEq<ImString> for &str {
    fn eq(&self, other: &ImString) -> bool {
        self.eq(other.content.as_ref())
    }
}

impl PartialEq<ImString> for str {
    fn eq(&self, other: &ImString) -> bool {
        self.eq(other.content.as_ref())
    }
}


impl PartialEq<String> for ImString {
    fn eq(&self, other: &String) -> bool {
        self.content.as_ref().eq(other)
    }
}

impl PartialEq<ImString> for String {
    fn eq(&self, other: &ImString) -> bool {
        self.eq(other.content.as_ref())
    }
}



// ==================
// === ToImString ===
// ==================

/// Conversion of a value to [`ImString`].
#[allow(missing_docs)]
pub trait ToImString {
    fn to_im_string(&self) -> ImString;
}

impl<T: core::fmt::Display> ToImString for T {
    default fn to_im_string(&self) -> ImString {
        format!("{}", self).into()
    }
}

impl ToImString for ImString {
    fn to_im_string(&self) -> ImString {
        self.clone()
    }
}

impl ToImString for String {
    fn to_im_string(&self) -> ImString {
        self.into()
    }
}

impl ToImString for &String {
    fn to_im_string(&self) -> ImString {
        self.into()
    }
}

impl ToImString for str {
    fn to_im_string(&self) -> ImString {
        self.into()
    }
}

impl ToImString for &str {
    fn to_im_string(&self) -> ImString {
        self.into()
    }
}



// === Macros ===

/// Defines a newtype for `ImString`.
#[macro_export]
macro_rules! im_string_newtype {
    ($($(#$meta:tt)* $name:ident),* $(,)?) => {
        im_string_newtype_without_serde!{ $(
            #[derive($crate::serde_reexports::Serialize,$crate::serde_reexports::Deserialize)]
            $(#$meta)* $name
        ),* }
    };
}

#[macro_export]
macro_rules! im_string_newtype_without_serde {
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

        impl From<ImString> for $name {
            fn from(t:ImString) -> Self {
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

        impl From<&$name> for String {
            fn from(t:&$name) -> Self {
                t.content.to_string()
            }
        }
    )*};
}

// ===============================
// === Common Pre- and Postfix ===
// ===============================

/// Return the length of the longest common prefix of the two strings. If they are completely
/// different this will be zero.
///
/// Example:
/// ```
/// # use enso_prelude::*;
/// let a = "🐁hospital";
/// let b = "🐁host";
/// let c = "🐇bunny🐇";
///
/// assert_eq!(common_prefix_length(a, b), 4);
/// assert_eq!(common_prefix_length(a, c), 0);
/// assert_eq!(common_prefix_length(a, a), 9);
/// ```
pub fn common_prefix_length(source_a: &str, source_b: &str) -> usize {
    let shortest = source_a.chars().count().min(source_b.chars().count());
    let chars_a = source_a.chars();
    let chars_b = source_b.chars();
    let mut zipped = chars_a.zip(chars_b);
    let mismatch = zipped.find_position(|(a, b)| *a != *b);
    mismatch.map(|(ix, _)| ix).unwrap_or(shortest)
}

/// Return the length of the longest common postfix of the two strings. If they are completely
/// different this will be zero.
///
/// Example:
/// ```
/// # use enso_prelude::*;
/// let a = "sunny🐇yard";
/// let b = "🐇yard";
/// let c = "🐇";
///
/// assert_eq!(common_postfix_length(a, b), 5);
/// assert_eq!(common_postfix_length(a, c), 0);
/// assert_eq!(common_postfix_length(a, a), 10);
/// ```
pub fn common_postfix_length(source_a: &str, source_b: &str) -> usize {
    let shortest = source_a.chars().count().min(source_b.chars().count());
    let chars_a = source_a.chars().rev();
    let chars_b = source_b.chars().rev();
    let mut zipped = chars_a.zip(chars_b);
    let mismatch = zipped.find_position(|(a, b)| *a != *b);
    mismatch.map(|(ix, _)| ix).unwrap_or(shortest)
}

// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_ops() {
        // === Matching against ascii ===
        assert!("{}".is_enclosed('{', '}'));
        assert!("{ }".is_enclosed('{', '}'));
        assert!(!"{".is_enclosed('{', '}'));
        assert!(!"{a".is_enclosed('{', '}'));
        assert!(!"a}".is_enclosed('{', '}'));
        assert!(!"}".is_enclosed('{', '}'));
        assert!(!"".is_enclosed('{', '}'));
        assert!("{a}".is_enclosed('{', '}'));
        assert!("{字}".is_enclosed('{', '}'));
        assert!(!"{".is_enclosed('{', '}'));
        assert!(!"{字".is_enclosed('{', '}'));
        assert!(!"字}".is_enclosed('{', '}'));
        assert!(!"}".is_enclosed('{', '}'));
        assert!(!"".is_enclosed('{', '}'));

        // === Matching against non-ascii ===
        assert!("【】".is_enclosed('【', '】'));
        assert!("【 】".is_enclosed('【', '】'));
        assert!("【 a】".is_enclosed('【', '】'));
        assert!(!"【".is_enclosed('【', '】'));
        assert!(!"【a".is_enclosed('【', '】'));
        assert!(!"a】".is_enclosed('【', '】'));
        assert!(!"】".is_enclosed('【', '】'));
        assert!(!"".is_enclosed('【', '】'));

        // === Edge case of matching single char string ===
        assert!("{".is_enclosed('{', '{'));
        assert!("【".is_enclosed('【', '【'));

        // === Splitting a string twice ===
        assert!("a.b.c,d,e".split_twice('.', ',').unwrap() == ("a", "b.c", "d,e"));
    }
}
