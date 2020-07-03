//! Text style definition (color, bold, italics, etc.).

use crate::data::color;
use crate::buffer::Range;
use super::*;



// ==============
// === Macros ===
// ==============

/// Defines a newtype for a primitive style property, like `Bold`. See usage below to learn more.
macro_rules! def_style_property {
    ($name:ident($field_type:ty)) => {
        /// Style property.
        #[derive(Clone,Copy,Debug,From,PartialEq,PartialOrd)]
        #[allow(missing_docs)]
        pub struct $name {
            /// The raw, weakly typed value.
            pub raw : $field_type
        }

        impl $name {
            /// Constructor.
            pub fn new(raw:$field_type) -> $name { $name {raw} }
        }

        /// Smart constructor.
        #[allow(non_snake_case)]
        pub fn $name(raw:$field_type) -> $name { $name {raw} }
    };
}


/// Defines struct containing all styles information. Also defines many utils, like iterator for it.
/// See the usage below to learn more.
macro_rules! define_styles {
    ($($field:ident : $field_type:ty),* $(,)?) => {

        // === StyleValue ===

        /// The value of a style at some point in the buffer.
        #[derive(Clone,Copy,Debug,Default)]
        pub struct StyleValue {
            $(pub $field : $field_type),*
        }

        #[derive(Debug)]
        struct StyleIteratorComponents {
            $($field : std::vec::IntoIter<(Range<Bytes>,$field_type)>),*
        }


        // === Iterator ===

        #[derive(Debug,Default)]
        struct StyleIteratorValue {
            $($field : Option<(Range<Bytes>,$field_type)>),*
        }

        impl Iterator for StyleIterator {
            type Item = StyleValue;
            fn next(&mut self) -> Option<Self::Item> {
                $(
                    if self.value.$field.map(|t| self.offset < t.0.end) != Some(true) {
                        self.value.$field = self.component.$field.next()
                    }
                    let $field = self.value.$field?.1;
                )*
                self.offset += 1.bytes();
                Some(StyleValue {$($field),*})
            }
        }


        // === Style ===

        /// Definition of possible text styles, like `color`, or `bold`. Each style is encoded as
        /// `Property` for some spans in the buffer.
        #[derive(Clone,Debug,Default)]
        #[allow(missing_docs)]
        pub struct Style {
            $(pub $field : Property<$field_type>),*
        }

        impl Style {
            /// Constructor.
            pub fn new() -> Self {
                Self::default()
            }

            /// Return new style narrowed to the given range.
            pub fn sub(&self, range:Range<Bytes>) -> Self {
                $(let $field = self.$field.sub(range);)*
                Self {$($field),*}
            }

            pub fn modify(&mut self, range:Range<Bytes>, len:Bytes) {
                $(self.$field.replace_resize(range,len,None);)*
            }

            /// Iterate over style values for subsequent bytes of the buffer.
            pub fn iter(&self) -> StyleIterator {
                $(let $field = self.$field.to_vector().into_iter();)*
                StyleIterator::new(StyleIteratorComponents {$($field),*})
            }
        }

        $(
            impl Setter<Option<$field_type>> for Buffer {
                fn modify
                (&self, range:impl data::RangeBounds, len:Bytes, data:Option<$field_type>) {
                    let range = self.data.borrow().crop_range(range);
                    self.data.borrow_mut().style.$field.replace_resize(range,len,data)
                }

                fn set(&self, range:impl data::RangeBounds, data:Option<$field_type>) {
                    let range = self.data.borrow().crop_range(range);
                    self.data.borrow_mut().style.$field.replace_resize(range,range.size(),data)
                }
            }

            impl Setter<$field_type> for Buffer {
                fn modify(&self, range:impl data::RangeBounds, len:Bytes, data:$field_type) {
                    self.modify(range,len,Some(data))
                }

                fn set(&self, range:impl data::RangeBounds, data:$field_type) {
                    self.set(range,Some(data))
                }
            }

            impl DefaultSetter<$field_type> for Buffer {
                fn set_default(&self, data:$field_type) {
                    self.data.borrow_mut().style.$field.default = data;
                }
            }
        )*
    };
}

/// Byte-based iterator for the `Style`.
#[derive(Debug)]
pub struct StyleIterator {
    offset    : Bytes,
    value     : StyleIteratorValue,
    component : StyleIteratorComponents,
}

impl StyleIterator {
    fn new(component:StyleIteratorComponents) -> Self {
        let offset = default();
        let value  = default();
        Self {offset,value,component}
    }

    /// Drop the given amount of bytes.
    pub fn drop(&mut self, bytes:Bytes) {
        for _ in 0 .. bytes.value {
            self.next();
        }
    }
}



// ================
// === Property ===
// ================

/// Style property, like `color` or `bold`. Records text spans it is applied to and a default value
/// used for places not covered by spans. Please note that the default value is can be changed at
/// runtime, which is useful for example when defining text field which should use white letters by
/// default (when new letter is written).
#[derive(Clone,Debug,Default)]
#[allow(missing_docs)]
pub struct Property<T:Clone> {
    pub spans : data::Spans<Option<T>>,
    default   : T,
}

impl<T:Clone> Property<T> {
    /// Return new property narrowed to the given range.
    pub fn sub(&self, range:Range<Bytes>) -> Self {
        let spans   = self.spans.sub(range);
        let default = self.default.clone();
        Self {spans,default}
    }

    /// Convert the property to a vector of spans.
    pub fn to_vector(&self) -> Vec<(Range<Bytes>,T)> {
        let spans_iter = self.spans.to_vector().into_iter();
        spans_iter.map(|t|(t.0,t.1.unwrap_or_else(||self.default.clone()))).collect_vec()
    }

    /// The default value of this property.
    pub fn default(&self) -> &T {
        &self.default
    }
}


// === Deref ===

impl<T:Clone> Deref for Property<T> {
    type Target = data::Spans<Option<T>>;
    fn deref(&self) -> &Self::Target {
        &self.spans
    }
}

impl<T:Clone> DerefMut for Property<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.spans
    }
}



// ========================
// === Style Definition ===
// ========================

def_style_property!(Size(f32));
def_style_property!(Bold(bool));
def_style_property!(Italic(bool));
def_style_property!(Underline(bool));

impl Default for Size      { fn default() -> Self { Self::new(12.0) } }
impl Default for Bold      { fn default() -> Self { Self::new(false) } }
impl Default for Italic    { fn default() -> Self { Self::new(false) } }
impl Default for Underline { fn default() -> Self { Self::new(false) } }

define_styles! {
    size      : Size,
    color     : color::Rgba,
    bold      : Bold,
    italics   : Italic,
    underline : Underline,
}
