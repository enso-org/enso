//! Text style definition (color, bold, italics, etc.).

use super::*;
use enso_text::spans::RangedValue;

use crate::buffer::Range;
use crate::font;

pub use crate::data::color;
pub use font::Style;
pub use font::Weight;
pub use font::Width;



// =============
// === Units ===
// =============

/// Defines a newtype for a primitive style property, like `Bold`. See usage below to learn more.
macro_rules! def_unit {
    ($name:ident($field_type:ty) = $def:expr) => {
        /// Formatting property.
        #[derive(Clone, Copy, Debug, From, PartialEq, PartialOrd)]
        #[allow(missing_docs)]
        pub struct $name {
            pub value: $field_type,
        }

        impl $name {
            /// Constructor.
            pub const fn new(value: $field_type) -> $name {
                $name { value }
            }
        }

        /// Smart constructor.
        #[allow(non_snake_case)]
        pub fn $name(value: $field_type) -> $name {
            $name { value }
        }

        impl Default for $name {
            fn default() -> Self {
                Self::new($def)
            }
        }
    };
}

def_unit!(Size(f32) = 12.0);
def_unit!(SdfWeight(f32) = 0.0);



/// ==================
/// === Properties ===
/// ==================

/// Run the provided macro with formatting properties definition.
#[macro_export]
macro_rules! with_formatting_properties {
    ($macro_name:ident) => {
        $macro_name! {
            size       : Size,
            color      : color::Lcha,
            weight     : Weight,
            width      : Width,
            style      : Style,
            sdf_weight : SdfWeight,
        }
    };
}

/// Define formatting properties. See the usage to learn more.
macro_rules! define_property {
    ($($field:ident : $field_type:ty),* $(,)?) => {paste! {
        /// Unresolved formatting property. Value of [`None`] indicates a default value.
        #[allow(missing_docs)]
        #[derive(Clone, Copy, Debug, From)]
        pub enum Property {
            $([<$field:camel>] (Option<$field_type>)),*
        }

        impl Property {
            /// The property tag accessor.
            pub fn tag(self) -> PropertyTag {
                self.into()
            }
        }

         $(
            impl From<$field_type> for Property {
                fn from(value: $field_type) -> Self {
                    Property::[<$field:camel>] (Some(value))
                }
            }
        )*

        /// Resolved property. Just like [`Property`] but without the possibility to use default
        /// value placeholder.
        #[allow(missing_docs)]
        #[derive(Clone, Copy, Debug, From)]
        pub enum ResolvedProperty {
            $([<$field:camel>] ($field_type)),*
        }

        impl ResolvedProperty {
            /// The property tag accessor.
            pub fn tag(self) -> PropertyTag {
                self.into()
            }
        }

        /// A property name without values.
        #[allow(missing_docs)]
        #[derive(Clone, Copy, Debug, From)]
        pub enum PropertyTag {
            $([<$field:camel>]),*
        }

        impl From<Property> for PropertyTag {
            fn from(property: Property) -> Self {
                match property {
                    $(Property::[<$field:camel>](_) => Self::[<$field:camel>]),*
                }
            }
        }

        impl From<ResolvedProperty> for PropertyTag {
            fn from(property: ResolvedProperty) -> Self {
                match property {
                    $(ResolvedProperty::[<$field:camel>](_) => Self::[<$field:camel>]),*
                }
            }
        }
    }};
}

with_formatting_properties! { define_property }

impl From<color::Rgba> for Property {
    fn from(t: color::Rgba) -> Self {
        Property::Color(Some(t.into()))
    }
}

impl From<color::Rgba> for ResolvedProperty {
    fn from(t: color::Rgba) -> Self {
        ResolvedProperty::Color(t.into())
    }
}



// ==================
// === Formatting ===
// ==================

/// Defines struct containing all styles information. Also defines many utils, like iterator for it.
/// See the usage below to learn more.
macro_rules! define_formatting {
    ($($field:ident : $field_type:ty),* $(,)?) => {paste! {

        /// Definition of possible text styles, like `color`, or `bold`. Each style is encoded as
        /// [`Spanned`] for some spans in the buffer.
        #[derive(Clone,Debug,Default)]
        #[allow(missing_docs)]
        pub struct Formatting {
            $(pub $field : Spanned<$field_type>),*
        }

        impl Formatting {
            /// Constructor.
            pub fn new() -> Self {
                Self::default()
            }

            /// Return new style narrowed to the given range.
            pub fn sub(&self, range:Range<UBytes>) -> Self {
                $(let $field = self.$field.sub(range);)*
                Self {$($field),*}
            }

            /// Replace the provided `range` with the `None` value (default), repeated over `len`
            /// bytes. Use with care, as it's very easy to provide incorrect byte size value, which
            /// may result in styles being applied to parts of grapheme clusters only.
            pub fn set_resize_with_default(&mut self, range:Range<UBytes>, len:UBytes) {
                $(self.$field.replace_resize(range,len,None);)*
            }

            /// Return all span ranges of default values for the given property.
            pub fn span_ranges_of_default_values(&self, tag:PropertyTag) -> Vec<Range<UBytes>> {
                match tag {
                    $(PropertyTag::[<$field:camel>] => {
                        let spans = self.$field.spans.to_vector();
                        spans.into_iter().filter(|t| t.value.is_none()).map(|t| t.range)
                            .collect_vec()
                    })*
                }
            }
        }

        impl Formatting {
            /// Set the value of the given property for the given range.
            pub fn set_property(&mut self, range:Range<UBytes>, property: Property) {
                let size = UBytes::try_from(range.size()).unwrap_or_default();
                match property {
                    $(Property::[<$field:camel>](t) => self.$field.replace_resize(range, size, t)),*
                }
            }

            /// Resolve the property. Applies the default value if the property value was [`None`].
            pub fn resolve_property(&self, property: Property) -> ResolvedProperty {
                match property {
                    $(Property::[<$field:camel>](t) => ResolvedProperty::[<$field:camel>]
                        (t.unwrap_or_default())),*
                }
            }

            /// Sets a new default value for the given property.
            pub fn set_property_default(&mut self, property: ResolvedProperty) {
                match property {
                    $(ResolvedProperty::[<$field:camel>](t) => self.$field.default = t),*
                }
            }

            /// Applies the property diff to the given property.
            pub fn mod_property(&mut self, range:Range<UBytes>, property: PropertyDiff) {
                match property {
                    PropertyDiff::Size(diff) => self.size.modify_resolved(range, |p|p.apply_diff(diff)),
                }
            }
        }
    }};
}

with_formatting_properties! { define_formatting }

impl Formatting {
    /// Returns list of spans for triples of (width, weight, style). The triple is used to identify
    /// a non-variable font family.
    pub fn non_variable_font_spans(&self) -> Vec<RangedValue<UBytes, font::NonVariableFaceHeader>> {
        let seq_width = self.width.to_vector();
        let seq_weight = self.weight.to_vector();
        let seq_style = self.style.to_vector();
        RangedValue::zip3_def_seq(
            &seq_width,
            &seq_weight,
            &seq_style,
            font::NonVariableFaceHeader::new,
        )
    }

    // FIXME: tu powinnismy gdzies zunifikowac chunki bo czasem sa takie same gdy np zrobilismy
    // literke 2 czerwona a potem z powortem defualtowa - wtedy i tak czunki beda 3 defaultowe

    pub fn chunks_per_font_face<'a>(
        &self,
        content: &'a str,
    ) -> impl Iterator<Item = (std::ops::Range<UBytes>, font::NonVariableFaceHeader)> + 'a {
        let seq_font_header = self.non_variable_font_spans();
        gen_iter!(move {
            let mut start_byte = UBytes(0);
            let mut end_byte = UBytes(0);
            let mut header_iter = seq_font_header.into_iter();
            let mut opt_header = header_iter.next();
            for chr in content.chars() {
                end_byte += UBytes(chr.len_utf8());
                if let Some(header) = opt_header {
                    if end_byte == header.range.end {
                        yield (start_byte..end_byte, header.value);
                        start_byte = end_byte;
                        opt_header = header_iter.next();
                    }
                }
            }
            if start_byte != end_byte {
                error!("Misaligned bytes found when shaping text. {:?} != {:?}", start_byte, end_byte);
                yield (start_byte..end_byte, default());
            }
        })
    }
}

fn merge_same_values<T>(mut iterator: impl Iterator<Item = T>) -> impl Iterator<Item = T> {
    gen_iter!(move {
        let mut to_be_yielded = None;
        for t in iterator {
            if let Some(to_be_yielded) = to_be_yielded {
                if to_be_yielded != t {
                    yield to_be_yielded;
                    to_be_yielded = None;
                }
            } else {
                to_be_yielded = Some(t);
            }
            yield t;
        }
    })
}



// =================
// === Iterators ===
// =================

/// Byte-based iterator for the [`Formatting`].
#[derive(Debug)]
pub struct FormattingIterator {
    offset:    UBytes,
    value:     StyleIteratorValue,
    component: StyleIteratorComponents,
}

impl FormattingIterator {
    fn new(component: StyleIteratorComponents) -> Self {
        let offset = default();
        let value = default();
        Self { offset, value, component }
    }

    /// Skip the given amount of bytes.
    pub fn skip_bytes(&mut self, bytes: UBytes) {
        for _ in 0..bytes.value {
            self.next();
        }
    }
}

macro_rules! define_iterators {
    ($($field:ident : $field_type:ty),* $(,)?) => {paste! {

        /// The formatting for the given byte in the buffer.
        #[allow(missing_docs)]
        #[derive(Clone,Copy,Debug,Default)]
        pub struct FormattingForByte {
            $(pub $field : $field_type),*
        }

        #[derive(Debug)]
        struct StyleIteratorComponents {
            $($field : std::vec::IntoIter<RangedValue<UBytes ,$field_type>>),*
        }

        #[derive(Debug,Default)]
        struct StyleIteratorValue {
            $($field : Option<RangedValue<UBytes, $field_type>>),*
        }

        impl Iterator for FormattingIterator {
            type Item = FormattingForByte;
            fn next(&mut self) -> Option<Self::Item> {
                $(
                    if self.value.$field.map(|t| self.offset < t.range.end) != Some(true) {
                        self.value.$field = self.component.$field.next()
                    }
                    let $field = self.value.$field?.value;
                )*
                self.offset += UBytes(1);
                Some(FormattingForByte {$($field),*})
            }
        }

        impl Formatting {
            /// Iterate over style values for subsequent bytes of the buffer.
            pub fn iter(&self) -> FormattingIterator {
                $(let $field = self.$field.to_vector().into_iter();)*
                FormattingIterator::new(StyleIteratorComponents {$($field),*})
            }
        }
    }};
}

with_formatting_properties! { define_iterators }



// ===============
// === Spanned ===
// ===============

/// Formatting property spanned over text. It also contains a default value to be used for places
/// not covered by the spans. The default value can be changed at runtime (e.g. the default color of
/// a text.
#[derive(Clone, Debug, Default, Deref, DerefMut)]
#[allow(missing_docs)]
pub struct Spanned<T: Copy> {
    #[deref]
    #[deref_mut]
    pub spans:   enso_text::Spans<Option<T>>,
    pub default: T,
}

impl<T: Copy + Debug> Spanned<T> {
    /// Return new property narrowed to the given range.
    pub fn sub(&self, range: Range<UBytes>) -> Self {
        let spans = self.spans.sub(range);
        let default = self.default;
        Self { spans, default }
    }

    /// Convert the property to a vector of spans.
    pub fn to_vector(&self) -> Vec<RangedValue<UBytes, T>> {
        let spans_iter = self.spans.to_vector().into_iter();
        spans_iter.map(|t| t.map_value(|v| v.unwrap_or(self.default))).collect_vec()
    }

    /// Modify the values in the given range, first resolving them. If a value was not set, the
    /// default value will be used instead.
    pub fn modify_resolved(&mut self, range: Range<UBytes>, f: impl Fn(T) -> T) {
        self.spans.modify(range, |t| Some(f(t.unwrap_or(self.default))))
    }
}



// ====================
// === PropertyDiff ===
// ====================

/// A diff for a property. It is used to modify a property in a given range. A struct is used
/// instead of a closure to better fit the FRP-model.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, From)]
pub enum PropertyDiff {
    Size(SizeDiff),
}

def_unit!(SizeDiff(f32) = 0.0);

impl Size {
    pub fn apply_diff(&self, diff: SizeDiff) -> Self {
        let value = self.value + diff.value;
        let value = if value < 0.0 { 0.0 } else { value };
        Size { value }
    }
}

impl From<PropertyDiff> for PropertyTag {
    fn from(t: PropertyDiff) -> Self {
        match t {
            PropertyDiff::Size(_) => PropertyTag::Size,
        }
    }
}



// =================
// === FormattingCell ===
// =================

// fixme: remove deref
/// Internally mutable version of `Formatting`.
#[derive(Clone, Debug, Default, Deref)]
pub struct FormattingCell {
    cell: RefCell<Formatting>,
}

impl FormattingCell {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Getter of the current style value.
    pub fn get(&self) -> Formatting {
        self.cell.borrow().clone()
    }

    /// Setter of the style value.
    pub fn set(&self, style: Formatting) {
        *self.cell.borrow_mut() = style;
    }

    /// Return style narrowed to the given range.
    pub fn sub(&self, range: Range<UBytes>) -> Formatting {
        self.cell.borrow().sub(range)
    }

    /// Replace the provided `range` with the `None` value (default), repeated over `len`
    /// bytes. Use with care, as it's very easy to provide incorrect byte size value, which
    /// may result in styles being applied to parts of grapheme clusters only.
    pub fn set_resize_with_default(&self, range: Range<UBytes>, len: UBytes) {
        self.cell.borrow_mut().set_resize_with_default(range, len)
    }

    pub fn set_property(&self, range: Range<UBytes>, property: Property) {
        self.cell.borrow_mut().set_property(range, property)
    }

    pub fn mod_property(&self, range: Range<UBytes>, property: PropertyDiff) {
        self.cell.borrow_mut().mod_property(range, property)
    }
}
