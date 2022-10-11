//! Text style definition (color, bold, italics, etc.).

use super::*;

use crate::buffer::Range;
use crate::font;

use enso_text::spans::RangedValue;


// ==============
// === Export ===
// ==============

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
        // We don't know what types this struct will be instantiated with. So, sometimes we might
        // not be able to derive Eq because of floats, but other structs might not use floats, and
        // will then be flagged by clippy.
        #[allow(clippy::derive_partial_eq_without_eq)]
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

            impl From<&$field_type> for Property {
                fn from(value: &$field_type) -> Self {
                    Property::[<$field:camel>] (Some(*value))
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

        $(
            impl From<&$field_type> for ResolvedProperty {
                fn from(value: &$field_type) -> Self {
                    ResolvedProperty::[<$field:camel>] (*value)
                }
            }
        )*

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

impl From<&color::Rgba> for Property {
    fn from(t: &color::Rgba) -> Self {
        Property::Color(Some(t.into()))
    }
}

impl From<color::Rgba> for ResolvedProperty {
    fn from(t: color::Rgba) -> Self {
        ResolvedProperty::Color(t.into())
    }
}

impl From<&color::Rgba> for ResolvedProperty {
    fn from(t: &color::Rgba) -> Self {
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
            pub fn sub(&self, range:Range<Byte>) -> Self {
                $(let $field = self.$field.sub(range);)*
                Self {$($field),*}
            }

            /// Replace the provided `range` with the `None` value (default), repeated over `len`
            /// bytes. Use with care, as it's very easy to provide incorrect byte size value, which
            /// may result in styles being applied to parts of grapheme clusters only.
            pub fn set_resize_with_default(&mut self, range:Range<Byte>, len:Byte) {
                $(self.$field.replace_resize(range,len,None);)*
            }

            /// Return all span ranges of default values for the given property.
            pub fn span_ranges_of_default_values(&self, tag:PropertyTag) -> Vec<Range<Byte>> {
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
            pub fn set_property(&mut self, range:Range<Byte>, property: Property) {
                let size = Byte::try_from(range.size()).unwrap_or_default();
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
        }
    }};
}

with_formatting_properties! { define_formatting }

impl Formatting {
    /// Returns list of spans for triples of (width, weight, style). The triple is used to identify
    /// a non-variable font family.
    pub fn non_variable_font_spans(&self) -> Vec<RangedValue<Byte, NonVariableFaceHeader>> {
        let seq_width = self.width.to_vector();
        let seq_weight = self.weight.to_vector();
        let seq_style = self.style.to_vector();
        RangedValue::zip3_def_seq(&seq_width, &seq_weight, &seq_style, NonVariableFaceHeader::new)
    }

    /// Return list of spans for different [`NonVariableFaceHeader`]. The result will be aligned
    /// with grapheme cluster boundaries. If the face header changes inside a grapheme cluster, the
    /// cluster will be associated with the header it starts with.
    pub fn chunks_per_font_face<'a>(
        &self,
        rope: &'a Rope,
    ) -> impl Iterator<Item = (std::ops::Range<Byte>, NonVariableFaceHeader)> + 'a {
        let seq_font_header = self.non_variable_font_spans();
        let iter = gen_iter!(move {
            let mut start_byte = Byte(0);
            let mut end_byte = Byte(0);
            let mut header_iter = seq_font_header.into_iter();
            let mut opt_header = header_iter.next();
            while let Some(header) = opt_header
               && let Some(new_end_byte) = rope.next_grapheme_offset(end_byte) {
                end_byte = new_end_byte;
                if end_byte >= header.range.end {
                    yield (start_byte..end_byte, header.value);
                    start_byte = end_byte;
                    opt_header = header_iter.next();
                }
            }
            if start_byte != end_byte {
                error!("Misaligned bytes found when shaping text. {:?} != {:?}", start_byte, end_byte);
                yield (start_byte..end_byte, default());
            }
        });
        // We are merging subsequent ranges if they have the same header. The underlying rope
        // implementation can return chunks with the same value. For example, after setting a glyph
        // to a bold face, and unsetting it, there will be separate chunks emitted.
        iter.coalesce(|mut a, b| {
            if a.1 == b.1 {
                a.0.end = b.0.end;
                Ok(a)
            } else {
                Err((a, b))
            }
        })
    }
}



// =================
// === Iterators ===
// =================

/// Byte-based iterator for the [`Formatting`].
#[derive(Debug)]
pub struct FormattingByteIterator {
    offset:    Byte,
    value:     StyleIteratorValue,
    component: StyleIteratorComponents,
}

impl FormattingByteIterator {
    /// Constructor.
    fn new(component: StyleIteratorComponents) -> Self {
        let offset = default();
        let value = default();
        Self { offset, value, component }
    }

    /// Skip the given amount of bytes.
    pub fn skip_bytes(&mut self, bytes: Byte) {
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
            $($field : std::vec::IntoIter<RangedValue<Byte ,$field_type>>),*
        }

        #[derive(Debug,Default)]
        struct StyleIteratorValue {
            $($field : Option<RangedValue<Byte, $field_type>>),*
        }

        impl Iterator for FormattingByteIterator {
            type Item = FormattingForByte;
            fn next(&mut self) -> Option<Self::Item> {
                $(
                    if self.value.$field.map(|t| self.offset < t.range.end) != Some(true) {
                        self.value.$field = self.component.$field.next()
                    }
                    let $field = self.value.$field?.value;
                )*
                self.offset += Byte(1);
                Some(FormattingForByte {$($field),*})
            }
        }

        impl Formatting {
            /// Iterate over style values for subsequent bytes of the buffer.
            pub fn iter_bytes(&self) -> FormattingByteIterator {
                $(let $field = self.$field.to_vector().into_iter();)*
                FormattingByteIterator::new(StyleIteratorComponents {$($field),*})
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
    pub fn sub(&self, range: Range<Byte>) -> Self {
        let spans = self.spans.sub(range);
        let default = self.default;
        Self { spans, default }
    }

    /// Convert the property to a vector of spans.
    pub fn to_vector(&self) -> Vec<RangedValue<Byte, T>> {
        let spans_iter = self.spans.to_vector().into_iter();
        spans_iter.map(|t| t.map_value(|v| v.unwrap_or(self.default))).collect_vec()
    }

    /// Modify the values in the given range, first resolving them. If a value was not set, the
    /// default value will be used instead.
    pub fn modify_resolved(&mut self, range: Range<Byte>, f: impl Fn(T) -> T) {
        self.spans.modify(range, |t| Some(f(t.unwrap_or(self.default))))
    }
}



// ====================
// === PropertyDiff ===
// ====================

/// Run the provided macro with formatting property diffs definition.
#[macro_export]
macro_rules! with_formatting_property_diffs {
    ($macro_name:ident) => {
        $macro_name! {
            Size: f32 = 0.0,
            Weight: i32 = 0,
            Width: i32 = 0,
            SdfWeight: f32 = 0.0,
        }
    };
}

macro_rules! define_property_diffs {
    ($($field:ident : $tp:ty = $def:expr),* $(,)?) => {paste! {

        $(def_unit!{[<$field Diff>]($tp) = $def})*

        /// A diff for a property. It is used to modify a property in a given range. A struct is used
        /// instead of a closure to better fit the FRP-model.
        #[allow(missing_docs)]
        #[derive(Clone, Copy, Debug, From)]
        pub enum PropertyDiff {
            $($field([<$field Diff>])),*
        }

        impl From<PropertyDiff> for PropertyTag {
            fn from(t: PropertyDiff) -> Self {
                match t {
                    $(PropertyDiff::$field(_) => PropertyTag::$field),*
                }
            }
        }

        impl Formatting {
            /// Applies the property diff to the given property.
            pub fn mod_property(&mut self, range:Range<Byte>, property: PropertyDiff) {
                match property {
                    $(PropertyDiff::$field(t) => {
                        self.[<$field:snake:lower>].modify_resolved(range, |p| p.apply_diff(t))
                    })*
                }
            }
        }
    }}
}

with_formatting_property_diffs!(define_property_diffs);

/// Apply the property diff.
#[allow(missing_docs)]
pub trait PropertyDiffApply<Diff> {
    fn apply_diff(&self, diff: Diff) -> Self;
}

impl PropertyDiffApply<SizeDiff> for Size {
    fn apply_diff(&self, diff: SizeDiff) -> Self {
        let value = self.value + diff.value;
        let value = if value < 0.0 { 0.0 } else { value };
        Size { value }
    }
}

impl PropertyDiffApply<WeightDiff> for Weight {
    fn apply_diff(&self, diff: WeightDiff) -> Self {
        Weight::from((self.to_number() as i32 + diff.value).max(0) as u16)
    }
}

impl PropertyDiffApply<WidthDiff> for Width {
    fn apply_diff(&self, diff: WidthDiff) -> Self {
        match self.to_number() as i32 + diff.value {
            1 => Width::UltraCondensed,
            2 => Width::ExtraCondensed,
            3 => Width::Condensed,
            4 => Width::SemiCondensed,
            5 => Width::Normal,
            6 => Width::SemiExpanded,
            7 => Width::Expanded,
            8 => Width::ExtraExpanded,
            9 => Width::UltraExpanded,
            _ => Width::Normal,
        }
    }
}

impl PropertyDiffApply<SdfWeightDiff> for SdfWeight {
    fn apply_diff(&self, diff: SdfWeightDiff) -> Self {
        SdfWeight(self.value + diff.value)
    }
}



// ======================
// === FormattingCell ===
// ======================

/// Internally mutable version of `Formatting`.
#[derive(Clone, Debug, Default)]
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
    pub fn sub(&self, range: Range<Byte>) -> Formatting {
        self.cell.borrow().sub(range)
    }

    /// Replace the provided `range` with the `None` value (default), repeated over `len`
    /// bytes. Use with care, as it's very easy to provide incorrect byte size value, which
    /// may result in styles being applied to parts of grapheme clusters only.
    pub fn set_resize_with_default(&self, range: Range<Byte>, len: Byte) {
        self.cell.borrow_mut().set_resize_with_default(range, len)
    }

    /// Set the property for the given range.
    pub fn set_property(&self, range: Range<Byte>, property: Property) {
        self.cell.borrow_mut().set_property(range, property)
    }

    /// Modify the property in the given range.
    pub fn mod_property(&self, range: Range<Byte>, property: PropertyDiff) {
        self.cell.borrow_mut().mod_property(range, property)
    }

    /// Set property default value in the given range.
    pub fn set_property_default(&self, property: ResolvedProperty) {
        self.cell.borrow_mut().set_property_default(property)
    }

    /// Resolve property by applying default values if needed.
    pub fn resolve_property(&self, property: Property) -> ResolvedProperty {
        self.cell.borrow().resolve_property(property)
    }
}

macro_rules! define_formatting_cell_getters {
    ($($field:ident : $field_type:ty),* $(,)?) => {paste! {
        impl FormattingCell {
            $(
                /// Property getter.
                pub fn [<$field:snake:lower>](&self) -> Spanned<$field_type> {
                    self.cell.borrow().[<$field:snake:lower>].clone()
                }
            )*
        }
    }};
}

with_formatting_properties! { define_formatting_cell_getters }
