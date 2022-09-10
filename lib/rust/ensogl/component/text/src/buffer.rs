//! Root of text buffer implementation. The text buffer is a sophisticated model for text styling
//! and editing operations.

use crate::prelude::*;



#[derive(Debug, Clone, Default, From)]
pub enum TextRange {
    #[default]
    Selections,
    BufferRange(Range<UBytes>),
    RangeBytes(std::ops::Range<UBytes>),
    RangeFull(std::ops::RangeFull),
}



// ==============
// === Export ===
// ==============

pub mod style;
pub mod view;



/// Common traits.
pub mod traits {
    pub use enso_text::traits::*;
}

pub use style::*;
pub use view::*;

pub use enso_text::unit::*;
pub use enso_text::Range;
pub use enso_text::Text;
pub use enso_text::TextCell;



// ==============
// === Buffer ===
// ==============

/// Internally mutable text container with associated styles.
#[derive(Clone, CloneRef, Debug, Default, Deref)]
pub struct Buffer {
    data: Rc<BufferData>,
}

impl Buffer {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Creates a new `View` for the buffer.
    pub fn new_view(&self) -> View {
        View::new(self)
    }

    pub fn replace(&self, range: impl enso_text::RangeBounds, text: impl Into<Text>) {
        let text = text.into();
        let range = self.crop_byte_range(range);
        let size = text.byte_size();
        self.text.replace(range, text);
        self.formatting.set_resize_with_default(range, size);
    }
}



// ==================
// === BufferData ===
// ==================

/// Internal data of `Buffer`.
#[derive(Debug, Default, Deref)]
pub struct BufferData {
    #[deref]
    pub(crate) text:       TextCell,
    pub(crate) formatting: FormattingCell,
}

impl BufferData {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Text getter.
    pub fn text(&self) -> Text {
        self.text.get()
    }

    /// Text setter.
    pub(crate) fn set_text(&self, text: impl Into<Text>) {
        self.text.set(text);
    }

    /// Formatting getter.
    pub fn style(&self) -> Formatting {
        self.formatting.get()
    }

    /// Formatting setter.
    pub(crate) fn set_style(&self, style: Formatting) {
        self.formatting.set(style)
    }

    /// Query style information for the provided range.
    pub fn sub_style(&self, range: impl enso_text::RangeBounds) -> Formatting {
        let range = self.crop_byte_range(range);
        self.formatting.sub(range)
    }
}



// ==============
// === Setter ===
// ==============

// /// Generic setter for default value for metadata like colors, font weight, etc.
// trait DefaultSetter<T> {
//     /// Replace the default value of the metadata. The exact meaning of this function depends on
// the     /// provided data type. See implementations provided in the `style` module.
//     fn set_default(&self, data: T);
// }
