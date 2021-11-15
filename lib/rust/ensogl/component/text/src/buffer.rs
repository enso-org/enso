//! Root of text buffer implementation. The text buffer is a sophisticated model for text styling
//! and editing operations.

use crate::prelude::*;



// ===============
// === Exports ===
// ===============

pub mod data;
pub mod style;
pub mod view;

/// Common traits.
pub mod traits {
    pub use super::data::traits::*;
}

pub use data::unit::*;
pub use data::Range;
pub use data::Text;
pub use data::TextCell;
pub use style::*;
pub use view::*;



// ==============
// === Buffer ===
// ==============

/// Internally mutable text container with associated styles.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Buffer {
    data: Rc<BufferData>,
}

impl Deref for Buffer {
    type Target = BufferData;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
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
}



// ==================
// === BufferData ===
// ==================

/// Internal data of `Buffer`.
#[derive(Debug, Default)]
pub struct BufferData {
    pub(crate) text:  TextCell,
    pub(crate) style: StyleCell,
}

impl Deref for BufferData {
    type Target = TextCell;
    fn deref(&self) -> &Self::Target {
        &self.text
    }
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

    /// Style getter.
    pub fn style(&self) -> Style {
        self.style.get()
    }

    /// Style setter.
    pub(crate) fn set_style(&self, style: Style) {
        self.style.set(style)
    }

    /// Query style information for the provided range.
    pub fn sub_style(&self, range: impl data::RangeBounds) -> Style {
        let range = self.crop_byte_range(range);
        self.style.sub(range)
    }
}



// ==============
// === Setter ===
// ==============

/// Generic setter for buffer data and metadata, like colors, font weight, etc.
trait Setter<T> {
    /// Replace the range with the provided value. The exact meaning of this function depends on the
    /// provided data type. See implementations provided in the `style` module.
    fn replace(&self, range: impl data::RangeBounds, data: T);
}

/// Generic setter for default value for metadata like colors, font weight, etc.
trait DefaultSetter<T> {
    /// Replace the default value of the metadata. The exact meaning of this function depends on the
    /// provided data type. See implementations provided in the `style` module.
    fn set_default(&self, data: T);
}

impl Setter<Text> for Buffer {
    fn replace(&self, range: impl data::RangeBounds, text: Text) {
        let range = self.crop_byte_range(range);
        let size = text.byte_size();
        self.text.replace(range, text);
        self.style.set_resize_with_default(range, size);
    }
}

impl Setter<&Text> for Buffer {
    fn replace(&self, range: impl data::RangeBounds, text: &Text) {
        self.replace(range, text.clone())
    }
}
