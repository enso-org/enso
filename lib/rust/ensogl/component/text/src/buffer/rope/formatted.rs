//! A rope (efficient text representation) with formatting information.

use crate::prelude::*;

use crate::buffer::formatting::Formatting;
use crate::buffer::formatting::FormattingCell;

use enso_text::Rope;
use enso_text::RopeCell;



// =====================
// === FormattedRope ===
// =====================

/// A rope (efficient text representation) with formatting information.
#[derive(Clone, CloneRef, Debug, Default, Deref)]
pub struct FormattedRope {
    data: Rc<FormattedRopeData>,
}

impl FormattedRope {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Replace the content of the buffer with the provided text.
    pub fn replace(&self, range: impl enso_text::RangeBounds, text: impl Into<Rope>) {
        let text = text.into();
        let range = self.crop_byte_range(range);
        let size = text.last_byte_index();
        self.text.replace(range, text);
        self.formatting.set_resize_with_default(range, size);
    }
}



// =========================
// === FormattedRopeData ===
// =========================

/// Internal data of `FormattedRope`.
#[derive(Debug, Default, Deref)]
pub struct FormattedRopeData {
    #[deref]
    pub(crate) text:       RopeCell,
    pub(crate) formatting: FormattingCell,
}

impl FormattedRopeData {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Rope getter.
    pub fn text(&self) -> Rope {
        self.text.get()
    }

    /// Rope setter.
    pub fn set_text(&self, text: impl Into<Rope>) {
        self.text.set(text);
    }

    /// Formatting getter.
    pub fn style(&self) -> Formatting {
        self.formatting.get()
    }

    /// Formatting setter.
    pub fn set_style(&self, style: Formatting) {
        self.formatting.set(style)
    }

    /// Query style information for the provided range.
    pub fn sub_style(&self, range: impl enso_text::RangeBounds) -> Formatting {
        let range = self.crop_byte_range(range);
        self.formatting.sub(range)
    }
}
