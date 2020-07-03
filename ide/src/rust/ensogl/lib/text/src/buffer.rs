#![allow(missing_docs)]

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
    pub use super::Setter        as TRAIT_Setter;
    pub use super::DefaultSetter as TRAIT_DefaultSetter;
}

pub use data::Data;
pub use data::Range;
pub use data::unit::*;
pub use view::*;
pub use style::*;



// fixme - refactor to undo/redo stub
// ================
// === EditType ===
// ================

#[derive(PartialEq,Eq,Clone,Copy,Debug)]
pub enum EditType {
    /// A catchall for edits that don't fit elsewhere, and which should
    /// always have their own undo groups; used for things like cut/copy/paste.
    Other,
    /// An insert from the keyboard/IME (not a paste or a yank).
    Insert,
    Newline,
    /// An indentation adjustment.
    Indent,
    Delete,
    Undo,
    Redo,
    Transpose,
    Surround,
}

impl EditType {
    // /// Checks whether a new undo group should be created between two edits.
    // fn breaks_undo_group(self, previous:EditType) -> bool {
    //     self == EditType::Other || self == EditType::Transpose || self != previous
    // }
}

impl Default for EditType {
    fn default() -> Self {
        Self::Other
    }
}



// ==============
// === Buffer ===
// ==============

#[derive(Clone,CloneRef,Debug,Default)]
pub struct Buffer {
    pub(crate) data : Rc<RefCell<BufferData>>
}

impl Buffer {
    pub fn new() -> Self {
        default()
    }

    /// Creates a new `View` for the buffer.
    pub fn new_view(&self) -> View {
        View::new(self)
    }

    pub fn sub_style(&self, range:impl data::RangeBounds) -> Style {
        self.data.borrow().sub_style(range)
    }
}



// ==================
// === BufferData ===
// ==================

/// Text container with associated styles.
#[derive(Debug,Default)]
pub struct BufferData {
    pub(crate) data       : Data,
    pub(crate) style      : Style,
    pub(crate) undo_stack : Vec<(Data,Style)>,
    pub(crate) redo_stack : Vec<(Data,Style)>,
}

impl Deref for BufferData {
    type Target = Data;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl BufferData {
    pub fn new() -> Self {
        default()
    }

    pub fn sub_style(&self, range:impl data::RangeBounds) -> Style {
        let range = self.crop_range(range);
        self.style.sub(range)
    }

    pub fn style(&self) -> Style {
        self.style.clone()
    }

    pub fn insert(&mut self, range:impl data::RangeBounds, text:&Data) {
        let range = self.crop_range(range);
        self.undo_stack.push((self.data.clone(),self.style.clone()));
        self.redo_stack = default();
        self.data.rope.edit(range.into_rope_interval(),text.rope.clone());
        self.style.modify(range,text.len().bytes());
    }

    pub fn undo(&mut self) {
        if let Some((data,style)) = self.undo_stack.pop() {
            self.redo_stack.push((self.data.clone(),self.style.clone()));
            self.data  = data;
            self.style = style;
        }
    }

    pub fn redo(&mut self) {
        if let Some((data,style)) = self.redo_stack.pop() {
            self.undo_stack.push((self.data.clone(),self.style.clone()));
            self.data  = data;
            self.style = style;
        }
    }
}



// ==============
// === Setter ===
// ==============

pub trait Setter<T> {
    fn modify(&self, range:impl data::RangeBounds, len:Bytes, data:T);
    fn set(&self, range:impl data::RangeBounds, data:T);
}

pub trait DefaultSetter<T> {
    fn set_default(&self, data:T);
}
