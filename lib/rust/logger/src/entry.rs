//! Logger entry. Entry can contain message, grouping, time information, etc.

pub mod message;
pub mod level;

pub use level::DefaultLevels;
pub use level::DefaultFilter;
pub use level::filter_from;

use crate::prelude::*;

use message::Message;



// =============
// === Entry ===
// =============

/// Logger entry. Contains the message, log level, and may contain other information in the future,
/// like time, frame number, etc.
///
/// Please note that grouping is realized by special entries `GroupBegin` and `GroupEnd`. They can
/// be used to define nested groups. See the `macros.rs` module to see example usage.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct Entry<Level> {
    pub level     : Level,
    pub gen_entry : GenericEntry,
}

/// Internal structure of `Entry`.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct GenericEntry {
    /// A dot-separated names of parent loggers and this logger.
    pub path    : ImString,
    pub content : Content,
}

/// Content of the entry. Can either contain simple message, or grouping information.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub enum Content {
    Message    (String),
    GroupBegin (GroupBegin),
    GroupEnd
}

// `Content::GroupBegin` representation.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct GroupBegin {
    pub collapsed : bool,
    pub message   : String,
}

impl<Level> Deref for Entry<Level> {
    type Target = GenericEntry;
    fn deref(&self) -> &Self::Target {
        &self.gen_entry
    }
}

impl Deref for GenericEntry {
    type Target = Content;
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl Content {
    /// Constructor.
    pub fn group_begin(collapsed:bool, message:String) -> Self {
        Self::GroupBegin(GroupBegin{collapsed,message})
    }

    /// Message getter. Returns `None` if it was group end.
    pub fn message(&self) -> Option<&str> {
        match self {
            Self::Message(msg)  => Some(msg),
            Self::GroupBegin(t) => Some(&t.message),
            Self::GroupEnd      => None,
        }
    }
}

impl<Level> Entry<Level> {
    /// Constructor.
    pub fn message(level:impl Into<Level>, path:ImString, message:impl Message) -> Self {
        let level    = level.into();
        let gen_entry = GenericEntry::message(path,message);
        Self {level,gen_entry}
    }

    /// Constructor.
    pub fn group_begin
    (level:impl Into<Level>, path:ImString, message:impl Message, collapsed:bool) -> Self {
        let level     = level.into();
        let gen_entry = GenericEntry::group_begin(path,message,collapsed);
        Self {level,gen_entry}
    }

    /// Constructor.
    pub fn group_end(level:impl Into<Level>, path:ImString) -> Self {
        let level     = level.into();
        let gen_entry = GenericEntry::group_end(path);
        Self {level,gen_entry}
    }
}

impl GenericEntry {
    /// Constructor.
    pub fn message(path:ImString, message:impl Message) -> Self {
        let content = Content::Message(message.get());
        Self {path,content}
    }

    /// Constructor.
    pub fn group_begin
    (path:ImString, message:impl Message, collapsed:bool) -> Self {
        let content = Content::group_begin(collapsed,message.get());
        Self {path,content}
    }

    /// Constructor.
    pub fn group_end(path:ImString) -> Self {
        let content = Content::GroupEnd;
        Self {path,content}
    }
}
