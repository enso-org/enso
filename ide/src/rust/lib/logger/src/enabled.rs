//! Contains implementation of default logger.

use enso_prelude::*;

use crate::AnyLogger;
use crate::LogMsg;

use shapely::CloneRef;
use std::fmt::Debug;

#[cfg(target_arch = "wasm32")]
use web_sys::console;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::JsValue;



// ==============
// === Logger ===
// ==============

/// Default Logger implementation.
#[derive(Clone,CloneRef,Debug,Default)]
pub struct Logger {
    /// Path that is used as an unique identifier of this logger.
    pub path:Rc<String>,
}


#[cfg(not(target_arch = "wasm32"))]
impl Logger {
    fn format<M:LogMsg>(path:&str, msg:M) -> String {
        msg.with_log_msg(|s| format!("[{}] {}", path, s))
    }
    /// Log with stacktrace and level:info.
    pub fn trace<M:LogMsg>(path:&str, msg:M) {
        println!("{}",Self::format(path,msg));
    }
    /// Log with level:debug
    pub fn debug<M:LogMsg>(path:&str, msg:M) {
        println!("{}",Self::format(path,msg));
    }
    /// Log with level:info.
    pub fn info<M:LogMsg>(path:&str, msg:M) {
        println!("{}",Self::format(path,msg));
    }
    /// Log with level:warning.
    pub fn warning<M:LogMsg>(path:&str, msg:M) {
        println!("[WARNING] {}",Self::format(path,msg));
    }
    /// Log with level:error.
    pub fn error<M:LogMsg>(path:&str, msg:M) {
        println!("[ERROR] {}",Self::format(path,msg));
    }
    /// Visually groups all logs between group_begin and group_end.
    pub fn group_begin<M:LogMsg>(path:&str, msg:M) {
        println!(">>> {}",Self::format(path,msg));
    }
    /// Visually groups all logs between group_begin and group_end.
    pub fn group_end() {
        println!("<<<")
    }
}

#[cfg(target_arch = "wasm32")]
impl Logger {
    fn format<M:LogMsg>(path:&str, msg:M) -> JsValue {
        msg.with_log_msg(|s| format!("[{}] {}", path, s)).into()
    }
    /// Log with stacktrace and level:info.
    pub fn trace<M:LogMsg>(path:&str, msg:M) {
        console::trace_1(&Self::format(path,msg));
    }
    /// Log with level:debug
    pub fn debug<M:LogMsg>(path:&str, msg:M) {
        console::debug_1(&Self::format(path,msg));
    }
    /// Log with level:info.
    pub fn info<M:LogMsg>(path:&str, msg:M) {
        console::info_1(&Self::format(path,msg));
    }
    /// Log with level:warning.
    pub fn warning<M:LogMsg>(path:&str, msg:M) {
        console::warn_1(&Self::format(path,msg));
    }
    /// Log with level:error.
    pub fn error<M:LogMsg>(path:&str, msg:M) {
        console::error_1(&Self::format(path,msg));
    }
    /// Visually groups all logs between group_begin and group_end.
    pub fn group_begin<M:LogMsg>(path:&str, msg:M) {
        console::group_1(&Self::format(path,msg));
    }
    /// Visually groups all logs between group_begin and group_end.
    pub fn group_end() {
        console::group_end();
    }
}



// ===================
// === Conversions ===
// ===================

impls!{ From + &From <crate::disabled::Logger> for Logger { |logger| Self::new(logger.path()) }}



// ======================
// === AnyLogger Impl ===
// ======================

impl AnyLogger for Logger {
    type This = Self;

    fn path(&self) -> &str {
        self.path.as_str()
    }

    fn new(path:impl Str) -> Self {
        Self {path:Rc::new(path.into())}
    }

    fn trace      <M:LogMsg>(&self, msg:M) { Self::trace      (&self.path,msg) }
    fn debug      <M:LogMsg>(&self, msg:M) { Self::debug      (&self.path,msg) }
    fn info       <M:LogMsg>(&self, msg:M) { Self::info       (&self.path,msg) }
    fn warning    <M:LogMsg>(&self, msg:M) { Self::warning    (&self.path,msg) }
    fn error      <M:LogMsg>(&self, msg:M) { Self::error      (&self.path,msg) }
    fn group_begin<M:LogMsg>(&self, msg:M) { Self::group_begin(&self.path,msg) }
    fn group_end            (&self       ) { Self::group_end  (              ) }
}
