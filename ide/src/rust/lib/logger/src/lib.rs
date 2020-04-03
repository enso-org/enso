#![feature(trait_alias)]
#![feature(set_stdio)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_prelude::*;
use shapely::CloneRef;
use std::fmt::Debug;
use wasm_bindgen::JsValue;

#[cfg(target_arch = "wasm32")]
use web_sys::console;



// ==============
// === LogMsg ===
// ==============

pub trait LogMsg {
    fn with_log_msg<F: FnOnce(&str) -> T, T>(&self, f:F) -> T;
}

impl LogMsg for &str {
    fn with_log_msg<F: FnOnce(&str) -> T, T>(&self, f:F) -> T {
        f(self)
    }
}

impl<F: Fn() -> S, S:Str> LogMsg for F {
    fn with_log_msg<G: FnOnce(&str) -> T, T>(&self, f:G) -> T {
        f(self().as_ref())
    }
}


// ==============
// === Logger ===
// ==============

#[derive(Clone,CloneRef,Debug,Default)]
pub struct Logger {
    pub path: Rc<String>,
}

#[allow(dead_code)]
impl Logger {
    pub fn new<T:Str>(path:T) -> Self {
        let path = Rc::new(path.into());
        Self {path}
    }

    pub fn sub<T:Str>(&self, path: T) -> Self {
        Self::new(format!("{}.{}", self.path, path.as_ref()))
    }

    pub fn group<M: LogMsg, T, F: FnOnce() -> T>(&self, msg: M, f: F) -> T {
        self.group_begin(msg);
        let out = f();
        self.group_end();
        out
    }

    fn format<M: LogMsg>(&self, msg: M) -> JsValue {
        msg.with_log_msg(|s| format!("[{}] {}", self.path, s)).into()
    }

    fn format2<M: LogMsg>(&self, msg: M) -> String {
        msg.with_log_msg(|s| format!("[{}] {}", self.path, s))
    }
}

#[cfg(target_arch = "wasm32")]
impl Logger {
    pub fn trace<M: LogMsg>(&self, _msg: M) {
//        console::debug_1(&self.format(msg));
    }

    pub fn info<M: LogMsg>(&self, _msg: M) {
//        console::group_1(&self.format(msg));
//        console::group_end();
    }

    pub fn warning<M: LogMsg>(&self, msg: M) {
        console::warn_1(&self.format(msg));
    }

    pub fn error<M: LogMsg>(&self, msg: M) {
        console::error_1(&self.format(msg));
    }

    pub fn group_begin<M: LogMsg>(&self, _msg: M) {
//        console::group_1(&self.format(msg));
    }

    pub fn group_end(&self) {
//        console::group_end();
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Logger {
    pub fn trace<M: LogMsg>(&self, msg:M) {
        println!("{}",self.format2(msg));
    }
    pub fn info<M: LogMsg>(&self, msg: M) {
        println!("{}",self.format2(msg));
    }
    pub fn warning<M: LogMsg>(&self, msg: M) {
        println!("[WARNING] {}",self.format2(msg));
    }
    pub fn error<M: LogMsg>(&self, msg: M) {
        println!("[ERROR] {}",self.format2(msg));
    }
    pub fn group_begin<M: LogMsg>(&self, msg: M) {
        println!(">>> {}",self.format2(msg));
    }
    pub fn group_end(&self) {
        println!("<<<")
    }
}


// ====================
// === Logger Utils ===
// ====================

#[macro_export]
macro_rules! fmt {
    ($($arg:tt)*) => (||(format!($($arg)*)))
}

#[macro_export]
macro_rules! group {
    ($logger:expr, $message:tt, {$($body:tt)*}) => {{
        let __logger = $logger.clone();
        __logger.group_begin(|| iformat!{$message});
        let out = {$($body)*};
        __logger.group_end();
        out
    }};
}

#[macro_export]
macro_rules! log_template {
    ($method:ident $logger:expr, $message:tt $($rest:tt)*) => {
        $crate::log_template_impl! {$method $logger, iformat!($message) $($rest)*}
    };
}


#[macro_export]
macro_rules! log_template_impl {
    ($method:ident $logger:expr, $expr:expr) => {{
        $logger.$method(|| $expr);
    }};
    ($method:ident $logger:expr, $expr:expr, $body:tt) => {{
        let __logger = $logger.clone();
        __logger.group_begin(|| $expr);
        let out = $body;
        __logger.group_end();
        out
    }};
}

#[macro_export]
macro_rules! with_internal_bug_message { ($f:ident $($args:tt)*) => { $crate::$f! {
"This is a bug. Please report it and and provide us with as much information as \
possible at https://github.com/luna/enso/issues. Thank you!"
$($args)*
}};}

#[macro_export]
macro_rules! log_internal_bug_template {
    ($($toks:tt)*) => {
        $crate::with_internal_bug_message! { log_internal_bug_template_impl $($toks)* }
    };
}

#[macro_export]
macro_rules! log_internal_bug_template_impl {
    ($note:tt $method:ident $logger:expr, $message:tt $($rest:tt)*) => {
        $crate::log_template_impl! {$method $logger,
            format!("Internal Error. {}\n\n{}",iformat!($message),$note) $($rest)*
        }
    };
}

#[macro_export]
macro_rules! trace {
    ($($toks:tt)*) => {
        $crate::log_template! {trace $($toks)*}
    };
}

#[macro_export]
macro_rules! info {
    ($($toks:tt)*) => {
        $crate::log_template! {info $($toks)*}
    };
}

#[macro_export]
macro_rules! warning {
    ($($toks:tt)*) => {
        $crate::log_template! {warning $($toks)*}
    };
}

#[macro_export]
macro_rules! internal_warning {
    ($($toks:tt)*) => {
        $crate::log_internal_bug_template! {warning $($toks)*}
    };
}

impl Into<Logger> for &Logger {
    fn into(self) -> Logger {
        self.clone()
    }
}
