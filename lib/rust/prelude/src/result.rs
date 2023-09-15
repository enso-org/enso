//! This module defines utilities for working with the `Result` type.

use std::fmt;
use std::fmt::Display;



// =================
// === ResultOps ===
// =================

/// Adds utilities to the `Result` type.
pub trait ResultOps {
    type Item;
    type Error;

    /// Call the given handler if this is an error and promote `Result` to `Option`.
    fn handle_err<F>(self, f: F) -> Option<Self::Item>
    where F: FnOnce(Self::Error);

    /// Print an error log if [`Err`]. The error message will be added to the `fmt` arguments.
    fn log_err_fmt(&self, fmt: fmt::Arguments)
    where Self::Error: Display;

    /// Print an error log if [`Err`]. The error message will be added to the `message` argument.
    fn log_err(&self, message: &str)
    where Self::Error: Display {
        self.log_err_fmt(format_args!("{message}"))
    }
}

impl<T, E> ResultOps for Result<T, E> {
    type Item = T;
    type Error = E;

    fn handle_err<F>(self, f: F) -> Option<Self::Item>
    where F: FnOnce(Self::Error) {
        self.map_err(f).ok()
    }

    fn log_err_fmt(&self, fmt: fmt::Arguments)
    where Self::Error: Display {
        if let Err(err) = self {
            crate::error!("{} {}", fmt, err)
        }
    }
}



// ========================
// === ResultUnwrapBoth ===
// ========================

pub trait ResultUnwrapBoth {
    type Item;

    /// Unwrap either `Ok` or `Err`. Possible only if both have the same type
    fn unwrap_both(self) -> Self::Item;
}

impl<T> ResultUnwrapBoth for Result<T, T> {
    type Item = T;

    fn unwrap_both(self) -> Self::Item {
        match self {
            Ok(t) => t,
            Err(t) => t,
        }
    }
}
