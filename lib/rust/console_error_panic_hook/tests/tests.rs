extern crate enso_console_error_panic_hook;

use std::fs::File;
use std::io::Read;
use std::panic;
use std::process::Command;

#[test]
fn can_set_as_hook() {
    panic::set_hook(Box::new(enso_console_error_panic_hook::hook));
}

#[test]
fn can_set_once() {
    for _ in 0..10 {
        enso_console_error_panic_hook::set_once();
    }
}
