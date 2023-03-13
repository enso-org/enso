// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_prelude::*;
use wasm_bindgen::prelude::*;

use enso_frp as frp;
use enso_logger::AnyLogger;
use enso_logger::WarningLogger as Logger;
use enso_shortcuts as shortcuts;
use enso_shortcuts::Registry;
use enso_web as web;
use frp::io::keyboard;
use frp::io::keyboard::Keyboard;



#[entry_point(shortcuts)]
#[allow(dead_code)]
pub fn main() {
    let shortcut_registry = shortcuts::AutomataRegistry::<String>::new();
    shortcut_registry.add(shortcuts::Press, "ctrl + a", "hello");
    shortcut_registry.add(shortcuts::Press, "ctrl + b", "hello");

    debug!(shortcut_registry.nfa_as_graphviz_code());

    let logger: Logger = Logger::new("kb");
    let kb = Keyboard::new();
    let bindings = keyboard::DomBindings::new(&logger, &kb, &default());

    frp::new_network! { network
        eval kb.down ((t)shortcut_registry.on_press(t.simple_name()));
        eval kb.up   ((t)shortcut_registry.on_release(t.simple_name()));
    }
    mem::forget(network);
    mem::forget(bindings);
    mem::forget(shortcut_registry);

    let shortcut_registry = shortcuts::HashSetRegistry::<String>::new();
    shortcut_registry.add(shortcuts::Press, "ctrl a", "press ctrl a");
    shortcut_registry.add(shortcuts::Release, "ctrl a", "release ctrl a");
    shortcut_registry.add(shortcuts::Press, "a", "press a");
    shortcut_registry.add(shortcuts::Release, "a", "release a");
    debug!("\n---------------");
    debug!("-> " shortcut_registry.on_press("ctrl-left");?);
    debug!("---");
    debug!("-> " shortcut_registry.on_press("a");?);
    debug!("---");
    web::simulate_sleep(1000.0);
    debug!("-> " shortcut_registry.on_release("ctrl-left");?);
    debug!("---");
    debug!("-> " shortcut_registry.on_release("a");?);
}
