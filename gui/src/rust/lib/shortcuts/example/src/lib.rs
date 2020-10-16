use enso_prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_system_web as web;

use enso_shortcuts as shortcuts;
use enso_shortcuts::Registry;

use enso_frp as frp;

use frp::io::keyboard::Keyboard;
use frp::io::keyboard as keyboard;

use enso_logger::AnyLogger;
use enso_logger::disabled::Logger;

#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_shortcuts() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    web::set_stack_trace_limit();
    main();
}

pub fn main() {
    let shortcut_registry = shortcuts::AutomataRegistry::<String>::new();
    shortcut_registry.add(shortcuts::Press, "ctrl + a", "hello");
    shortcut_registry.add(shortcuts::Press, "ctrl + b", "hello");

    println!("{}",shortcut_registry.nfa_as_graphviz_code());

    let logger   = Logger::new("kb");
    let kb       = Keyboard::new();
    let bindings = keyboard::DomBindings::new(&logger,&kb,&default());

    let shortcut_registry2 = shortcut_registry.clone_ref();
    let shortcut_registry3 = shortcut_registry.clone_ref();
    frp::new_network! { network
        on_down <- kb.down.map (move |t| shortcut_registry2.on_press(t.simple_name()));
        on_up   <- kb.up.map   (move |t| shortcut_registry3.on_release(t.simple_name()));
        trace on_down;
        trace on_up;
    }
    mem::forget(network);
    mem::forget(bindings);
    mem::forget(shortcut_registry);


    let shortcut_registry = shortcuts::HashSetRegistry::<String>::new();
    shortcut_registry.add(shortcuts::Press, "ctrl a", "press ctrl a");
    shortcut_registry.add(shortcuts::Release, "ctrl a", "release ctrl a");
    shortcut_registry.add(shortcuts::Press, "a", "press a");
    shortcut_registry.add(shortcuts::Release, "a", "release a");
    println!("\n---------------");
    println!("-> {:?}", shortcut_registry.on_press("ctrl-left"));
    println!("---");
    println!("-> {:?}", shortcut_registry.on_press("a"));
    println!("---");
    web::simulate_sleep(1000.0);
    println!("-> {:?}", shortcut_registry.on_release("ctrl-left"));
    println!("---");
    println!("-> {:?}", shortcut_registry.on_release("a"));
}
