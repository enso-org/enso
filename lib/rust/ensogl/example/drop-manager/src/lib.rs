//! An example of [`deop::Manager`] usage. The dropped files metadata and content is printed to
//! the console.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

use enso_prelude::*;

use ensogl_core::display::world::World;
use ensogl_core::frp::web;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::spawn_local;

fn download_file(file: ensogl_drop_manager::File) {
    spawn_local(async move {
        INFO!("Received file: {file:?}");
        loop {
            match file.read_chunk().await {
                Ok(Some(chunk)) => {
                    INFO!("Received chunk: {chunk:?}");
                }
                Ok(None) => {
                    INFO!("All chunks received successfully");
                    break;
                }
                Err(err) => {
                    ERROR!("Error in receiving chunk promise: {err:?}");
                    break;
                }
            }
        }
    });
}

/// The example entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_drop_manager() {
    web::forward_panic_hook_to_console();

    let world = World::new(&web::get_html_element_by_id("root").unwrap());
    let drop_manager = ensogl_drop_manager::Manager::new(world.scene().dom.root.as_ref());
    let network = enso_frp::Network::new("Debug Scene");
    enso_frp::extend! { network
        let file_received = drop_manager.files_received().clone_ref();
        eval file_received ([](files) for file in files { download_file(file.clone_ref())});
    }

    let mut loader_hidden = false;
    world
        .on_frame(move |_| {
            if !loader_hidden {
                web::get_element_by_id("loader")
                    .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()))
                    .ok();
                loader_hidden = true;
            }
        })
        .forget();

    std::mem::forget(world);
    std::mem::forget(network);
    std::mem::forget(drop_manager);
}
