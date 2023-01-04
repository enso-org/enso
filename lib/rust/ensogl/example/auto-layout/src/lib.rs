//! Example scene showing the usage of display object auto-layout.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// FIXME: remove
#![feature(local_key_cell_methods)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;


use naga;
use naga::back::glsl as glsl_out;
use naga::back::spv as spv_out;
use naga::front::glsl::Options;
use naga::front::glsl::Parser;
use naga::ShaderStage;
use rspirv::binary::Disassemble;
use wasm_bindgen::JsCast;


// ==============
// === Shapes ===
// ==============

mod rectangle {
    use super::*;
    ensogl_core::shape2! {
        (style: Style, color: Vector4<f32>) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let rect = Rect((&width, &height)).corners_radius(10.0.px());
            let shape = rect.fill(color::Rgba(0.0,0.0,0.0,0.2));
            shape.into()
        }
    }
}

// #[wasm_bindgen(module = "/spirv-tools.js")]
// #[wasm_bindgen(inline_js = "exports.Module = Module")]
// extern "C" {
//     pub fn test();
//     #[wasm_bindgen(js_namespace = Module)]
//     pub fn optimize_me(t: JsValue) -> JsValue;
// }


// ===================
// === Entry Point ===
// ===================

#[before_main]
pub fn test() {
    println!("test");
}

/// The example entry point.
/// foo bar baz
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    //     warn!("{:?}", rectangle::Shape::definition_path());
    //     let glsl = r#"
    //         #version 450 core
    //
    // void main () {}
    //     "#;
    //
    //     let mut parser = Parser::default();
    //     let options = Options::from(ShaderStage::Vertex);
    //     let module = parser.parse(&options, glsl).unwrap();
    //     warn!("{:#?}", module);
    //
    //     let capabilities = naga::valid::Capabilities::all();
    //     let info = naga::valid::Validator::new(naga::valid::ValidationFlags::empty(),
    // capabilities)         .validate(&module)
    //         .expect("Naga module validation failed");
    //
    //     let mut spv_vec =
    //         spv_out::write_vec(&module, &info, &spv_out::Options::default(), None).unwrap();
    //     // let dis = rspirv::dr::load_words(spv_vec).expect("Produced invalid
    // SPIR-V").disassemble();     // let spv_vec_js = js_sys::Uint32Array::from(&spv_vec[..]);
    //     // let spv_vec_js = JsValue::from(spv_vec_js);
    //     // warn!("{}", dis);
    //
    //     let spv_vec8 = unsafe {
    //         let ratio = mem::size_of::<u32>() / mem::size_of::<u8>();
    //
    //         let length = spv_vec.len() * ratio;
    //         let capacity = spv_vec.capacity() * ratio;
    //         let ptr = spv_vec.as_mut_ptr() as *mut u8;
    //
    //         // Don't run the destructor for vec32
    //         mem::forget(spv_vec);
    //
    //         // Construct new Vec
    //         Vec::from_raw_parts(ptr, length, capacity)
    //     };
    //
    //     let v: Vec<u8> = vec![];
    //     let spv_vec_js = js_sys::Uint8Array::from(&spv_vec8[..]);
    //     let spv_vec_js = JsValue::from(spv_vec_js);
    //
    //     let out = optimize_me(spv_vec_js);
    //     let out = out.unchecked_into::<js_sys::Uint32Array>();
    //     let out = out.to_vec();
    //     // warn!("OUT: {:?}", out);
    //     let dis = rspirv::dr::load_words(out).expect("Produced invalid SPIR-V").disassemble();
    //     // let spv_vec_js = js_sys::Uint32Array::from(&spv_vec[..]);
    //     // let spv_vec_js = JsValue::from(spv_vec_js);
    //     warn!("DIS: {}", dis);
    //
    //     let mut glsl_out = String::new();
    //     let glsl_out_opts = glsl_out::Options::default();
    //     let glsl_out_pipeline_opts = glsl_out::PipelineOptions {
    //         shader_stage: naga::ShaderStage::Vertex,
    //         entry_point:  "main".to_string(),
    //         multiview:    None,
    //     };
    //     let mut glsl_writer = glsl_out::Writer::new(
    //         &mut glsl_out,
    //         &module,
    //         &info,
    //         &glsl_out_opts,
    //         &glsl_out_pipeline_opts,
    //         naga::proc::index::BoundsCheckPolicies::default(),
    //     )
    //     .unwrap();
    //     glsl_writer.write().unwrap();
    //
    //     warn!("GLSL: {}", glsl_out);

    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let rect1 = rectangle::View::new();
    rect1.set_size(Vector2::new(100.0, 100.0));
    rect1.color.set(color::Rgba::new(0.5, 0.0, 0.0, 0.3).into());

    let rect2 = rectangle::View::new();
    rect2.set_size(Vector2::new(100.0, 100.0));
    rect2.color.set(color::Rgba::new(0.5, 0.0, 0.0, 0.3).into());

    let root = display::object::Instance::new();
    root.set_size(Vector2::new(300.0, 100.0));
    root.use_auto_layout();
    root.add_child(&rect1);
    root.add_child(&rect2);
    world.add_child(&root);

    warn!("rect1: {:?}", rect1.display_object());

    // scene::precompile_shaders();

    let r = rect1.clone_ref();
    let mut i = 0;
    world
        .on
        .before_frame
        .add(move |_| {
            if i == 10 {
                warn!("rect1: {:?}", r.display_object());
                // warn!("rect1 sprite: {:?}", r.sprite.borrow().display_object());
            }
            i += 1;
        })
        .forget();

    world.keep_alive_forever();
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(rect1);
    mem::forget(rect2);
    warn!("Hello World!");
}
