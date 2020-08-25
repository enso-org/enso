//! This is a visualization example scene which creates a sinusoidal graph.

use crate::graph_editor::component::visualization::Data;
use crate::graph_editor::component::visualization;
use crate::graph_editor::component::visualization::Registry;
use crate::graph_editor::data;

use ensogl::application::Application;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::system::web;
use ensogl_text_msdf_sys::run_once_initialized;
use js_sys::Math::sin;
use nalgebra::Vector2;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

fn generate_data(seconds:f64) -> Vec<Vector2<f32>> {
    let mut data = Vec::new();
    for x in 0..100 {
        let x = x as f64 / 50.0 - 1.0;
        let y = sin(x * std::f64::consts::PI + seconds);
        data.push(Vector2::new(x as f32,y as f32));
    }
    data
}



fn constructor_graph() -> visualization::java_script::Definition {
    let source = r#"
        class Graph {
            static inputTypes = ["[[Float,Float,Float]]"]

            onDataReceived(root, data) {
                if (!root.canvas) {
                    root.canvas  = document.createElement("canvas");
                    root.context = root.canvas.getContext("2d");
                    root.appendChild(root.canvas);
                }

                let first = data.shift();
                if (first) {
                    root.context.clearRect(0,0,root.canvas.width,root.canvas.height);
                    root.context.save();
                    root.context.scale(root.canvas.width/2,root.canvas.height/2);
                    root.context.translate(1,1);
                    root.context.lineWidth = 1/Math.min(root.canvas.width,root.canvas.height);
                    root.context.beginPath();
                    root.context.moveTo(first[0],first[1]);
                    data.forEach(data => {
                        root.context.lineTo(data[0],data[1]);
                    });
                    root.context.stroke();
                    root.context.restore();
                    root.context.beginPath();
                    root.context.moveTo(first[0],first[1]);
                    root.context.stroke();
                }
            }

            setSize(root, size) {
                if (root.canvas) {
                    root.canvas.width  = size[0];
                    root.canvas.height = size[1];
                }
            }
        }

        return Graph;
    "#;
    visualization::java_script::Definition::new(data::builtin_library(),source).unwrap() // FIXME unwrap
}

#[wasm_bindgen]
#[allow(dead_code,missing_docs)]
pub fn entry_point_visualization() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        std::mem::forget(app);
    });
}

fn init(app:&Application) {
    let world     = &app.display;
    let scene     = world.scene();
    let camera    = scene.camera();
    let navigator = Navigator::new(&scene,&camera);
    let registry  = Registry::with_default_visualizations();

    registry.add(constructor_graph());

    let vis_factories = registry.valid_sources(&"[[Float,Float,Float]]".into());
    let vis_class     = vis_factories.iter().find(|class| {
        &*class.signature.name == "Graph"
    }).expect("Couldn't find Graph class.");
    let visualization = vis_class.new_instance(&scene).expect("Couldn't create visualiser.");

    let mut was_rendered = false;
    let mut loader_hidden = false;
    world.on_frame(move |time_info| {
        let _keep_alive = &navigator;

        let data    = generate_data((time_info.local / 1000.0).into());
        let data    = Rc::new(data);
        let content = serde_json::to_value(data).unwrap();
        let data    = Data::from(content);

        visualization.send_data.emit(data);

        // Temporary code removing the web-loader instance.
        // To be changed in the future.
        if was_rendered && !loader_hidden {
            web::get_element_by_id("loader").map(|t| {
                t.parent_node().map(|p| {
                    p.remove_child(&t).unwrap()
                })
            }).ok();
            loader_hidden = true;
        }
        was_rendered = true;
    }).forget();
}
