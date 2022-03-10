//! This is a visualization example scene which creates a sinusoidal graph.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl::animation;
use ensogl::application::Application;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::system::web;
use ensogl_text_msdf_sys::run_once_initialized;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::component::visualization::Data;
use ide_view::graph_editor::component::visualization::Registry;
use js_sys::Math::sin;
use nalgebra::Vector2;



fn generate_data(seconds: f64) -> Vec<Vector2<f32>> {
    let mut data = Vec::new();
    for x in 0..100 {
        let x = x as f64 / 50.0 - 1.0;
        let y = sin(x * std::f64::consts::PI + seconds);
        data.push(Vector2::new(x as f32, y as f32));
    }
    data
}



fn constructor_graph() -> visualization::java_script::Definition {
    let source = r#"
        class Graph extends Visualization {
            static inputType = "[[Float,Float,Float]]"

            onDataReceived(data) {
                if (!this.canvas) {
                    this.canvas  = document.createElement("canvas");
                    this.canvas.setAttribute("tabindex","0");
                    this.context = this.canvas.getContext("2d");
                    this.dom.appendChild(this.canvas);
                    this.dom.addEventListener("keydown", function(e) {
                        console.log("pressed",e);
                    })
                }
                this.setPreprocessor(`x ->\n IO.println "Preprocessor set after receiving ${data}`)

                let first = data.shift();
                if (first) {
                    this.context.clearRect(0,0,this.canvas.width,this.canvas.height);
                    this.context.save();
                    this.context.scale(this.canvas.width/2,this.canvas.height/2);
                    this.context.translate(1,1);
                    this.context.lineWidth = 1/Math.min(this.canvas.width,this.canvas.height);
                    this.context.beginPath();
                    this.context.moveTo(first[0],first[1]);
                    data.forEach(data => {
                        this.context.lineTo(data[0],data[1]);
                    });
                    this.context.stroke();
                    this.context.restore();
                    this.context.beginPath();
                    this.context.moveTo(first[0],first[1]);
                    this.context.stroke();
                }
            }

            setSize(size) {
                if (this.canvas) {
                    this.canvas.width  = size[0];
                    this.canvas.height = size[1];
                }
            }
        }

        return Graph
    "#;
    let mut sources = visualization::java_script::Sources::empty();
    sources.add_file("demo.js", source);
    visualization::java_script::Definition::new_builtin(sources).unwrap()
}

#[wasm_bindgen]
#[allow(dead_code, missing_docs)]
pub fn entry_point_visualization() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        std::mem::forget(app);
    });
}

fn init(app: &Application) {
    let world = &app.display;
    let scene = &world.default_scene;
    let camera = scene.camera();
    let navigator = Navigator::new(scene, &camera);
    let registry = Registry::new();

    registry.add(constructor_graph());

    let vis_factories = registry.valid_sources(&"[[Float,Float,Float]]".into());
    let vis_class = vis_factories
        .iter()
        .find(|class| &*class.signature.name == "Graph")
        .expect("Couldn't find Graph class.");
    let visualization = vis_class.new_instance(scene).expect("Couldn't create visualiser.");
    visualization.activate.emit(());

    let network = enso_frp::Network::new("VisualizationExample");
    enso_frp::extend! { network
        trace visualization.on_preprocessor_change;
    };
    std::mem::forget(network);

    let mut was_rendered = false;
    let mut loader_hidden = false;
    world
        .on
        .before_frame
        .add(move |time_info: animation::TimeInfo| {
            let _keep_alive = &navigator;

            let data = generate_data((time_info.local / 1000.0).into());
            let data = Rc::new(data);
            let content = serde_json::to_value(data).unwrap();
            let data = Data::from(content);

            visualization.send_data.emit(data);

            // Temporary code removing the web-loader instance.
            // To be changed in the future.
            if was_rendered && !loader_hidden {
                web::document
                    .get_element_by_id("loader")
                    .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()));
                loader_hidden = true;
            }
            was_rendered = true;
        })
        .forget();
}
