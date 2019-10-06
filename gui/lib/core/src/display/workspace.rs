use crate::prelude::*;

use crate::backend::webgl;
use crate::dirty::Dirty;
use crate::system::web;
use crate::system::web::fmt;
use crate::system::web::resize_observer::ResizeObserver;
use crate::system::web::Logger;
use wasm_bindgen::prelude::Closure;
use web_sys::WebGlRenderingContext;

// =============
// === Error ===
// =============

#[derive(Debug, Fail, From)]
pub enum Error {
    #[fail(display = "Web Platform error: {}", error)]
    WebError { error: web::Error },
}

// =============
// === Types ===
// =============

pub type ID = usize;

// =================
// === Workspace ===
// =================

#[derive(Debug, Shrinkwrap)]
pub struct Workspace {
    #[shrinkwrap(main_field)]
    pub data: Rc<WorkspaceData>,
    pub listeners: Listeners,
}

#[derive(Debug)]
pub struct Listeners {
    resize: ResizeObserver,
}

impl Workspace {
    pub fn new(id: ID, dom: &str, sup_logger: &Logger, sup_dirty: &Dirty) -> Result<Self, Error> {
        let data = Rc::new(WorkspaceData::new(id, dom, sup_logger, sup_dirty)?);
        let listeners = Self::new_listeners(&data);
        data.logger.trace("Initialized.");
        Ok(Self { data, listeners })
    }

    pub fn new_listeners(data: &Rc<WorkspaceData>) -> Listeners {
        let data_weak = Rc::downgrade(&data);
        let resize = Closure::new(move |width, height| {
            data_weak.upgrade().map(|data| data.resize(width, height));
        });
        let resize = ResizeObserver::new(&data.canvas, resize);
        Listeners { resize }
    }

    pub fn refresh(&self) {
        if self.dirty.is_set() {
            self.logger.group("Refresh.", || {
                self.dirty.unset();
                let vert_shader = webgl::compile_shader(
                    &self.context,
                    webgl::Context::VERTEX_SHADER,
                    r#"
        attribute vec4 position;
        void main() {
            gl_Position = position;
        }
    "#,
                )
                .unwrap();
                let frag_shader = webgl::compile_shader(
                    &self.context,
                    webgl::Context::FRAGMENT_SHADER,
                    r#"
        void main() {
            gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
        }
    "#,
                )
                .unwrap();
                let program =
                    webgl::link_program(&self.context, &vert_shader, &frag_shader).unwrap();
                self.context.use_program(Some(&program));

                let vertices: [f32; 9] = [-1.0, -1.0, 0.0, 1.0, -1.0, 0.0, 0.0, 1.0, 0.0];

                let buffer = self.context.create_buffer().ok_or("failed to create buffer").unwrap();
                self.context.bind_buffer(webgl::Context::ARRAY_BUFFER, Some(&buffer));

                // Note that `Float32Array::view` is somewhat dangerous (hence the
                // `unsafe`!). This is creating a raw view into our module's
                // `WebAssembly.Memory` buffer, but if we allocate more pages for ourself
                // (aka do a memory allocation in Rust) it'll cause the buffer to change,
                // causing the `Float32Array` to be invalid.
                //
                // As a result, after `Float32Array::view` we have to be very careful not to
                // do any memory allocations before it's dropped.
                unsafe {
                    let vert_array = js_sys::Float32Array::view(&vertices);

                    self.context.buffer_data_with_array_buffer_view(
                        webgl::Context::ARRAY_BUFFER,
                        &vert_array,
                        webgl::Context::STATIC_DRAW,
                    );
                }

                self.context.vertex_attrib_pointer_with_i32(
                    0,
                    3,
                    webgl::Context::FLOAT,
                    false,
                    0,
                    0,
                );
                self.context.enable_vertex_attrib_array(0);

                self.context.clear_color(0.0, 0.0, 0.0, 1.0);
                self.context.clear(webgl::Context::COLOR_BUFFER_BIT);

                self.context.draw_arrays(webgl::Context::TRIANGLES, 0, (vertices.len() / 3) as i32);
            });
        }
    }
}

// =====================
// === WorkspaceData ===
// =====================

#[derive(Debug)]
pub struct WorkspaceData {
    pub id:      ID,
    pub canvas:  web_sys::HtmlCanvasElement,
    pub context: WebGlRenderingContext,
    pub logger:  Logger,
    pub dirty:   Dirty,
}

impl WorkspaceData {
    pub fn new(id: ID, dom: &str, sup_logger: &Logger, sup_dirty: &Dirty) -> Result<Self, Error> {
        let logger = sup_logger.sub(id.to_string());
        logger.trace("Initializing.");
        let canvas = web::get_canvas(dom)?;
        let context = web::get_webgl_context(&canvas, 1)?;
        let dirty = sup_dirty.new_child(&logger);
        Ok(Self { id, canvas, context, logger, dirty })
    }

    pub fn resize(&self, width: i32, height: i32) {
        self.logger.group(fmt!("Resized to {}px x {}px.", width, height), || {
            self.canvas.set_attribute("width", &width.to_string()).unwrap();
            self.canvas.set_attribute("height", &height.to_string()).unwrap();
            self.context.viewport(0, 0, width, height);
            self.dirty.set();
        });
    }
}
