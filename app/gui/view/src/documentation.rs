//! Documentation view visualization generating and presenting Enso Documentation under
//! the documented node.

use crate::prelude::*;

use crate::graph_editor::component::visualization;

pub use visualization::container::overlay;

use enso_frp as frp;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl::display::shape::primitive::system::DynamicShape;
use ensogl::display::shape::primitive::StyleWatch;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::system::web::clipboard;
use ensogl::system::web::traits::*;
use ensogl_component::shadow;
use web::Closure;
use web::HtmlElement;
use web::JsCast;
use web::MouseEvent;



// =================
// === Constants ===
// =================

/// Width of Documentation panel.
pub const VIEW_WIDTH: f32 = 300.0;
/// Height of Documentation panel.
pub const VIEW_HEIGHT: f32 = 300.0;

/// Content in the documentation view when there is no data available.
const CORNER_RADIUS: f32 = crate::graph_editor::component::node::CORNER_RADIUS;
const PADDING: f32 = 15.0;
const PADDING_TOP: f32 = 5.0;
const CODE_BLOCK_CLASS: &str = "doc-code-container";
const COPY_BUTTON_CLASS: &str = "doc-copy-btn";



// =============
// === Model ===
// =============

type CodeCopyClosure = Closure<dyn FnMut(MouseEvent)>;

/// Model of Native visualization that generates documentation for given Enso code and embeds
/// it in a HTML container.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Model {
    logger:             Logger,
    outer_dom:          DomSymbol,
    inner_dom:          DomSymbol,
    size:               Rc<Cell<Vector2>>,
    /// The purpose of this overlay is stop propagating mouse events under the documentation panel
    /// to EnsoGL shapes, and pass them to the DOM instead.
    overlay:            overlay::View,
    display_object:     display::object::Instance,
    code_copy_closures: Rc<CloneCell<Vec<CodeCopyClosure>>>,
}

impl Model {
    /// Constructor.
    fn new(scene: &Scene) -> Self {
        let logger = Logger::new("DocumentationView");
        let display_object = display::object::Instance::new(&logger);
        let outer_div = web::document.create_div_or_panic();
        let outer_dom = DomSymbol::new(&outer_div);
        let inner_div = web::document.create_div_or_panic();
        let inner_dom = DomSymbol::new(&inner_div);
        let size =
            Rc::new(Cell::new(Vector2(VIEW_WIDTH - PADDING, VIEW_HEIGHT - PADDING - PADDING_TOP)));
        let overlay = overlay::View::new(&logger);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let style_path = ensogl_hardcoded_theme::application::documentation::background;
        let bg_color = styles.get_color(style_path);
        let bg_color = bg_color.to_javascript_string();

        outer_dom.dom().set_style_or_warn("white-space", "normal");
        outer_dom.dom().set_style_or_warn("overflow-y", "auto");
        outer_dom.dom().set_style_or_warn("overflow-x", "auto");
        outer_dom.dom().set_style_or_warn("background-color", bg_color);
        outer_dom.dom().set_style_or_warn("pointer-events", "auto");
        outer_dom.dom().set_style_or_warn("border-radius", format!("{}px", CORNER_RADIUS));
        shadow::add_to_dom_element(&outer_dom, &styles);

        inner_dom.dom().set_attribute_or_warn("class", "scrollable");
        inner_dom.dom().set_style_or_warn("white-space", "normal");
        inner_dom.dom().set_style_or_warn("overflow-y", "auto");
        inner_dom.dom().set_style_or_warn("overflow-x", "auto");
        inner_dom.dom().set_style_or_warn("padding", format!("{}px", PADDING));
        inner_dom.dom().set_style_or_warn("padding-top", "5px");
        inner_dom.dom().set_style_or_warn("pointer-events", "auto");
        inner_dom.dom().set_style_or_warn("border-radius", format!("{}px", CORNER_RADIUS));

        overlay.roundness.set(1.0);
        overlay.radius.set(CORNER_RADIUS);
        display_object.add_child(&outer_dom);
        outer_dom.add_child(&inner_dom);
        display_object.add_child(&overlay);
        scene.dom.layers.front.manage(&outer_dom);
        scene.dom.layers.front.manage(&inner_dom);

        let code_copy_closures = default();
        Model { logger, outer_dom, inner_dom, size, overlay, display_object, code_copy_closures }
            .init()
    }

    fn init(self) -> Self {
        self.reload_style();
        self
    }

    /// Set size of the documentation view.
    fn set_size(&self, size: Vector2) {
        self.size.set(size);
        self.overlay.size.set(size);
        self.reload_style();
    }

    /// Create a container for generated content and embed it with stylesheet.
    fn push_to_dom(&self, content: String) {
        let no_doc_txt = "<p style=\"color: #a3a6a9;\">No documentation available</p>";
        // FIXME [MM] : Temporary solution until engine update with changed class name in docs
        // parser.
        let content = content
            .replace("<p>No documentation available</p>", no_doc_txt)
            .replace("class=\"doc\"", "class=\"enso docs\"")
            .replace("\"font-size: 13px;\"", "\"font-size: 15px;\"")
            .replace("ALIAS", "alias");
        self.inner_dom.dom().set_inner_html(&content);
    }

    /// Append listeners to copy buttons in doc to enable copying examples.
    /// It is possible to do it with implemented method, because get_elements_by_class_name
    /// returns top-to-bottom sorted list of elements, as found in:
    /// https://stackoverflow.com/questions/35525811/order-of-elements-in-document-getelementsbyclassname-array
    fn attach_listeners_to_copy_buttons(&self) {
        let code_blocks = self.inner_dom.dom().get_elements_by_class_name(CODE_BLOCK_CLASS);
        let copy_buttons = self.inner_dom.dom().get_elements_by_class_name(COPY_BUTTON_CLASS);
        let closures = (0..copy_buttons.length()).map(|i| -> Result<CodeCopyClosure, u32> {
            let create_closures = || -> Option<CodeCopyClosure> {
                let copy_button = copy_buttons.get_with_index(i)?.dyn_into::<HtmlElement>().ok()?;
                let code_block = code_blocks.get_with_index(i)?.dyn_into::<HtmlElement>().ok()?;
                let closure: Closure<dyn FnMut(MouseEvent)> = Closure::new(move |_: MouseEvent| {
                    let inner_code = code_block.inner_text();
                    clipboard::write_text(inner_code);
                });
                let callback = closure.as_js_function();
                match copy_button.add_event_listener_with_callback("click", callback) {
                    Ok(_) => Some(closure),
                    Err(e) => {
                        error!(&self.logger, "Unable to add event listener to copy button: {e:?}");
                        None
                    }
                }
            };
            create_closures().ok_or(i)
        });
        let (closures, errors): (Vec<_>, Vec<_>) = closures.partition(Result::is_ok);
        let ok_closures = closures.into_iter().filter_map(|t| t.ok()).collect_vec();
        let err_indices = errors.into_iter().filter_map(|t| t.err()).collect_vec();
        if !err_indices.is_empty() {
            error!(
                &self.logger,
                "Failed to attach listeners to copy buttons with indices: {err_indices:?}."
            )
        }
        self.code_copy_closures.set(ok_closures)
    }

    /// Receive data, process and present it in the documentation view.
    fn receive_data(&self, data: &visualization::Data) -> Result<(), visualization::DataError> {
        let string = match data {
            visualization::Data::Json { content } => match serde_json::to_string_pretty(&**content)
            {
                Ok(string) => string,
                Err(err) => {
                    error!(
                        self.logger,
                        "Error during documentation vis-data serialization: \
                        {err:?}"
                    );
                    return Err(visualization::DataError::InternalComputationError);
                }
            },
            visualization::Data::Binary =>
                return Err(visualization::DataError::BinaryNotSupported),
        };
        self.display_doc(&string);
        Ok(())
    }

    /// Displays the received data in the panel.
    fn display_doc(&self, content: &str) {
        self.push_to_dom(String::from(content));
        self.attach_listeners_to_copy_buttons();
    }

    /// Load an HTML file into the documentation view when user is waiting for data to be received.
    /// TODO [MM] : This should be replaced with a EnsoGL spinner in the next PR.
    fn load_waiting_screen(&self) {
        let spinner = include_str!("documentation/spinner.html");
        self.push_to_dom(String::from(spinner))
    }

    fn reload_style(&self) {
        let size = self.size.get();
        let padding = (size.x.min(size.y) / 2.0).min(PADDING);
        self.outer_dom.set_size(Vector2(size.x, size.y));
        self.inner_dom.set_size(Vector2(size.x - padding, size.y - padding - PADDING_TOP));
        self.inner_dom.dom().set_style_or_warn("padding", format!("{}px", padding));
        self.inner_dom.dom().set_style_or_warn("padding-top", format!("{}px", PADDING_TOP));
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Display documentation of the entity represented by given code.
        display_documentation (String)
    }
    Output {
        /// Indicates whether the documentation panel has been selected through clicking into
        /// it, or deselected by clicking somewhere else.
        is_selected(bool),
    }
}


// ============
// === View ===
// ============

/// View of the visualization that renders the given documentation as a HTML page.
///
/// The documentation can be provided in two formats: it can be code of the entity (type, method,
/// function etc) with doc comments, or the docstring only - in the latter case
/// however we're unable to summarize methods and atoms of types.
///
/// The default format is the docstring.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
#[allow(missing_docs)]
pub struct View {
    #[shrinkwrap(main_field)]
    pub model:             Model,
    pub visualization_frp: visualization::instance::Frp,
    pub frp:               Frp,
}

impl View {
    /// Definition of this visualization.
    pub fn definition() -> visualization::Definition {
        let path = visualization::Path::builtin("Documentation View");
        visualization::Definition::new(
            visualization::Signature::new_for_any_type(path, visualization::Format::Json),
            |scene| Ok(Self::new(scene).into()),
        )
    }

    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        let frp = Frp::new();
        let visualization_frp = visualization::instance::Frp::new(&frp.network);
        let model = Model::new(scene);
        model.load_waiting_screen();
        Self { model, visualization_frp, frp }.init(scene)
    }

    fn init(self, scene: &Scene) -> Self {
        let network = &self.frp.network;
        let model = &self.model;
        let visualization = &self.visualization_frp;
        let frp = &self.frp;
        frp::extend! { network

            // === Displaying documentation ===

            eval frp.display_documentation ((cont) model.display_doc(cont));
            eval visualization.send_data([visualization,model](data) {
                if let Err(error) = model.receive_data(data) {
                    visualization.data_receive_error.emit(error)
                }
            });


            // === Size and position ===

            eval visualization.set_size  ((size) model.set_size(*size));


            // === Activation ===

            mouse_down_target <- scene.mouse.frp.down.map(f_!(scene.mouse.target.get()));
            selected <- mouse_down_target.map(f!([model,visualization] (target){
                if !model.overlay.is_this_target(*target) {
                    visualization.deactivate.emit(());
                    false
                } else {
                    visualization.activate.emit(());
                    true
                }
            }));
            is_selected_changed <= selected.map2(&frp.output.is_selected, |&new,&old| {
                (new != old).as_some(new)
            });
            frp.source.is_selected <+ is_selected_changed;
        }
        visualization.pass_events_to_dom_if_active(scene, network);
        self
    }
}

impl From<View> for visualization::Instance {
    fn from(t: View) -> Self {
        Self::new(&t, &t.visualization_frp, &t.frp.network, Some(t.model.outer_dom.clone_ref()))
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
