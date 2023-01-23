//! Example visualisation showing the provided data as text.

use crate::component::visualization::*;
use crate::prelude::*;
use ensogl::system::web::traits::*;

use crate::component::visualization;
use crate::SharedHashMap;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl::display::shape::primitive::StyleWatch;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl_hardcoded_theme;
use serde::Deserialize;
use serde::Serialize;


// ==============
// === Export ===
// ==============

pub use crate::component::node::error::Kind;



// =================
// === Constants ===
// =================

const PADDING_TEXT: f32 = 10.0;

/// The module containing the `PREPROCESSOR_FUNCTION`. See there.
// NOTE: contents of this const need to be kept in sync with Scala test in
// RuntimeVisualisationsTest.scala, used to verify the snippet's correctness
const PREPROCESSOR_MODULE: &str = "Standard.Visualization.Preprocessor";

/// The method name of the error preprocessor.
// NOTE: contents of this const need to be kept in sync with Scala test in
// RuntimeVisualisationsTest.scala, used to verify the snippet's correctness
const PREPROCESSOR_METHOD: &str = "error_preprocessor";

/// The list of arguments passed to the error preprocessor.
const PREPROCESSOR_ARGUMENTS: Vec<String> = vec![];

/// Get preprocessor configuration for error visualization.
pub fn preprocessor() -> instance::PreprocessorConfiguration {
    instance::PreprocessorConfiguration::new(
        PREPROCESSOR_MODULE,
        PREPROCESSOR_METHOD,
        PREPROCESSOR_ARGUMENTS,
    )
}

/// Get metadata description for error visualization.
pub fn metadata() -> Metadata {
    let preprocessor = preprocessor();
    Metadata { preprocessor }
}

// =============
// === Input ===
// =============

/// The input for Error Visualization.
#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Input {
    pub kind:    Option<Kind>,
    pub message: String,
}



// =============
// === Error ===
// =============

/// Sample visualization that renders the given data as text. Useful for debugging and testing.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Error {
    pub frp: visualization::instance::Frp,
    model:   Model,
    network: frp::Network,
}

impl Deref for Error {
    type Target = visualization::instance::FrpInputs;

    fn deref(&self) -> &Self::Target {
        &self.frp.inputs
    }
}

impl Error {
    /// The visualization path.
    pub fn path() -> Path {
        Path::builtin("Error")
    }

    /// Definition of this visualization.
    pub fn definition() -> Definition {
        let path = Self::path();
        Definition::new(Signature::new_for_any_type(path, Format::Json), |app| {
            Ok(Self::new(app).into())
        })
    }

    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let network = frp::Network::new("js_visualization_raw_text");
        let frp = visualization::instance::Frp::new(&network);
        let model = Model::new(scene.clone_ref());
        Self { frp, model, network }.init()
    }

    fn init(self) -> Self {
        let network = &self.network;
        let model = self.model.clone_ref();
        let frp = self.frp.clone_ref();
        frp::extend! { network
            eval frp.set_size ((size) model.set_size(*size));
            eval frp.send_data ([frp,model](data) {
                if let Err(e) = model.receive_data(data) {
                    frp.data_receive_error.emit(Some(e));
                }
            });
            eval frp.set_layer ((layer) model.set_layer(*layer));
        }

        frp.preprocessor_change.emit(preprocessor());
        self
    }

    /// Sets the visualization data directly from the [`Input`] structure (not from the serialized
    /// JSON).
    pub fn set_data(&self, input: &Input) {
        self.model.set_data(input);
    }

    /// Switch displayed error kind.
    ///
    /// The visualization keep the messages received for each kind, because they may be not
    /// synchronized with expression update payload informing us which kind we ought to display.
    pub fn display_kind(&self, new: Kind) {
        self.model.display_kind(new);
    }
}

#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Model {
    logger:    Logger,
    dom:       DomSymbol,
    size:      Rc<Cell<Vector2>>,
    // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
    // system (#795)
    styles:    StyleWatch,
    // Because the payloads (with panic messages) and visualization updates (with dataflow error
    // messages) are not synchronized, we need to keep both versions, always ready to switch them
    // when payload changes.
    displayed: Rc<CloneCell<Kind>>,
    messages:  SharedHashMap<Kind, ImString>,
    scene:     Scene,
}

impl Model {
    /// Constructor.
    fn new(scene: Scene) -> Self {
        let logger = Logger::new("RawText");
        let div = web::document.create_div_or_panic();
        let dom = DomSymbol::new(&div);
        let size = Rc::new(Cell::new(Vector2(200.0, 200.0)));
        let displayed = Rc::new(CloneCell::new(Kind::Panic));
        let messages = default();

        let styles = StyleWatch::new(&scene.style_sheet);
        let padding_text = format!("{}px", PADDING_TEXT);

        dom.dom().set_attribute_or_warn("class", "visualization scrollable");
        dom.dom().set_style_or_warn("overflow-x", "hidden");
        dom.dom().set_style_or_warn("overflow-y", "auto");
        dom.dom().set_style_or_warn("font-family", "DejaVuSansMonoBook");
        dom.dom().set_style_or_warn("font-size", "12px");
        dom.dom().set_style_or_warn("border-radius", "14px");
        dom.dom().set_style_or_warn("padding-left", &padding_text);
        dom.dom().set_style_or_warn("padding-top", &padding_text);
        dom.dom().set_style_or_warn("pointer-events", "auto");

        scene.dom.layers.back.manage(&dom);
        Model { logger, dom, size, styles, displayed, messages, scene }.init()
    }

    fn init(self) -> Self {
        self.reload_style();
        self
    }

    fn set_size(&self, size: Vector2) {
        let x_mod = size.x - PADDING_TEXT;
        let y_mod = size.y - PADDING_TEXT;
        let size = Vector2(x_mod, y_mod);
        self.size.set(size);
        self.reload_style();
    }

    fn receive_data(&self, data: &Data) -> Result<(), DataError> {
        match data {
            Data::Json { content } => {
                let input_result = serde_json::from_value(content.deref().clone());
                let input: Input = input_result.map_err(|_| DataError::InvalidDataType)?;
                self.set_data(&input);
                Ok(())
            }
            Data::Binary => Err(DataError::BinaryNotSupported),
        }
    }

    fn set_data(&self, input: &Input) {
        if let Some(kind) = input.kind {
            self.messages.insert(kind, input.message.clone().into());
            if kind == self.displayed.get() {
                self.dom.dom().set_inner_text(&input.message);
            }
        }
        // else we don't update the text, as the node does not contain error anymore. The
        // visualization will be hidden once we receive expression update message.
    }

    fn display_kind(&self, new: Kind) {
        let color_style = match new {
            Kind::Panic => ensogl_hardcoded_theme::graph_editor::visualization::error::panic::text,
            Kind::Dataflow =>
                ensogl_hardcoded_theme::graph_editor::visualization::error::dataflow::text,
        };
        let default = "";
        let opt_message = self.messages.get_cloned_ref(&new);
        let message = opt_message.as_ref().map_or(default, |s| s.as_str());
        self.dom.dom().set_inner_text(message);
        self.set_text_color(color_style);
        self.displayed.set(new);
    }

    fn reload_style(&self) {
        self.dom.set_dom_size(self.size.get());
    }

    fn set_text_color(&self, color: impl Into<display::style::Path>) {
        let text_color = self.styles.get_color(color);
        let red = text_color.red * 255.0;
        let green = text_color.green * 255.0;
        let blue = text_color.blue * 255.0;
        let text_color = format!("rgba({},{},{},{})", red, green, blue, text_color.alpha);
        self.dom.dom().set_style_or_warn("color", text_color);
    }

    fn set_layer(&self, layer: Layer) {
        layer.apply_for_html_component(&self.scene, &self.dom)
    }
}

impl From<Error> for Instance {
    fn from(t: Error) -> Self {
        Self::new(&t, &t.frp, &t.network, Some(t.model.dom.clone_ref()))
    }
}

impl display::Object for Error {
    fn display_object(&self) -> &display::object::Instance {
        self.model.dom.display_object()
    }
}
