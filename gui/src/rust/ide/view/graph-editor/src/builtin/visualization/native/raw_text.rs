//! Example visualisation showing the provided data as text.

use crate::prelude::*;

use crate::component::visualization;
use crate::component::visualization::*;

use enso_frp as frp;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl::display::shape::primitive::StyleWatch;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::system::web::AttributeSetter;
use ensogl::system::web::StyleSetter;
use ensogl_theme;



// =================
// === Constants ===
// =================
const PADDING_TEXT: f32 = 10.0;



// ===============
// === RawText ===
// ===============

/// Sample visualization that renders the given data as text. Useful for debugging and testing.
#[derive(Debug, Shrinkwrap)]
#[allow(missing_docs)]
pub struct RawText {
    #[shrinkwrap(main_field)]
    model:   RawTextModel,
    frp:     visualization::instance::Frp,
    network: frp::Network,
}

impl RawText {
    /// Definition of this visualization.
    pub fn definition() -> Definition {
        let path = Path::builtin("JSON");
        Definition::new(Signature::new_for_any_type(path, Format::Json), |scene| {
            Ok(Self::new(scene.clone_ref()).into())
        })
    }

    /// Constructor.
    pub fn new(scene: Scene) -> Self {
        let network = frp::Network::new("js_visualization_raw_text");
        let frp = visualization::instance::Frp::new(&network);
        let model = RawTextModel::new(scene);
        Self { model, frp, network }.init()
    }

    fn init(self) -> Self {
        let network = &self.network;
        let model = self.model.clone_ref();
        let frp = self.frp.clone_ref();
        frp::extend! { network
            eval frp.set_size  ((size) model.set_size(*size));
            eval frp.send_data ([frp,model](data) {
                if let Err(e) = model.receive_data(data) {
                    frp.data_receive_error.emit(Some(e));
                }
             });
            eval frp.set_layer ((layer) model.set_layer(*layer));
        }
        self
    }
}

#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct RawTextModel {
    logger: Logger,
    dom:    DomSymbol,
    size:   Rc<Cell<Vector2>>,
    scene:  Scene,
}

impl RawTextModel {
    /// Constructor.
    fn new(scene: Scene) -> Self {
        let logger = Logger::new("RawText");
        let div = web::create_div();
        let dom = DomSymbol::new(&div);
        let size = Rc::new(Cell::new(Vector2(200.0, 200.0)));

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let text_color = styles.get_color(ensogl_theme::graph_editor::visualization::text);
        let _red = text_color.red * 255.0;
        let _green = text_color.green * 255.0;
        let _blue = text_color.blue * 255.0;
        let text_color = format!("rgba({},{},{},{})", _red, _green, _blue, text_color.alpha);
        let padding_text = format!("{}px", PADDING_TEXT);

        dom.dom().set_attribute_or_warn("class", "visualization scrollable", &logger);
        dom.dom().set_style_or_warn("white-space", "pre", &logger);
        dom.dom().set_style_or_warn("overflow-y", "auto", &logger);
        dom.dom().set_style_or_warn("overflow-x", "auto", &logger);
        dom.dom().set_style_or_warn("font-family", "DejaVuSansMonoBook", &logger);
        dom.dom().set_style_or_warn("font-size", "12px", &logger);
        dom.dom().set_style_or_warn("padding-left", &padding_text, &logger);
        dom.dom().set_style_or_warn("padding-top", &padding_text, &logger);
        dom.dom().set_style_or_warn("color", text_color, &logger);
        dom.dom().set_style_or_warn("pointer-events", "auto", &logger);

        scene.dom.layers.back.manage(&dom);
        RawTextModel { logger, dom, size, scene }.init()
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
        let data_inner = match data {
            Data::Json { content } => content,
            _ => todo!(), // FIXME
        };
        let data_str = serde_json::to_string_pretty(&**data_inner);
        let data_str = data_str.unwrap_or_else(|e| format!("<Cannot render data: {}>", e));
        self.dom.dom().set_inner_text(&data_str);
        Ok(())
    }

    fn reload_style(&self) {
        self.dom.set_size(self.size.get());
    }

    fn set_layer(&self, layer: Layer) {
        layer.apply_for_html_component(&self.scene, &self.dom)
    }
}

impl From<RawText> for Instance {
    fn from(t: RawText) -> Self {
        Self::new(&t, &t.frp, &t.network, Some(t.model.dom.clone_ref()))
    }
}

impl display::Object for RawText {
    fn display_object(&self) -> &display::object::Instance {
        self.dom.display_object()
    }
}
