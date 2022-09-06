//! Example visualisation showing the provided data as text.

use crate::component::visualization::*;
use crate::prelude::*;
use ensogl::system::web::traits::*;

use crate::component::visualization;

use enso_frp as frp;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl::display::shape::primitive::StyleWatch;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl_hardcoded_theme;



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
        let div = web::document.create_div_or_panic();
        let dom = DomSymbol::new(&div);
        let size = Rc::new(Cell::new(Vector2(200.0, 200.0)));

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let text_color =
            styles.get_color(ensogl_hardcoded_theme::graph_editor::visualization::text);
        let _red = text_color.red * 255.0;
        let _green = text_color.green * 255.0;
        let _blue = text_color.blue * 255.0;
        let text_color = format!("rgba({},{},{},{})", _red, _green, _blue, text_color.alpha);
        let padding_text = format!("{}px", PADDING_TEXT);

        dom.dom().set_attribute_or_warn("class", "visualization scrollable");
        dom.dom().set_style_or_warn("white-space", "pre");
        dom.dom().set_style_or_warn("overflow-y", "auto");
        dom.dom().set_style_or_warn("overflow-x", "auto");
        dom.dom().set_style_or_warn("font-family", "DejaVuSansMonoBook");
        dom.dom().set_style_or_warn("font-size", "12px");
        dom.dom().set_style_or_warn("padding-left", &padding_text);
        dom.dom().set_style_or_warn("padding-top", &padding_text);
        dom.dom().set_style_or_warn("color", text_color);
        dom.dom().set_style_or_warn("pointer-events", "auto");

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
        self.dom.dom().set_inner_html("");
        let data_str = serde_json::to_string_pretty(&**data_inner);
        let data_str = data_str.unwrap_or_else(|e| format!("<Cannot render data: {}>", e));
        let max_line_size = 1024;
        if data_str.len() > max_line_size {
            split_long_lines(&data_str, max_line_size, &mut |line| {
                let node = web::document.create_div_or_panic();
                node.set_inner_text(line);
                let res = self.dom.dom().append_child(&node);
                if res.is_err() {
                    Err(DataError::InternalComputationError)
                } else {
                    Ok(())
                }
            })
        } else {
            self.dom.dom().set_inner_text(&data_str);
            Ok(())
        }
    }

    fn reload_style(&self) {
        self.dom.set_size(self.size.get());
    }

    fn set_layer(&self, layer: Layer) {
        layer.apply_for_html_component(&self.scene, &self.dom)
    }
}

fn split_long_lines(
    data_str: &str,
    max_line_size: usize,
    process_line: &mut impl FnMut(&str) -> Result<(), DataError>,
) -> Result<(), DataError> {
    let chunks = data_str.char_indices().chunks(max_line_size);
    let chunk_boundaries = chunks
        .into_iter()
        .filter_map(|mut chunk| chunk.next().map(|(ix, _)| ix))
        .chain(std::iter::once(data_str.len()));
    for (start, end) in chunk_boundaries.into_iter().tuple_windows() {
        process_line(&data_str[start..end])?;
    }
    Ok(())
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

#[cfg(test)]

mod tests {
    use crate::component::visualization::DataError;

    #[test]
    fn test_split_long_lines() {
        let str = "ABCDEFGH".to_string().repeat(1024);
        let mut cnt = 0;
        let res = super::split_long_lines(&str, 512, &mut |l| {
            assert_eq!(l.len(), 512);
            assert_eq!(&l[0..1], "A");
            cnt += 1;
            Ok(())
        });
        assert!(res.is_ok());
        assert_eq!(cnt, 16);
    }

    #[test]
    fn test_split_long_lines_with_failure() {
        let str = "ABCDEFGH".to_string().repeat(1024);
        let mut cnt = 0;
        let res = super::split_long_lines(&str, 128, &mut |l| {
            assert_eq!(l.len(), 128);
            assert_eq!(&l[0..1], "A");
            cnt += 1;
            if cnt >= 4 {
                Err(DataError::InvalidJsonText)
            } else {
                Ok(())
            }
        });
        assert!(res.is_err());
        assert_eq!(cnt, 4);
    }

    #[test]
    fn test_emoticons() {
        let str = "👨‍👨‍👧‍👧👨‍👨‍👧‍👧👨‍👨‍👧‍👧👨‍👨‍👧‍👧👨‍👨‍👧‍👧👨‍👨‍👧‍👧👨‍👨‍👧‍👧👨‍👨‍👧‍👧"
            .to_string()
            .repeat(1024);
        let res = super::split_long_lines(&str, 512, &mut |l| {
            assert_eq!(l.chars().count(), 512);
            Ok(())
        });
        assert!(res.is_ok());
    }
}
