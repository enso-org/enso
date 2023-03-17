use crate::prelude::*;

use crate::component::node::input::widget::triangle;
use crate::component::node::input::widget::SampledFrp;
use crate::component::node::input::widget::ACTIVATION_SHAPE_COLOR;
use crate::component::node::input::widget::ACTIVATION_SHAPE_SIZE;

use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::GenericLayoutApi;

const GAP: f32 = 3.0;

#[derive(Clone, CloneRef, Debug)]
pub struct Elements {
    app:            Application,
    display_object: display::object::Instance,
    items:          Rc<RefCell<Vec<ensogl_component::text::Text>>>,
}

impl Elements {
    const HEIGHT: f32 = 16.0;
    const GAP: f32 = 10.0;

    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        let items = default();
        display_object.use_auto_layout().set_gap_x(GAP);
        display_object.set_size_y(0.0);
        Self { app, display_object, items }
    }

    fn clear_elements(&self) {
        self.items.borrow_mut().clear();
    }

    fn set_elements<'a>(&self, codes: impl IntoIterator<Item = &'a str>) {
        let mut codes = codes.into_iter();
        let mut items = self.items.borrow_mut();
        for (element, code) in items.iter_mut().zip(&mut codes) {
            if element.content.value().to_string() != code {
                warn!("Updating content to {code:?}");
                element.set_content(ImString::new(code));
            }
        }
        for code in codes {
            let new_element = ensogl_component::text::Text::new(&self.app);
            self.display_object.add_child(&new_element);
            warn!("Setting new content to {code:?}");
            new_element.set_content(ImString::new(code));
            new_element.set_alignment_center_x();
            self.app.display.default_scene.layers.label.add(&new_element);
            items.push(new_element);
        }
    }

    fn show(&self) {
        self.display_object.set_size_y(Self::HEIGHT);
    }

    fn hide(&self) {
        self.display_object.set_size_y(0.0);
    }
}

impl display::Object for Elements {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    network:          frp::Network,
    display_object:   display::object::Instance,
    activation_shape: triangle::View,
    elements:         Elements,
}

impl Model {
    pub fn new(app: &Application, parent: &display::object::Instance, frp: &SampledFrp) -> Self {
        let network = frp::Network::new("vector_editor::Model");
        let display_object = display::object::Instance::new();
        let activation_shape = triangle::View::new();
        let elements = Elements::new(app);

        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);
        display_object.add_child(&elements);
        display_object.add_child(&activation_shape);
        display_object
            .use_auto_layout()
            .set_column_count(1)
            .set_gap_y(GAP)
            .set_children_alignment_center();
        display_object.set_size_x_to_hug();
        parent.add_child(&display_object);

        frp::extend! { network
            init <- source_();
            let dot_clicked = activation_shape.events.mouse_down_primary.clone_ref();
            toggle_focus <- dot_clicked.map(f!([display_object](()) !display_object.is_focused()));
            set_focused <- any(toggle_focus, frp.set_focused);
            eval set_focused([display_object, elements](focus) match focus {
                true => {
                    display_object.focus();
                    elements.show();
                },
                false => {
                    display_object.blur();
                    elements.hide();
                },
            });

            set_visible <- all(&frp.set_visible, &init)._0();
            shape_alpha <- set_visible.map(|visible| if *visible { 1.0 } else { 0.0 });
            shape_color <- shape_alpha.map(|a| ACTIVATION_SHAPE_COLOR.with_alpha(*a));
            eval shape_color([activation_shape] (color) {
                activation_shape.color.set(color::Rgba::from(color).into());
            });

            non_empty_value <- frp.set_current_value.filter_map(|v| v.clone());
            trace frp.set_current_value;
            empty_value <- frp.set_current_value.filter_map(|v| v.is_none().then_some(()));
            eval non_empty_value ((val) elements.set_elements(Self::parse_array_code(val.as_str())));
            eval empty_value ((()) elements.clear_elements());
        }
        init.emit(());

        Self { network, display_object, activation_shape, elements }
    }

    fn parse_array_code(code: &str) -> impl Iterator<Item = &str> {
        let without_braces = code.trim_start_matches([' ', '[']).trim_end_matches([' ', ']']);
        warn!("{without_braces:?}");
        let elements_with_trailing_spaces = without_braces.split(',');
        elements_with_trailing_spaces.map(|s| s.trim())
    }

    pub fn set_port_size(&self, port_size: Vector2) {
        // let height = ACTIVATION_SHAPE_SIZE.y() + GAP + Elements::HEIGHT;
        self.display_object.set_x(port_size.x() / 2.0);
        // self.display_object.set_y(-port_size.y() / 2.0 - height);
    }
}
