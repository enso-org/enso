use crate::prelude::*;

use crate::component::node::input::widget::triangle;
use crate::component::node::input::widget::SampledFrp;
use crate::component::node::input::widget::ACTIVATION_SHAPE_COLOR;
use crate::component::node::input::widget::ACTIVATION_SHAPE_SIZE;
use crate::component::node::input::widget::ACTIVATION_SHAPE_Y_OFFSET;

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
        display_object.set_size_y(Self::HEIGHT);
        Self { app, display_object, items }
    }

    fn clear_elements(&self) {
        self.items.borrow_mut().clear();
    }

    fn set_elements<'a>(&self, codes: impl IntoIterator<Item = &'a str>) {
        let mut codes = codes.into_iter();
        let mut items = self.items.borrow_mut();
        let mut remaining_count = 0;
        for (element, code) in items.iter_mut().zip(&mut codes) {
            remaining_count += 1;
            if element.content.value().to_string() != code {
                element.set_content(ImString::new(code));
            }
        }
        items.truncate(remaining_count);
        for code in codes {
            let new_element = ensogl_component::text::Text::new(&self.app);
            self.display_object.add_child(&new_element);
            new_element.set_content(ImString::new(code));
            self.app.display.default_scene.layers.label.add(&new_element);
            items.push(new_element);
        }
    }
}

impl display::Object for Elements {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    network:            frp::Network,
    display_object:     display::object::Instance,
    elements_container: display::object::Instance,
    activation_shape:   triangle::View,
    elements:           Elements,
    pub set_port_size:  frp::Source<Vector2>,
}

impl Model {
    pub fn new(app: &Application, parent: &display::object::Instance, frp: &SampledFrp) -> Self {
        let network = frp::Network::new("vector_editor::Model");
        let display_object = display::object::Instance::new();
        let elements_container = display::object::Instance::new();
        let activation_shape = triangle::View::new();
        let elements = Elements::new(app);

        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);
        display_object.add_child(&elements_container);
        display_object.add_child(&activation_shape);
        display_object
            .use_auto_layout()
            .set_column_count(1)
            .set_gap_y(GAP)
            .set_children_alignment_center();
        display_object.set_size_hug();
        elements_container.set_size_hug();
        parent.add_child(&display_object);

        frp::extend! { network
            init <- source_();
            set_port_size <- source::<Vector2>();
            let dot_clicked = activation_shape.events.mouse_down_primary.clone_ref();
            toggle_focus <- dot_clicked.map(f!([display_object](()) !display_object.is_focused()));
            set_focused <- any(toggle_focus, frp.set_focused);
            eval set_focused([display_object, elements_container, elements](focus) match focus {
                true => {
                    display_object.focus();
                    elements_container.add_child(&elements);
                },
                false => {
                    display_object.blur();
                    elements_container.remove_child(&elements);
                },
            });

            set_visible <- all(&frp.set_visible, &init)._0();
            shape_alpha <- set_visible.map(|visible| if *visible { 1.0 } else { 0.0 });
            shape_color <- shape_alpha.map(|a| ACTIVATION_SHAPE_COLOR.with_alpha(*a));
            eval shape_color([activation_shape] (color) {
                activation_shape.color.set(color::Rgba::from(color).into());
            });

            value <- all(frp.set_current_value, init)._0();
            non_empty_value <- value.filter_map(|v| v.clone());
            empty_value <- value.filter_map(|v| v.is_none().then_some(()));
            eval non_empty_value ((val) elements.set_elements(Self::parse_array_code(val.as_str())));
            eval empty_value ((()) elements.clear_elements());

            widget_size <- display_object.on_updated.map(f!((()) display_object.computed_size())).on_change();
            port_and_widget_size <- all(&set_port_size, &widget_size);
            eval port_and_widget_size ([display_object]((port_sz, sz)) {
                warn!("port_sz: {port_sz}, sz: {sz}");
                display_object.set_x(port_sz.x() / 2.0 - sz.x() / 2.0);
                display_object.set_y(-port_sz.y() - sz.y() - 5.0);
            });
        }
        init.emit(());

        Self {
            network,
            display_object,
            activation_shape,
            elements,
            elements_container,
            set_port_size,
        }
    }

    fn parse_array_code(code: &str) -> impl Iterator<Item = &str> {
        let looks_like_array = code.starts_with('[') && code.ends_with(']');
        warn!("Code \"{code}\": {looks_like_array}");
        let opt_iterator = if looks_like_array {
            let without_braces = code.trim_start_matches([' ', '[']).trim_end_matches([' ', ']']);
            let elements_with_trailing_spaces = without_braces.split(',');
            Some(elements_with_trailing_spaces.map(|s| s.trim()))
        } else {
            None
        };
        opt_iterator.into_iter().flatten()
    }
}
