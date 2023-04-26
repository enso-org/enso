//! An example scene showing the slider component usage.

// === Features ===
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::application::View;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_slider as slider;
use ensogl_text_msdf::run_once_initialized;



// ========================
// === Model definition ===
// ========================

/// The slider collection model holds a set of sliders that can be instantiated and dropped.
#[derive(Debug, Clone, CloneRef)]
pub struct Model {
    /// Vector that holds example sliders until they are dropped.
    sliders:   Rc<RefCell<Vec<slider::Slider>>>,
    app:       Application,
    root:      display::object::Instance,
    navigator: Navigator,
}

impl Model {
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let world = app.display.clone();
        let scene = &world.default_scene;
        let camera = scene.camera().clone_ref();
        let navigator = Navigator::new(scene, &camera);
        let sliders = Rc::new(RefCell::new(Vec::new()));
        let root = display::object::Instance::new();
        let model = Self { app, sliders, root, navigator };
        model.init_sliders();
        model
    }

    /// Add example sliders to scene.
    fn init_sliders(&self) {
        let slider1 = self.app.new_view::<slider::Slider>();
        slider1.set_size((200.0, 24.0));
        slider1.set_y(-120.0);
        slider1.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        slider1.frp.set_label("Soft limits + tooltip");
        slider1.frp.set_lower_limit_type(slider::SliderLimit::Soft);
        slider1.frp.set_upper_limit_type(slider::SliderLimit::Soft);
        slider1.frp.set_tooltip("Slider information tooltip.");
        self.root.add_child(&slider1);
        self.sliders.borrow_mut().push(slider1);

        // # IMPORTANT
        // This code is commented because the slider implementation is not finished yet. Please
        // refer to the doc comments in the slider's module to learn more.

        //
        // let slider2 = self.app.new_view::<slider::Slider>();
        // slider2.set_size((400.0, 50.0));
        // slider2.set_y(-60.0);
        // slider2.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider2.frp.set_slider_disabled(true);
        // slider2.frp.set_label("Disabled");
        // self.root.add_child(&slider2);
        // self.sliders.borrow_mut().push(slider2);
        //
        // let slider3 = self.app.new_view::<slider::Slider>();
        // slider3.set_size((400.0, 50.0));
        // slider3.set_y(0.0);
        // slider3.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider3.frp.set_default_value(100.0);
        // slider3.frp.set_value(100.0);
        // slider3.frp.set_max_value(500.0);
        // slider3.frp.set_label("Adaptive lower limit");
        // slider3.frp.set_lower_limit_type(slider::SliderLimit::Adaptive);
        // self.root.add_child(&slider3);
        // self.sliders.borrow_mut().push(slider3);
        //
        // let slider4 = self.app.new_view::<slider::Slider>();
        // slider4.set_size((400.0, 50.0));
        // slider4.set_y(60.0);
        // slider4.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider4.frp.set_label("Adaptive upper limit");
        // slider4.frp.set_label_position(slider::LabelPosition::Inside);
        // slider4.frp.set_upper_limit_type(slider::SliderLimit::Adaptive);
        // self.root.add_child(&slider4);
        // self.sliders.borrow_mut().push(slider4);
        //
        // let slider5 = self.app.new_view::<slider::Slider>();
        // slider5.set_size((75.0, 230.0));
        // slider5.set_y(-35.0);
        // slider5.set_x(275.0);
        // slider5.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider5.frp.set_label("Hard limits");
        // slider5.frp.orientation(Axis2::Y);
        // slider5.frp.set_max_disp_decimal_places(4);
        // self.root.add_child(&slider5);
        // self.sliders.borrow_mut().push(slider5);
        //
        // let slider6 = self.app.new_view::<slider::Slider>();
        // slider6.set_size((75.0, 230.0));
        // slider6.set_y(-35.0);
        // slider6.set_x(375.0);
        // slider6.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider6.frp.set_label("Soft\nlimits");
        // slider6.frp.set_label_position(slider::LabelPosition::Inside);
        // slider6.frp.set_lower_limit_type(slider::SliderLimit::Soft);
        // slider6.frp.set_upper_limit_type(slider::SliderLimit::Soft);
        // slider6.frp.orientation(Axis2::Y);
        // slider6.frp.set_max_disp_decimal_places(4);
        // self.root.add_child(&slider6);
        // self.sliders.borrow_mut().push(slider6);
        //
        // let slider7 = self.app.new_view::<slider::Slider>();
        // slider7.set_size((400.0, 10.0));
        // slider7.set_y(-160.0);
        // slider7.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider7.frp.show_value(false);
        // slider7.frp.set_precision_adjustment_disabled(true);
        // slider7.frp.kind(slider::Kind::Scrollbar(0.1));
        // slider7.frp.set_thumb_size(0.1);
        // self.root.add_child(&slider7);
        // self.sliders.borrow_mut().push(slider7);
        //
        // let slider8 = self.app.new_view::<slider::Slider>();
        // slider8.set_size((400.0, 10.0));
        // slider8.set_y(-180.0);
        // slider8.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider8.frp.show_value(false);
        // slider8.frp.set_precision_adjustment_disabled(true);
        // slider8.frp.kind(slider::Kind::Scrollbar(0.25));
        // slider8.frp.set_thumb_size(0.25);
        // self.root.add_child(&slider8);
        // self.sliders.borrow_mut().push(slider8);
        //
        // let slider9 = self.app.new_view::<slider::Slider>();
        // slider9.set_size((400.0, 10.0));
        // slider9.set_y(-200.0);
        // slider9.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider9.frp.show_value(false);
        // slider9.frp.set_precision_adjustment_disabled(true);
        // slider9.frp.kind(slider::Kind::Scrollbar(0.5));
        // slider9.frp.set_thumb_size(0.5);
        // self.root.add_child(&slider9);
        // self.sliders.borrow_mut().push(slider9);
        //
        // let slider10 = self.app.new_view::<slider::Slider>();
        // slider10.set_size((10.0, 230));
        // slider10.set_y(-35.0);
        // slider10.set_x(430.0);
        // slider10.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
        // slider10.frp.show_value(false);
        // slider10.frp.set_precision_adjustment_disabled(true);
        // slider10.frp.kind(slider::Kind::Scrollbar(0.1));
        // slider10.frp.orientation(Axis2::Y);
        // self.root.add_child(&slider10);
        // self.sliders.borrow_mut().push(slider10);
    }

    /// Drop all sliders from scene.
    fn drop_sliders(&self) {
        for slider in self.sliders.borrow_mut().drain(0..) {
            self.root.remove_child(&slider);
        }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}



// ===================
// === FRP network ===
// ===================

ensogl_core::define_endpoints! {
    Input {
        /// Add example sliders to scene.
        init_sliders(),
        /// Drop all sliders from scene.
        drop_sliders(),
    }
    Output {
    }
}

impl FrpNetworkProvider for SliderCollection {
    fn network(&self) -> &frp::Network {
        self.frp.network()
    }
}



// ==========================
// === Slider collection ===
// ==========================

/// A component that stores an array of slider components. It receives shortcuts to either
/// instantiate a new set of sliders or to drop the existing ones.
#[derive(Clone, Debug, Deref)]
struct SliderCollection {
    #[deref]
    frp:   Frp,
    app:   Application,
    model: Model,
}

impl SliderCollection {
    fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let app = app.clone_ref();
        let model = Model::new(&app);
        Self { frp, app, model }.init()
    }

    fn init(self) -> Self {
        let network = self.frp.network();
        let input = &self.frp.input;
        let model = &self.model;

        frp::extend! { network
            eval_ input.init_sliders( model.init_sliders() );
            eval_ input.drop_sliders( model.drop_sliders() );
        }
        self
    }
}

impl display::Object for SliderCollection {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}

impl View for SliderCollection {
    fn label() -> &'static str {
        "Slider Collection"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }

    fn app(&self) -> &Application {
        &self.app
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::Press;
        vec![
            Self::self_shortcut(Press, "ctrl a", "init_sliders"),
            Self::self_shortcut(Press, "ctrl d", "drop_sliders"),
        ]
    }
}



// ===================
// === Entry Point ===
// ===================

/// Entry point for the example scene.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}



// ========================
// === Init Application ===
// ========================

/// Initialize a `SliderCollection` and do not drop it.
fn init(app: &Application) {
    let slider_collection = app.new_view::<SliderCollection>().leak();
    app.display.add_child(&slider_collection);
}
