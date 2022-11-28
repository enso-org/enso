//! A debug scene which shows the slider component

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
use wasm_bindgen::prelude::*;

use enso_frp as frp;
use ensogl_core::application::{Application, View};
use ensogl_core::application::shortcut;
use ensogl_core::data::color;
use ensogl_hardcoded_theme as theme;
use ensogl_slider as slider;
use ensogl_text_msdf::run_once_initialized;



// ===================
// === FRP network ===
// ===================

ensogl_core::define_endpoints! {
    Input {
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

#[derive(Clone, Debug, Deref)]
struct SliderCollection {
    #[deref]
    frp:            Frp,
    app:            Application,
    sliders:        Vec<slider::Slider>,
}

impl SliderCollection {
    fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let app = app.clone_ref();
        let sliders = Vec::new();
        Self { frp, app, sliders }.init()
    }

    fn init(self) -> Self {
        let network = self.frp.network();
        let input = &self.frp.input;
        let slider_collection = &self;

        frp::extend! { network
            eval_ input.drop_sliders(
                slider_collection.drop_sliders();
            );
            trace input.drop_sliders;
        }
        self
    }

    fn make_slider(&mut self) -> &slider::Slider {
        let slider = self.app.new_view::<slider::Slider>();
        slider.frp.set_background_color(color::Lcha(0.8, 0.0, 0.0, 1.0));
        slider.frp.set_max_value(5.0);
        slider.frp.set_default_value(1.0);
        slider.frp.set_value(1.0);
        self.app.display.add_child(&slider);
        self.sliders.push(slider);
        self.sliders.last().unwrap()
    }

    fn drop_sliders(&mut self) {
        warn!("Dropping sliders!");

        for slider in self.sliders.iter() {
            self.app.display.remove_child(&slider);
        }
        self.sliders.clear();
    }
}



// ============
// === View ===
// ============

impl View for SliderCollection {
    fn label() -> &'static str {
        "Sliders"
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
            Self::self_shortcut(Press, "cmd c", "drop_sliders"),
        ]
    }
}



// ===================
// === Entry Point ===
// ===================

/// An entry point.
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

fn init(app: &Application) {
    theme::builtin::dark::register(app);
    theme::builtin::light::register(app);
    theme::builtin::light::enable(app);

    let sliders = app.new_view::<SliderCollection>();
    let mut sliders = Leak::new(sliders);

    let slider1 = sliders.inner_mut().make_slider();
    slider1.frp.set_width(400.0);
    slider1.frp.set_height(50.0);
    slider1.set_y(-120.0);
    slider1.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider1.frp.set_label("Soft limits + tooltip");
    slider1.frp.set_lower_limit_type(slider::SliderLimit::Soft);
    slider1.frp.set_upper_limit_type(slider::SliderLimit::Soft);
    slider1.frp.set_tooltip("Slider information tooltip.");

    let slider2 = sliders.inner_mut().make_slider();
    slider2.frp.set_width(400.0);
    slider2.frp.set_height(50.0);
    slider2.set_y(-60.0);
    slider2.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider2.frp.set_slider_disabled(true);
    slider2.frp.set_label("Disabled");

    let slider3 = sliders.inner_mut().make_slider();
    slider3.frp.set_width(400.0);
    slider3.frp.set_height(50.0);
    slider3.set_y(0.0);
    slider3.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider3.frp.set_label("Adaptive lower limit");
    slider3.frp.set_lower_limit_type(slider::SliderLimit::Adaptive);

    let slider4 = sliders.inner_mut().make_slider();
    slider4.frp.set_width(400.0);
    slider4.frp.set_height(50.0);
    slider4.set_y(60.0);
    slider4.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider4.frp.set_label("Adaptive upper limit");
    slider4.frp.set_label_position(slider::LabelPosition::Inside);
    slider4.frp.set_upper_limit_type(slider::SliderLimit::Adaptive);


    let slider5 = sliders.inner_mut().make_slider();
    slider5.frp.set_width(75.0);
    slider5.frp.set_height(230.0);
    slider5.set_y(-35.0);
    slider5.set_x(275.0);
    slider5.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider5.frp.set_label("Hard limits");
    slider5.frp.set_orientation(slider::SliderOrientation::Vertical);

    let slider6 = sliders.inner_mut().make_slider();
    slider6.frp.set_width(75.0);
    slider6.frp.set_height(230.0);
    slider6.set_y(-35.0);
    slider6.set_x(375.0);
    slider6.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider6.frp.set_label("Soft\nlimits");
    slider6.frp.set_label_position(slider::LabelPosition::Inside);
    slider6.frp.set_lower_limit_type(slider::SliderLimit::Soft);
    slider6.frp.set_upper_limit_type(slider::SliderLimit::Soft);
    slider6.frp.set_orientation(slider::SliderOrientation::Vertical);

    let slider7 = sliders.inner_mut().make_slider();
    slider7.frp.set_width(400.0);
    slider7.frp.set_height(10.0);
    slider7.set_y(-160.0);
    slider7.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider7.frp.set_value_text_hidden(true);
    slider7.frp.set_precision_adjustment_disabled(true);
    slider7.frp.set_value_indicator(slider::ValueIndicator::Thumb);
    slider7.frp.set_thumb_size(0.1);

    let slider8 = sliders.inner_mut().make_slider();
    slider8.frp.set_width(400.0);
    slider8.frp.set_height(10.0);
    slider8.set_y(-180.0);
    slider8.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider8.frp.set_value_text_hidden(true);
    slider8.frp.set_precision_adjustment_disabled(true);
    slider8.frp.set_value_indicator(slider::ValueIndicator::Thumb);
    slider8.frp.set_thumb_size(0.25);

    let slider9 = sliders.inner_mut().make_slider();
    slider9.frp.set_width(400.0);
    slider9.frp.set_height(10.0);
    slider9.set_y(-200.0);
    slider9.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider9.frp.set_value_text_hidden(true);
    slider9.frp.set_precision_adjustment_disabled(true);
    slider9.frp.set_value_indicator(slider::ValueIndicator::Thumb);
    slider9.frp.set_thumb_size(0.5);

    let slider10 = sliders.inner_mut().make_slider();
    slider10.frp.set_width(10.0);
    slider10.frp.set_height(230.0);
    slider10.set_y(-35.0);
    slider10.set_x(430.0);
    slider10.frp.set_value_indicator_color(color::Lcha(0.4, 0.7, 0.7, 1.0));
    slider10.frp.set_value_text_hidden(true);
    slider10.frp.set_precision_adjustment_disabled(true);
    slider10.frp.set_value_indicator(slider::ValueIndicator::Thumb);
    slider10.frp.set_orientation(slider::SliderOrientation::Vertical);
}
