#![allow(missing_docs)]

use ensogl::traits::*;

use ensogl::display::world::World;
use ensogl::display::shape::text::glyph::font::FontRegistry;
use ensogl::display::shape::text::text_field::TextField;
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::display::world::*;
use ensogl::system::web;
use nalgebra::Vector2;
use nalgebra::Vector4;
use wasm_bindgen::prelude::*;



const TEXT:&str =
"To be, or not to be, that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune,
Or to take arms against a sea of troubles
And by opposing end them. To die—to sleep,
No more; and by a sleep to say we end
The heart-ache and the thousand natural shocks
That flesh is heir to: 'tis a consummation
Devoutly to be wish'd.";


#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_text_field() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    ensogl_core_msdf_sys::run_once_initialized(|| {
        let world     = &World::new(&web::get_html_element_by_id("root").unwrap());
        let mut fonts = FontRegistry::new();
        let font      = fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();

        let properties = TextFieldProperties {
            font,
            text_size  : 16.0,
            base_color : Vector4::new(0.0, 0.0, 0.0, 1.0),
            size       : Vector2::new(200.0, 200.0)
        };

        let text_field = TextField::new_with_content(&world,TEXT,properties);
        text_field.set_position(Vector3::new(10.0, 600.0, 0.0));
        text_field.jump_cursor(Vector2::new(50.0, -40.0),false);
        world.add_child(&text_field.display_object());

        world.on_frame(move |_| { let _keep_alive = &text_field; }).forget();
        world.keep_alive_forever();
    });
}
