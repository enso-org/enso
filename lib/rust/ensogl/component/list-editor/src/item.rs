use ensogl_core::prelude::*;

use ensogl_core::display;
use ensogl_core::Animation;

use crate::placeholder::StrongPlaceholder;


ensogl_core::define_endpoints_2! {
    Input {
        set_margin_left(f32),
        skip_margin_anim(),
    }
    Output {
        margin_left(f32),
    }
}

#[derive(Debug)]
pub struct Item<T> {
    pub frp:         Frp,
    pub elem:        T,
    pub placeholder: StrongPlaceholder,
    pub debug:       Rc<Cell<bool>>,
}

impl<T: display::Object> Item<T> {
    pub fn new(elem: T) -> Self {
        let placeholder = StrongPlaceholder::new();
        let this = Self::new_from_placeholder(elem, placeholder);
        this
    }

    pub fn new_from_placeholder(elem: T, placeholder: StrongPlaceholder) -> Self {
        let frp = Frp::new();
        placeholder.add_child(&elem);
        let network = frp.network();
        let elem_obj = elem.display_object();
        let margin_left = Animation::<f32>::new_with_init(network, 0.0);
        let elem_offset = Animation::<Vector2>::new_with_init(network, elem.position().xy());
        margin_left.simulator.update_spring(|s| s * crate::DEBUG_ANIMATION_SPRING_FACTOR);
        elem_offset.simulator.update_spring(|s| s * crate::DEBUG_ANIMATION_SPRING_FACTOR);
        let debug = Rc::new(Cell::new(false));

        frp::extend! { network
            margin_left.skip <+ frp.skip_margin_anim;
            margin_left.target <+ frp.set_margin_left;
            frp.private.output.margin_left <+ margin_left.value;
            target_size <- all_with(&elem_obj.on_resized, &frp.set_margin_left, |w, m| w.x + m);
            placeholder.frp.set_target_size <+ target_size;
            _eval <- all_with(&margin_left.value, &elem_offset.value, f!((margin, offset) {
                elem_obj.set_xy(*offset + Vector2(*margin, 0.0));
            }));
        }
        elem_offset.target.emit(Vector2(0.0, 0.0));
        Self { frp, elem, placeholder, debug }
    }

    pub fn set_margin_left(&self, margin: f32) {
        self.frp.set_margin_left.emit(margin)
    }
}

impl<T> display::Object for Item<T> {
    fn display_object(&self) -> &display::object::Instance {
        self.placeholder.display_object()
    }
}
