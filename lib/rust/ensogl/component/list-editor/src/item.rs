use ensogl_core::prelude::*;

use ensogl_core::display;
use ensogl_core::Animation;

use crate::placeholder::StrongPlaceholder;


ensogl_core::define_endpoints_2! {
    Input {
        set_margin_left(f32),
        skip_anim(),
        skip_margin_anim(),
    }
    Output {
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
    pub fn new(elem: T, init_width: f32) -> Self {
        let placeholder = StrongPlaceholder::new();
        let this = Self::new_from_placeholder(elem, placeholder);
        this.placeholder.set_target_size(init_width);
        this.placeholder.skip_animation();
        this
    }
    pub fn new_from_placeholder(elem: T, placeholder: StrongPlaceholder) -> Self {
        let frp = Frp::new();
        placeholder.add_child(&elem);
        let network = frp.network();
        let elem_display_object = elem.display_object();
        let margin_left = Animation::<f32>::new_with_init(network, 0.0);
        let elem_offset = Animation::<Vector2>::new_with_init(network, elem.position().xy());
        margin_left.simulator.update_spring(|s| s * crate::DEBUG_ANIMATION_SPRING_FACTOR);
        elem_offset.simulator.update_spring(|s| s * crate::DEBUG_ANIMATION_SPRING_FACTOR);
        let debug = Rc::new(Cell::new(false));

        frp::extend! { network
            margin_left.target <+ frp.set_margin_left;
            // fixme:
            elem_size <- elem_display_object.on_transformed.map(f!((_)
                elem_display_object.computed_size().x
            ));
            target_size <= all_with(&elem_size, &margin_left.value, f!([debug](w, m) {
                let out = w + m;
                let out = if *w > 0.0 {
                    Some(out)
                } else {
                    None
                };
                if debug.get() {
                    warn!("target_size: {} + {} = {:?}", w,m,out);
                }
                out
            }));
            placeholder.frp.set_target_size <+ target_size;
            _eval <- all_with(&margin_left.value, &elem_offset.value, f!((m, o) {
                elem_display_object.set_xy(*o + Vector2(*m, 0.0));
            }));

            placeholder.frp.skip_animation <+ frp.skip_anim;
            margin_left.skip <+ frp.skip_anim;
            margin_left.skip <+ frp.skip_margin_anim;
            // eval margin_left.value((t) elem_display_object.set_x(*t));
            // eval elem_offset.value((t) elem_display_object.set_xy(*t));
        }
        elem_offset.target.emit(Vector2(0.0, 0.0));
        //fixme
        mem::forget(margin_left);
        mem::forget(elem_offset);


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
