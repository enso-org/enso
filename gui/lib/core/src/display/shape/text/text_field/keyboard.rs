//! A FRP definitions for keyboard event handling, with biding this FRP graph to js events.

use crate::prelude::*;

use crate::display::shape::text::text_field::cursor::Step;
use crate::display::shape::text::text_field::TextFieldData;
use crate::system::web::text_input::KeyboardBinding;

use enso_frp::*;
use web_sys::KeyboardEvent;



// ====================
// === TextFieldFrp ===
// ====================

/// This structure contains all nodes in FRP graph handling keyboards events of one TextField
/// component.
///
/// The most of TextField actions are covered by providing actions to KeyboardActions for specific
/// key masks. However, there are special actions which must be done in a lower level:
///  * *clipboard operations* - they are performed by reading text input js events directly from
///    text area component. See `system::web::text_input` crate.
///  * *text input operations* - here we want to handle all the keyboard mapping set by user, so
///    we connect this action directly to `key_press` node from `keyboard`.
#[derive(Debug)]
pub struct TextFieldFrp {
    /// A "keyboard" part of graph derived from frp crate.
    keyboard: Keyboard,
    /// Keyboard actions. Here we define shortcuts for all actions except letters input, copying
    /// and pasting.
    actions: KeyboardActions,
    /// Event sent once cut operation was requested.
    on_cut: Dynamic<()>,
    /// Event sent once copy operation was requested.
    on_copy: Dynamic<()>,
    /// Event sent once paste operation was requested.
    on_paste: Dynamic<String>,
    /// A lambda node performing cut operation. Returns the string which should be copied to
    /// clipboard.
    do_cut: Dynamic<String>,
    /// A lambda node performing copy operation. Returns the string which should be copied to
    /// clipboard.
    do_copy: Dynamic<String>,
    /// A lambda node performing paste operation.
    do_paste: Dynamic<()>,
    /// A lambda node performing character input operation.
    do_char_input: Dynamic<()>,
}

impl TextFieldFrp {
    /// Create FRP graph operating on given TextField pointer.
    pub fn new(text_field_ptr:Weak<RefCell<TextFieldData>>) -> TextFieldFrp {
        let keyboard    = Keyboard::default();
        let mut actions = KeyboardActions::new(&keyboard);
        let cut         = Self::copy_lambda(true, text_field_ptr.clone());
        let copy        = Self::copy_lambda(false, text_field_ptr.clone());
        let paste       = Self::paste_lambda(text_field_ptr.clone());
        let insert_char = Self::char_typed_lambda(text_field_ptr.clone());
        frp! {
            text_field.on_cut        = source();
            text_field.on_copy       = source();
            text_field.on_paste      = source();
            text_field.do_copy       = on_copy .map(move |()| copy());
            text_field.do_cut        = on_cut  .map(move |()| cut());
            text_field.do_paste      = on_paste.map(paste);
            text_field.do_char_input = keyboard.on_pressed.map2(&keyboard.key_mask,insert_char);
        }
        Self::initialize_actions_map(&mut actions,text_field_ptr);
        TextFieldFrp {keyboard,actions,on_cut,on_copy,on_paste,do_cut,do_copy,do_paste,
            do_char_input}
    }

    /// Bind this FRP graph to js events.
    ///
    /// Until the returned `KeyboardBinding` structure lives, the js events will emit the proper
    /// source events in this graph.
    pub fn bind_frp_to_js_text_input_actions(&self) -> KeyboardBinding {
        let mut binding      = KeyboardBinding::create();
        let frp_key_pressed  = self.keyboard.on_pressed.clone_ref();
        let frp_key_released = self.keyboard.on_released.clone_ref();
        let frp_cut          = self.on_cut.clone_ref();
        let frp_copy         = self.on_copy.clone_ref();
        let frp_paste        = self.on_paste.clone_ref();
        let frp_text_to_copy = self.do_copy.clone_ref();
        binding.set_key_down_handler(move |event:KeyboardEvent| {
            if let Ok(key) = event.key().parse::<Key>() {
                frp_key_pressed.event.emit(key);
            }
        });
        binding.set_key_up_handler(move |event:KeyboardEvent| {
            if let Ok(key) = event.key().parse::<Key>() {
                frp_key_released.event.emit(key);
            }
        });
        binding.set_copy_handler(move |is_cut| {
            if is_cut {
                frp_cut.event.emit(())
            } else {
                frp_copy.event.emit(());
            }
            frp_text_to_copy.behavior.current_value()
        });
        binding.set_paste_handler(move |text_to_paste| {
            frp_paste.event.emit(text_to_paste);
        });
        binding
    }
}


// === Private ===

impl TextFieldFrp {

    fn copy_lambda(cut:bool, text_field_ptr:Weak<RefCell<TextFieldData>>)
                   -> impl Fn() -> String {
        move || {
            text_field_ptr.upgrade().map_or(default(),|text_field| {
                let mut text_field_ref = text_field.borrow_mut();
                let result             = text_field_ref.get_selected_text();
                if cut { text_field_ref.remove_selection(); }
                result
            })
        }
    }

    fn paste_lambda(text_field_ptr:Weak<RefCell<TextFieldData>>) -> impl Fn(&String) {
        move |text_to_paste| {
            let inserted = text_to_paste.as_str();
            text_field_ptr.upgrade().for_each(|tf| { tf.borrow_mut().write(inserted) })
        }
    }

    fn char_typed_lambda(text_field_ptr:Weak<RefCell<TextFieldData>>) -> impl Fn(&Key,&KeyMask) {
        move |key,mask| {
            text_field_ptr.upgrade().for_each(|text_field| {
                if let Key::Character(string) = key {
                    let modifiers = &[Key::Control,Key::Alt,Key::Meta];
                    if !modifiers.iter().any(|k| mask.has_key(k)) {
                        text_field.borrow_mut().write(string);
                    }
                }
            })
        }
    }

    fn initialize_actions_map
    (actions:&mut KeyboardActions, text_field_ptr:Weak<RefCell<TextFieldData>>) {
        use Key::*;
        let mut setter = TextFieldActionsSetter{actions,text_field_ptr};
        setter.set_navigation_action(&[ArrowLeft],    Step::Left);
        setter.set_navigation_action(&[ArrowRight],   Step::Right);
        setter.set_navigation_action(&[ArrowUp],      Step::Up);
        setter.set_navigation_action(&[ArrowDown],    Step::Down);
        setter.set_navigation_action(&[Home],         Step::LineBegin);
        setter.set_navigation_action(&[End],          Step::LineEnd);
        setter.set_navigation_action(&[Control,Home], Step::DocBegin);
        setter.set_navigation_action(&[Control,End],  Step::DocEnd);
        setter.set_action(&[Enter],     |t| t.write("\n"));
        setter.set_action(&[Delete],    |t| t.do_delete_operation(Step::Right));
        setter.set_action(&[Backspace], |t| t.do_delete_operation(Step::Left));
    }
}


// === Private Utilities ===

/// An utility struct for setting actions in text field. See `initialize_actions_map` function
/// for its usage.
struct TextFieldActionsSetter<'a> {
    text_field_ptr: Weak<RefCell<TextFieldData>>,
    actions       : &'a mut KeyboardActions,
}

impl<'a> TextFieldActionsSetter<'a> {
    fn set_action<F>(&mut self, keys:&[Key], action:F)
    where F : Fn(&mut TextFieldData) + 'static {
        let ptr = self.text_field_ptr.clone();
        self.actions.set_action(keys.into(), move |_| {
            if let Some(ptr) = ptr.upgrade() {
                let mut text_field_ref = ptr.borrow_mut();
                action(&mut text_field_ref);
            }
        });
    }

    fn set_navigation_action(&mut self, base_keys:&[Key], step:Step) {
        self.set_action(base_keys, move |t| t.navigate_cursors(step,false));
        let base_keys_cloned = base_keys.iter().cloned();
        let selecting_keys   = base_keys_cloned.chain(std::iter::once(Key::Shift)).collect_vec();
        self.set_action(selecting_keys.as_ref(), move |t| t.navigate_cursors(step,true));
    }
}
