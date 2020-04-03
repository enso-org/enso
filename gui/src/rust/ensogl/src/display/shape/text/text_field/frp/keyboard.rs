//! A FRP definitions for keyboard event handling, with biding this FRP graph to js events.

use crate::prelude::*;

use crate::display::shape::text::text_field::cursor::Step;
use crate::display::shape::text::text_field::TextField;
use crate::display::shape::text::text_field::WeakTextField;
use crate::system::web::text_input::KeyboardBinding;
use crate::system::web::text_input::bind_frp_to_js_keyboard_actions;

use enso_frp::*;



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
pub struct TextFieldKeyboardFrp {
    /// A "keyboard" part of graph derived from frp crate.
    pub keyboard: Keyboard,
    /// Keyboard actions. Here we define shortcuts for all actions except letters input, copying
    /// and pasting.
    pub actions: KeyboardActions,
    /// Event sent once cut operation was requested.
    pub on_cut: Dynamic<()>,
    /// Event sent once copy operation was requested.
    pub on_copy: Dynamic<()>,
    /// Event sent once paste operation was requested.
    pub on_paste: Dynamic<String>,
    /// A lambda node performing cut operation. Returns the string which should be copied to
    /// clipboard.
    pub do_cut: Dynamic<String>,
    /// A lambda node performing copy operation. Returns the string which should be copied to
    /// clipboard.
    pub do_copy: Dynamic<String>,
    /// A lambda node performing paste operation.
    pub do_paste: Dynamic<()>,
    /// A lambda node performing character input operation.
    pub do_char_input: Dynamic<()>,
}

impl TextFieldKeyboardFrp {
    /// Create FRP graph operating on given TextField pointer.
    pub fn new(text_field:WeakTextField) -> Self {
        let keyboard    = Keyboard::default();
        let mut actions = KeyboardActions::new(&keyboard);
        let cut         = Self::copy_lambda(true, text_field.clone_ref());
        let copy        = Self::copy_lambda(false, text_field.clone_ref());
        let paste       = Self::paste_lambda(text_field.clone_ref());
        let insert_char = Self::char_typed_lambda(text_field.clone_ref());
        frp! {
            text_field.on_cut        = source();
            text_field.on_copy       = source();
            text_field.on_paste      = source();
            text_field.do_copy       = on_copy .map(move |()| copy());
            text_field.do_cut        = on_cut  .map(move |()| cut());
            text_field.do_paste      = on_paste.map(paste);
            text_field.do_char_input = keyboard.on_pressed.map2(&keyboard.key_mask,insert_char);
        }
        Self::initialize_actions_map(&mut actions, text_field);
        TextFieldKeyboardFrp {keyboard,actions,on_cut,on_copy,on_paste,do_cut,do_copy,do_paste,
            do_char_input}
    }

    /// Bind this FRP graph to js events.
    ///
    /// Until the returned `KeyboardBinding` structure lives, the js events will emit the proper
    /// source events in this graph.
    pub fn bind_frp_to_js_text_input_actions(&self, binding:&mut KeyboardBinding) {
        bind_frp_to_js_keyboard_actions(&self.keyboard,binding);
        let copy_handler = enclose!(
            ( self.on_cut  => on_cut
            , self.on_copy => on_copy
            , self.do_cut  => do_cut
            , self.do_copy => do_copy
            ) move |is_cut| {
                if is_cut {
                    on_cut.event.emit(());
                    do_cut.behavior.current_value()
                } else {
                    on_copy.event.emit(());
                    do_copy.behavior.current_value()
                }
            }
        );
        let paste_handler = enclose!((self.on_paste => on_paste) move |text_to_paste| {
            on_paste.event.emit(text_to_paste);
        });
        binding.set_copy_handler(copy_handler);
        binding.set_paste_handler(paste_handler);
    }
}


// === Private ===

impl TextFieldKeyboardFrp {

    fn copy_lambda(cut:bool, text_field:WeakTextField)
                   -> impl Fn() -> String {
        move || {
            text_field.upgrade().map_or(default(), |text_field| {
                let result = text_field.get_selected_text();
                if cut { text_field.remove_selection(); }
                result
            })
        }
    }

    fn paste_lambda(text_field:WeakTextField) -> impl Fn(&String) {
        move |text_to_paste| {
            let inserted = text_to_paste.as_str();
            text_field.upgrade().for_each(|tf| { tf.write(inserted) })
        }
    }

    fn char_typed_lambda(text_field:WeakTextField) -> impl Fn(&Key,&KeyMask) {
        move |key,mask| {
            text_field.upgrade().for_each(|text_field| {
                if let Key::Character(string) = key {
                    let modifiers = &[Key::Control,Key::Alt];
                    let is_modifier  = modifiers.iter().any(|k| mask.has_key(k));
                    let is_alt_graph = mask.has_key(&Key::AltGraph);
                    // On Windows AltGraph is emitted as both AltGraph and Ctrl. Therefore we don't
                    // care about modifiers when AltGraph is pressed.
                    if  !is_modifier || is_alt_graph {
                        text_field.write(string);
                    }
                }
            })
        }
    }

    fn initialize_actions_map
    (actions:&mut KeyboardActions, text_field:WeakTextField) {
        use Key::*;
        let mut setter = TextFieldActionsSetter{actions,text_field};
        setter.set_navigation_action(&[ArrowLeft],          Step::Left);
        setter.set_navigation_action(&[ArrowRight],         Step::Right);
        setter.set_navigation_action(&[ArrowUp],            Step::Up);
        setter.set_navigation_action(&[ArrowDown],          Step::Down);
        setter.set_navigation_action(&[Home],               Step::LineBegin);
        setter.set_navigation_action(&[End],                Step::LineEnd);
        setter.set_navigation_action(&[Control,Home],       Step::DocBegin);
        setter.set_navigation_action(&[Control,End],        Step::DocEnd);
        setter.set_navigation_action(&[Control,ArrowLeft],  Step::LeftWord);
        setter.set_navigation_action(&[Control,ArrowRight], Step::RightWord);
        setter.set_action(&[Alt, Character("j".into())], |t| t.select_next_word_occurrence());
        setter.set_action(&[Enter],                      |t| t.write("\n"));
        setter.set_action(&[Delete],                     |t| t.do_delete_operation(Step::Right));
        setter.set_action(&[Backspace],                  |t| t.do_delete_operation(Step::Left));
        setter.set_action(&[Escape],                     |t| t.finish_multicursor_mode());
        setter.set_action(&[PageDown],                   |t| t.page_down());
        setter.set_action(&[PageUp],                     |t| t.page_up());
    }
}


// === Private Utilities ===

/// An utility struct for setting actions in text field. See `initialize_actions_map` function
/// for its usage.
struct TextFieldActionsSetter<'a> {
    text_field : WeakTextField,
    actions    : &'a mut KeyboardActions,
}

impl<'a> TextFieldActionsSetter<'a> {
    fn set_action<F>(&mut self, keys:&[Key], action:F)
    where F : Fn(&TextField) + 'static {
        let ptr = self.text_field.clone_ref();
        self.actions.set_action(keys.into(), move |_| {
            if let Some(text_field) = ptr.upgrade() {
                action(&text_field);
            }
        });
    }

    fn set_navigation_action(&mut self, base:&[Key], step:Step) {
        let selecting   = base.iter().cloned().chain(std::iter::once(Key::Shift)).collect_vec();
        self.set_action(base, move |t| t.navigate_cursors(step,false));
        self.set_action(selecting.as_ref(), move |t| t.navigate_cursors(step,true));
    }
}
