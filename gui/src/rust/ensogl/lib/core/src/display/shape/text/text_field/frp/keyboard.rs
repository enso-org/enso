//! A FRP definitions for keyboard event handling, with biding this FRP graph to js events.

use crate::prelude::*;

use crate::display::shape::text::text_field::cursor::Step;
use crate::display::shape::text::text_field::TextField;
use crate::display::shape::text::text_field::WeakTextField;
use crate::system::web::text_input::KeyboardBinding;
use crate::system::web::text_input::bind_frp_to_js_keyboard_actions;
use crate::system::web::platform;

use enso_frp as frp;
use enso_frp::io::keyboard_old::Keyboard;
use enso_frp::io::keyboard_old;



// ====================
// === TextFieldFrp ===
// ====================

/// This structure contains all nodes in FRP graph handling keyboards events of one TextField
/// component.
///
/// The most of TextField actions are covered by providing actions to Actions for specific
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
    pub actions: keyboard_old::Actions,
    pub network: frp::Network,
    /// Event sent once cut operation was requested.
    pub on_cut: frp::Source,
    /// Event sent once copy operation was requested.
    pub on_copy: frp::Source,
    /// Event sent once paste operation was requested.
    pub on_paste: frp::Source<String>,
    /// A lambda node performing cut operation. Returns the string which should be copied to
    /// clipboard.
    pub do_cut_sampler: frp::Sampler<String>,
    /// A lambda node performing copy operation. Returns the string which should be copied to
    /// clipboard.
    pub do_copy_sampler: frp::Sampler<String>,
    /// A lambda node performing paste operation.
    pub do_paste: frp::Stream,
    /// A lambda node performing character input operation.
    pub do_char_input: frp::Stream,
}

impl TextFieldKeyboardFrp {
    /// Create FRP graph operating on given TextField pointer.
    pub fn new(text_field:WeakTextField) -> Self {
        let keyboard    = Keyboard::default();
        let mut actions = keyboard_old::Actions::new(&keyboard);
        let cut         = Self::copy_lambda(true,text_field.clone_ref());
        let copy        = Self::copy_lambda(false,text_field.clone_ref());
        let paste       = Self::paste_lambda(text_field.clone_ref());
        let insert_char = Self::char_typed_lambda(text_field.clone_ref());
        frp::new_network! { text_field_network // FIXME name
            def on_cut          = source();
            def on_copy         = source();
            def on_paste        = source();
            def do_copy         = on_copy .map(move |()| copy());
            def do_cut          = on_cut  .map(move |()| cut());
            def do_cut_sampler  = do_cut.sampler();
            def do_copy_sampler = do_copy.sampler();
            def do_paste        = on_paste.map(paste);
            def do_char_input   = keyboard.on_pressed.map2(&keyboard.key_mask,insert_char);
        }
        Self::initialize_actions_map(&mut actions,text_field);
        let network = text_field_network;
        TextFieldKeyboardFrp
            {keyboard,actions,network,on_cut,on_copy,on_paste,do_cut_sampler,do_copy_sampler
            ,do_paste,do_char_input}
    }

    /// Bind this FRP graph to js events.
    ///
    /// Until the returned `KeyboardBinding` structure lives, the js events will emit the proper
    /// source events in this graph.
    pub fn bind_frp_to_js_text_input_actions(&self, binding:&mut KeyboardBinding) {
        bind_frp_to_js_keyboard_actions(&self.keyboard,binding);
        let copy_handler = enclose!(
            ( self.on_cut          => on_cut
            , self.on_copy         => on_copy
            , self.do_cut_sampler  => do_cut_sampler
            , self.do_copy_sampler => do_copy_sampler
            ) move |is_cut| {
                if is_cut {
                    on_cut.emit(());
                    do_cut_sampler.value()
                } else {
                    on_copy.emit(());
                    do_copy_sampler.value()
                }
            }
        );
        let paste_handler = enclose!((self.on_paste => on_paste) move |text_to_paste| {
            on_paste.emit(text_to_paste);
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

    fn enables_writing(mask:&keyboard_old::KeyMask) -> bool {
        let modifiers = &[keyboard_old::Key::Control, keyboard_old::Key::Alt, keyboard_old::Key::Meta];
        let is_modifier  = modifiers.iter().any(|key| mask.contains(key));
        let is_alt_graph = mask.contains(&keyboard_old::Key::AltGraph);
        match platform::current() {
            // On Windows AltGraph is emitted as both AltGraph and Ctrl. Therefore we don't
            // care about modifiers when AltGraph is pressed.
            platform::Windows => !is_modifier || is_alt_graph,
            _                 => !is_modifier
        }
    }

    fn char_typed_lambda(text_field:WeakTextField) -> impl Fn(&keyboard_old::Key,&keyboard_old::KeyMask) {
        move |key,mask| {
            text_field.upgrade().for_each(|text_field| {
                if let keyboard_old::Key::Character(string) = key {
                    if Self::enables_writing(mask) {
                        text_field.write(string);
                    }
                }
            })
        }
    }

    fn initialize_actions_map
    (actions:&mut keyboard_old::Actions, text_field:WeakTextField) {
        use keyboard_old::Key::*;
        let mut setter = TextFieldActionsSetter{actions,text_field};
        setter.set_navigation_action(&[ArrowLeft],          Step::Left);
        setter.set_navigation_action(&[ArrowRight],         Step::Right);
        setter.set_navigation_action(&[ArrowUp],            Step::Up);
        setter.set_navigation_action(&[ArrowDown],          Step::Down);
        setter.set_navigation_action(&[PageDown],           Step::PageDown);
        setter.set_navigation_action(&[PageUp],             Step::PageUp);
        setter.set_navigation_action(&line_begin_keys(), Step::LineBegin);
        setter.set_navigation_action(&line_end_keys(),   Step::LineEnd);
        setter.set_navigation_action(&doc_begin_keys(),  Step::DocBegin);
        setter.set_navigation_action(&doc_end_keys(),    Step::DocEnd);
        setter.set_navigation_action(&left_word_keys(),  Step::LeftWord);
        setter.set_navigation_action(&right_word_keys(), Step::RightWord);
        setter.set_action(&[Alt, Character("j".into())], |t| t.select_next_word_occurrence());
        setter.set_action(&[Enter],                      |t| t.write("\n"));
        setter.set_action(&[Delete],                     |t| t.do_delete_operation(Step::Right));
        setter.set_action(&[Backspace],                  |t| t.do_delete_operation(Step::Left));
        setter.set_action(&[Escape],                     |t| t.finish_multicursor_mode());
    }
}


// === Keys combinations ===

fn line_begin_keys() -> Vec<keyboard_old::Key> {
    if let platform::MacOS = platform::current() {
        vec![keyboard_old::Key::Meta, keyboard_old::Key::ArrowLeft]
    } else {
        vec![keyboard_old::Key::Home]
    }
}

fn line_end_keys() -> Vec<keyboard_old::Key> {
    if let platform::MacOS = platform::current() {
        vec![keyboard_old::Key::Meta, keyboard_old::Key::ArrowRight]
    } else {
        vec![keyboard_old::Key::End]
    }
}

fn doc_begin_keys() -> Vec<keyboard_old::Key> {
    if let platform::MacOS = platform::current() {
        vec![keyboard_old::Key::Meta, keyboard_old::Key::ArrowUp]
    } else {
        vec![keyboard_old::Key::Control, keyboard_old::Key::Home]
    }
}

fn doc_end_keys() -> Vec<keyboard_old::Key> {
    if let platform::MacOS = platform::current() {
        vec![keyboard_old::Key::Meta, keyboard_old::Key::ArrowDown]
    } else {
        vec![keyboard_old::Key::Control, keyboard_old::Key::End]
    }
}

fn left_word_keys() -> Vec<keyboard_old::Key> {
    if let platform::MacOS = platform::current() {
        vec![keyboard_old::Key::Alt, keyboard_old::Key::ArrowLeft]
    } else {
        vec![keyboard_old::Key::Control, keyboard_old::Key::ArrowLeft]
    }
}

fn right_word_keys() -> Vec<keyboard_old::Key> {
    if let platform::MacOS = platform::current() {
        vec![keyboard_old::Key::Alt, keyboard_old::Key::ArrowRight]
    } else {
        vec![keyboard_old::Key::Control, keyboard_old::Key::ArrowRight]
    }
}


// === Private Utilities ===

/// An utility struct for setting actions in text field. See `initialize_actions_map` function
/// for its usage.
struct TextFieldActionsSetter<'a> {
    text_field : WeakTextField,
    actions    : &'a mut keyboard_old::Actions,
}

impl<'a> TextFieldActionsSetter<'a> {
    fn set_action<F>(&mut self, keys:&[keyboard_old::Key], action:F)
    where F : Fn(&TextField) + 'static {
        let ptr = self.text_field.clone_ref();
        self.actions.add_action_for_key_mask(keys.into(), move || {
            if let Some(text_field) = ptr.upgrade() {
                action(&text_field);
            }
        }).forget(); // FIXME remove forget
    }

    fn set_navigation_action(&mut self, base:&[keyboard_old::Key], step:Step) {
        let selecting   = base.iter().cloned().chain(std::iter::once(keyboard_old::Key::Shift)).collect_vec();
        self.set_action(base, move |t| t.navigate_cursors(step,false));
        self.set_action(selecting.as_ref(), move |t| t.navigate_cursors(step,true));
    }
}
