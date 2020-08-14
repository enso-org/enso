//! Keyboard and mouse shortcut management.

use crate::prelude::*;

use super::command;

use crate::frp::io::keyboard::Keyboard;
use crate::frp::io::keyboard;
use crate::frp::io::mouse::Mouse;
use crate::frp::io::mouse;
use crate::frp;
use crate::system::web;



// =================
// === Constants ===
// =================

const DOUBLE_PRESS_THRESHOLD_MS : f32 = 300.0;


/// A pattern abstraction for IO actions. It is used for example to express a key binding which
/// works for any key pressed.
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub enum Pattern<T> {
    Equal(T), Any
}

impl<T:Into<mouse::ButtonMask>> From<T> for Pattern<mouse::ButtonMask> {
    fn from(t:T) -> Self {
        Self::Equal(t.into())
    }
}

impl<T:Into<keyboard::KeyMask>> From<T> for Pattern<keyboard::KeyMask> {
    fn from(t:T) -> Self {
        Self::Equal(t.into())
    }
}



// =====================
// === ActionPattern ===
// =====================

/// A pattern for mouse actions.
pub type MousePattern = Pattern<mouse::ButtonMask>;

/// A pattern for keyboard actions.
pub type KeyboardPattern = Pattern<keyboard::KeyMask>;

/// A mask for keyboard and mouse events. Consists of separate `Pattern` for the mouse and
/// the keyboard bit-masks.
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub struct ActionPattern {
    pub keyboard : KeyboardPattern,
    pub mouse    : MousePattern,
}

impl ActionPattern {
    fn new(keyboard:impl Into<KeyboardPattern>, mouse:impl Into<MousePattern>) -> Self {
        let keyboard = keyboard.into();
        let mouse    = mouse.into();
        Self {keyboard,mouse}
    }

    fn any() -> Self {
        let keyboard = Pattern::Any;
        let mouse    = Pattern::Any;
        Self {keyboard,mouse}
    }

    fn as_any_mouse_pattern(&self) -> Self {
        let keyboard = self.keyboard.clone();
        let mouse    = Pattern::Any;
        Self {keyboard,mouse}
    }

    fn as_any_keyboard_pattern(&self) -> Self {
        let keyboard = Pattern::Any;
        let mouse    = self.mouse.clone();
        Self {keyboard,mouse}
    }
}

#[derive(Clone,Debug,Default,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub struct ActionMask {
    pub keyboard : keyboard::KeyMask,
    pub mouse    : mouse::ButtonMask,
}


impl ActionMask {
    fn new(keyboard:impl Into<keyboard::KeyMask>, mouse:impl Into<mouse::ButtonMask>) -> Self {
        let keyboard = keyboard.into();
        let mouse    = mouse.into();
        Self {keyboard,mouse}
    }
}

impl From<&ActionMask> for ActionPattern {
    fn from(t:&ActionMask) -> Self {
        let keyboard = (&t.keyboard).into();
        let mouse    = (&t.mouse).into();
        Self {keyboard,mouse}
    }
}


// ==============
// === Action ===
// ==============

/// A type of a keyboard action.
#[derive(Clone,Copy,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub enum ActionType {
    Press, Release, DoublePress
}

/// Keyboard action defined as `ActionType` and `ActionPattern`, like "press both key 'n' and primary
/// mouse button". Please note that the release action happens as soon as the key mask is no longer
/// valid. For example, pressing key "n", and then pressing key "a" (while holding "n") will trigger
/// the release event for the key "n".
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct Action {
    pub tp   : ActionType,
    pub mask : ActionPattern,
}

impl Action {
    /// Constructor.
    pub fn new(tp:impl Into<ActionType>, mask:impl Into<ActionPattern>) -> Self {
        let tp   = tp.into();
        let mask = mask.into();
        Self {tp,mask}
    }

    /// Smart constructor for the `Press` action.
    pub fn press
    (keyboard_mask:impl Into<KeyboardPattern>, mouse_mask:impl Into<MousePattern>) -> Self {
        Self::new(ActionType::Press,ActionPattern::new(keyboard_mask,mouse_mask))
    }

    /// Smart constructor for the `Release` action.
    pub fn release
    (keyboard_mask:impl Into<KeyboardPattern>, mouse_mask:impl Into<MousePattern>) -> Self {
        Self::new(ActionType::Release,ActionPattern::new(keyboard_mask,mouse_mask))
    }

    /// Smart constructor for the `DoublePress` action.
    pub fn double_press
    (keyboard_mask:impl Into<KeyboardPattern>, mouse_mask:impl Into<MousePattern>) -> Self {
        Self::new(ActionType::DoublePress,ActionPattern::new(keyboard_mask,mouse_mask))
    }
}



// ===============
// === Command ===
// ===============

/// A command name.
#[derive(Clone,Debug,Eq,From,Hash,Into,PartialEq,Shrinkwrap)]
pub struct Command {
    name : String,
}

impl From<&str> for Command {
    fn from(s:&str) -> Self {
        Self {name:s.into()}
    }
}



// =================
// === Condition ===
// =================

// TODO[WD]: Uncomment and handle more complex cases. Left commented to show the direction of future
//           development.
/// Condition expression.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub enum Condition {
    Ok,
    Simple (String),
    // Or  (Box<Condition>, Box<Condition>),
    // And (Box<Condition>, Box<Condition>),
}



// ============
// === Rule ===
// ============

/// A shortcut rule. Consist of target identifier (like "TextEditor"), a `Command` that will be
/// evaluated on the target, and a `Condition` which needs to be true in order for the command
/// to be executed.
#[derive(Clone,Debug)]
pub struct Rule {
    target  : String,
    command : Command,
    when    : Condition,
}

impl Rule {
    /// Constructor. Version without condition checker.
    pub fn new<T,C>(target:T, command:C) -> Self
        where T:Into<String>, C:Into<Command> {
        Self::new_when(target,command,Condition::Ok)
    }

    /// Constructor.
    pub fn new_when<T,C>(target:T, command:C, when:Condition) -> Self
        where T:Into<String>, C:Into<Command> {
        let target  = target.into();
        let command = command.into();
        Self {target,when,command}
    }
}



// ================
// === Shortcut ===
// ================

/// A keyboard shortcut, an `Action` associated with a `Rule`.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct Shortcut {
    #[shrinkwrap(main_field)]
    rule   : Rule,
    action : Action,
}

impl Shortcut {
    /// Constructor. Version without condition checker.
    pub fn new<A,T,C>(action:A, target:T, command:C) -> Self
        where A:Into<Action>, T:Into<String>, C:Into<Command> {
        let rule   = Rule::new(target,command);
        let action = action.into();
        Self {rule,action}
    }

    /// Constructor.
    pub fn new_when<A,T,C>(action:A, target:T, command:C, condition:Condition) -> Self
        where A:Into<Action>, T:Into<String>, C:Into<Command> {
        let rule     = Rule::new_when(target,command,condition);
        let action = action.into();
        Self {rule,action}
    }
}



// ================
// === Registry ===
// ================

/// Shortcut registry. See `Shortcut` to learn more.
///
/// You can add new shortcuts by using the `add` method and get a `Handle` back. When `Handle` is
/// dropped, the shortcut will be lazily removed. This is useful when defining shortcuts by GUI
/// components. When a component is unloaded, all its default shortcuts should be removed as well.
///
/// ## Implementation Notes
/// There should be a layer for user shortcuts which will remember handles permanently until a
/// shortcut is unregistered.
#[derive(Clone,CloneRef,Debug)]
pub struct Registry {
    model   : RegistryModel,
    network : frp::Network,
}

type RuleMap   = HashMap<ActionPattern,Vec<WeakHandle>>;
type ActionMap = HashMap<ActionType,RuleMap>;

/// Internal representation of `Registry`.
#[derive(Clone,CloneRef,Debug)]
pub struct RegistryModel {
    logger            : Logger,
    keyboard          : Keyboard,
    mouse             : Mouse,
    command_registry  : command::Registry,
    action_map        : Rc<RefCell<ActionMap>>,
}

impl Deref for Registry {
    type Target = RegistryModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl Registry {
    /// Constructor.
    pub fn new(logger:&Logger, mouse:&Mouse, keyboard:&Keyboard, command_registry:&command::Registry) -> Self {
        let model    = RegistryModel::new(logger,mouse,keyboard,command_registry);
        let keyboard = &model.keyboard;
        let mouse    = &model.mouse;

        frp::new_network! { network
            mask <- all_with(&keyboard.key_mask,&mouse.button_mask,|k,m|ActionMask::new(k,m));
            nothing_pressed      <- mask.map(|m| *m == default());
            nothing_pressed_prev <- nothing_pressed.previous();
            press                <- mask.gate_not(&nothing_pressed);
            single_press         <- press.gate(&nothing_pressed_prev);
            eval press ((m) model.process_action(ActionType::Press,m));

            single_press_prev  <- single_press.previous();
            press_time         <- single_press.map(|_| web::performance().now() as f32);
            press_time_prev    <- press_time.previous();
            time_delta         <- press_time.map2(&press_time_prev, |t1,t2| (t1-t2));
            is_double_press    <- time_delta.map4(&press,&single_press_prev,&nothing_pressed_prev,
                move |delta,t,s,g| *g && *delta < DOUBLE_PRESS_THRESHOLD_MS && t == s);
            double_press       <- press.gate(&is_double_press);
            eval double_press ((m) model.process_action(ActionType::DoublePress,m));

            prev_mask_on_key_up <- keyboard.key_mask.map3
                (&keyboard.prev_key_mask,&mouse.button_mask,|_,k,m|ActionMask::new(k,m));
            prev_mask_on_mouse_up <- mouse.button_mask.map3
                (&keyboard.key_mask,&mouse.prev_button_mask,|_,k,m|ActionMask::new(k,m));
            prev_mask <- any(prev_mask_on_key_up,prev_mask_on_mouse_up);
            same_key  <- prev_mask.map2(&mask,|t,s| t == s);
            release   <- prev_mask.gate_not(&same_key);
            eval release ((m) model.process_action(ActionType::Release,m));
        }
        Self {model,network}
    }
}

impl RegistryModel {
    /// Constructor.
    pub fn new
    (logger:impl AnyLogger, mouse:&Mouse, keyboard:&Keyboard, command_registry:&command::Registry) -> Self {
        let logger            = Logger::sub(logger,"ShortcutRegistry");
        let keyboard          = keyboard.clone_ref();
        let mouse             = mouse.clone_ref();
        let command_registry  = command_registry.clone_ref();
        let action_map        = default();
        Self {logger,keyboard,mouse,command_registry,action_map}
    }

    fn process_action(&self, action_type:ActionType, mask:&ActionMask) {
        let pattern        = ActionPattern::from(mask);
        let action_map_mut = &mut self.action_map.borrow_mut();
        if let Some(rule_map) = action_map_mut.get_mut(&action_type) {
            if let Some(rules) = rule_map.get_mut(&pattern) {
                self.process_rules(rules)
            }
            if let Some(rules) = rule_map.get_mut(&pattern.as_any_keyboard_pattern()) {
                self.process_rules(rules)
            }
            if let Some(rules) = rule_map.get_mut(&pattern.as_any_mouse_pattern()) {
                self.process_rules(rules)
            }
            if let Some(rules) = rule_map.get_mut(&ActionPattern::any()) {
                self.process_rules(rules)
            }
        }
    }

    fn process_rules(&self, rules:&mut Vec<WeakHandle>) {
        let mut targets = Vec::new();
        {
            let borrowed_command_map = self.command_registry.instances.borrow();
            rules.retain(|weak_rule| {
                weak_rule.upgrade().map(|rule| {
                    let target = &rule.target;
                    borrowed_command_map.get(target).for_each(|commands| {
                        for command in commands {
                            if Self::condition_checker(&rule.when,&command.status_map) {
                                let command_name = &rule.command.name;
                                match command.command_map.get(command_name){
                                    Some(t) => targets.push(t.frp.clone_ref()),
                                    None    => warning!(&self.logger,
                                        "Command {command_name} was not found on {target}."),
                                }
                            }
                        }
                    })
                }).is_some()
            })
        }
        for target in targets {
            target.emit(())
        }
    }

    fn condition_checker
    (condition:&Condition, status_map:&HashMap<String,command::Status>) -> bool {
        match condition {
            Condition::Ok           => true,
            Condition::Simple(name) => status_map.get(name).map(|t| t.frp.value()).unwrap_or(false)
        }
    }
}

impl Add<Shortcut> for &Registry {
    type Output = Handle;
    fn add(self, shortcut:Shortcut) -> Handle {
        let handle     = Handle::new(shortcut.rule);
        let instance   = handle.downgrade();
        let action_map = &mut self.action_map.borrow_mut();
        let rule_map   = action_map.entry(shortcut.action.tp).or_default();
        let rules      = rule_map.entry(shortcut.action.mask).or_default();
        rules.push(instance);
        handle
    }
}



// ==============
// === Handle ===
// ==============

/// A handle to registered shortcut. When dropped, the shortcut will be lazily removed from the
/// `Registry`.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct Handle {
    rule : Rc<Rule>
}

impl Handle {
    /// Constructor.
    pub fn new(rule:Rule) -> Self {
        let rule = Rc::new(rule);
        Self {rule}
    }

    fn downgrade(&self) -> WeakHandle {
        let rule = Rc::downgrade(&self.rule);
        WeakHandle {rule}
    }
}

/// Weak version of the `Handle`.
#[derive(Clone,CloneRef,Debug)]
pub struct WeakHandle {
    rule : Weak<Rule>
}

impl WeakHandle {
    fn upgrade(&self) -> Option<Handle> {
        self.rule.upgrade().map(|rule| Handle {rule})
    }
}



// ===============================
// === DefaultShortcutProvider ===
// ===============================

/// Trait allowing providing default set of shortcuts exposed by an object.
pub trait DefaultShortcutProvider : command::Provider {
    /// Set of default shortcuts.
    fn default_shortcuts() -> Vec<Shortcut> {
        default()
    }

    /// Helper for defining shortcut targeting this object.
    fn self_shortcut_when
    (action:impl Into<Action>, command:impl Into<Command>, condition:Condition) -> Shortcut {
        Shortcut::new_when(action,Self::label(),command,condition)
    }

    /// Helper for defining shortcut targeting this object. Version which does not accept
    /// condition checker.
    fn self_shortcut(action:impl Into<Action>, command:impl Into<Command>) -> Shortcut {
        Shortcut::new(action,Self::label(),command)
    }
}
