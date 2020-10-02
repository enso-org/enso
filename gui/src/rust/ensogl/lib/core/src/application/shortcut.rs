//! Keyboard and mouse shortcut management.

use crate::prelude::*;

use super::command;

use crate::frp::io::keyboard;
use crate::frp::io::mouse::Mouse;
use crate::frp;

use enso_shortcuts as shortcuts;
use enso_shortcuts::traits::*;

pub use shortcuts::ActionType;



// ============
// === Rule ===
// ============

/// Shortcut action rule, a combination of `ActionType`, like `Press` and a pattern, like
/// "ctrl shift s".
#[derive(Clone,Debug,Eq,PartialEq,Hash)]
#[allow(missing_docs)]
pub struct Rule {
    pub tp      : ActionType,
    pub pattern : String,
}

impl Rule {
    /// Constructor.
    pub fn new(tp:impl Into<ActionType>, pattern:impl Into<String>) -> Self {
        let tp      = tp.into();
        let pattern = pattern.into();
        Self {tp,pattern}
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
#[derive(Clone,Debug,Eq,PartialEq,Hash)]
#[allow(missing_docs)]
pub enum Condition {
    Ok,
    Simple (String),
    // Or  (Box<Condition>, Box<Condition>),
    // And (Box<Condition>, Box<Condition>),
}



// ==============
// === Action ===
// ==============

/// A shortcut action. Consist of target identifier (like "TextEditor"), a `Command` that will be
/// evaluated on the target, and a `Condition` which needs to be true in order for the command
/// to be executed.
#[derive(Clone,Debug,Eq,PartialEq,Hash)]
pub struct Action {
    target  : String,
    command : Command,
    when    : Condition,
}

impl Action {
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

/// A keyboard shortcut, an `Rule` associated with a `Action`.
#[derive(Clone,Debug,Eq,PartialEq,Hash,Shrinkwrap)]
pub struct Shortcut {
    #[shrinkwrap(main_field)]
    action : Action,
    rule   : Rule,
}

impl Shortcut {
    /// Constructor. Version without condition checker.
    pub fn new<A,T,C>(rule:A, target:T, command:C) -> Self
    where A:Into<Rule>, T:Into<String>, C:Into<Command> {
        let action = Action::new(target,command);
        let rule   = rule.into();
        Self {action,rule}
    }

    /// Constructor.
    pub fn new_when<A,T,C>(rule:A, target:T, command:C, condition:Condition) -> Self
        where A:Into<Rule>, T:Into<String>, C:Into<Command> {
        let action = Action::new_when(target,command,condition);
        let rule   = rule.into();
        Self {action,rule}
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

/// Internal representation of `Registry`.
#[derive(Clone,CloneRef,Debug)]
pub struct RegistryModel {
    logger             : Logger,
    keyboard           : keyboard::Keyboard,
    mouse              : Mouse,
    command_registry   : command::Registry,
    shortcuts_registry : shortcuts::HashSetRegistry<Shortcut>,
}

impl Deref for Registry {
    type Target = RegistryModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl Registry {
    /// Constructor.
    pub fn new
    (logger:&Logger, mouse:&Mouse, keyboard:&keyboard::Keyboard, cmd_registry:&command::Registry)
    -> Self {
        let model = RegistryModel::new(logger,mouse,keyboard,cmd_registry);
        let mouse = &model.mouse;

        frp::new_network! { network
            kb_down    <- keyboard.down.map (f!((t) model.shortcuts_registry.on_press(t.simple_name())));
            kb_up      <- keyboard.up.map   (f!((t) model.shortcuts_registry.on_release(t.simple_name())));
            mouse_down <- mouse.down.map    (f!((t) model.shortcuts_registry.on_press(t.simple_name())));
            mouse_up   <- mouse.up.map      (f!((t) model.shortcuts_registry.on_release(t.simple_name())));
            event      <- any(kb_down,kb_up,mouse_down,mouse_up);
            eval event ((m) model.process_rules(&m));
        }
        Self {model,network}
    }
}

impl RegistryModel {
    /// Constructor.
    pub fn new
    ( logger           : impl AnyLogger
    , mouse            : &Mouse
    , keyboard         : &keyboard::Keyboard
    , command_registry : &command::Registry
    ) -> Self {
        let logger             = Logger::sub(logger,"ShortcutRegistry");
        let keyboard           = keyboard.clone_ref();
        let mouse              = mouse.clone_ref();
        let command_registry   = command_registry.clone_ref();
        let shortcuts_registry = default();
        Self {logger,keyboard,mouse,command_registry,shortcuts_registry}
    }

    fn process_rules(&self, rules:&[Shortcut]) {
        let mut targets = Vec::new();
        {
            let borrowed_command_map = self.command_registry.instances.borrow();
            for rule in rules {
                let target = &rule.action.target;
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
            }
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
    type Output = ();
    fn add(self, shortcut:Shortcut) {
        self.model.shortcuts_registry.add(shortcut.rule.tp,&shortcut.rule.pattern,shortcut.clone());
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
    (action:impl Into<Rule>, command:impl Into<Command>, condition:Condition) -> Shortcut {
        Shortcut::new_when(action,Self::label(),command,condition)
    }

    /// Add a new shortcut targetting the self object.
    fn self_shortcut
    (action_type:ActionType, pattern:impl Into<String>, command:impl Into<Command>) -> Shortcut {
        Shortcut::new(Rule::new(action_type,pattern),Self::label(),command)
    }
}
