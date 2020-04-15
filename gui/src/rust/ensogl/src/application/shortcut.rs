//! Keyboard shortcut management.

use crate::prelude::*;

use super::command;

use crate::control::io::keyboard::listener::KeyboardFrpBindings;
use crate::frp::io::keyboard::Keyboard;
use crate::frp::io::keyboard;
use crate::frp;



// ================
// === Registry ===
// ================

/// Keyboard shortcut registry. You can add new shortcuts by using the `add` method and get a
/// `Handle` back. When `Handle` is dropped, the shortcut will be lazily removed. This is useful
/// when defining shortcuts by GUI components. When a component is unloaded, all its default
/// shortcuts should be removed as well.
///
/// Note: we should probably handle user shortcuts in a slightly different way. User shortcuts
/// should persist and should probably not return handles. Alternatively, there should be an user
/// shortcut manager which will own and manage the handles.
#[derive(Clone,CloneRef,Debug)]
pub struct Registry {
    logger            : Logger,
    keyboard          : Keyboard,
    keyboard_bindings : Rc<KeyboardFrpBindings>,
    network           : frp::Network,
    command_registry  : command::Registry,
    rule_map          : Rc<RefCell<HashMap<keyboard::KeyMask,Vec<Instance>>>>
}

impl Registry {
    /// Constructor.
    pub fn new(logger:&Logger, command_registry:&command::Registry) -> Self {
        let logger            = logger.sub("ShortcutRegistry");
        let keyboard          = Keyboard::default();
        let keyboard_bindings = Rc::new(KeyboardFrpBindings::new(&logger,&keyboard));
        let command_registry  = command_registry.clone_ref();
        let rule_map          = default();
        let network           = default();
        Self {logger,keyboard,keyboard_bindings,network,command_registry,rule_map} . init()
    }

    fn init(self) -> Self {
        let network          = &self.network;
        let logger           = self.logger.clone_ref();
        let rule_map         = self.rule_map.clone_ref();
        let command_registry = self.command_registry.clone_ref();
        frp::extend_network! { network
            def _on_key_press = self.keyboard.key_mask.map(move |key_mask| {
                rule_map.borrow_mut().get_mut(key_mask).map(|rules| {
                    Self::process_rules(&logger,&command_registry,rules)
                })
            });
        }
        self
    }

    fn process_rules
    (logger:&Logger, command_registry:&command::Registry, rules:&mut Vec<Instance>) {
        let mut targets = Vec::new();
        {
            let borrowed_command_map = command_registry.instances.borrow();
            rules.retain(|weak_rule| {
                weak_rule.upgrade().map(|rule| {
                    let target = &rule.target;
                    borrowed_command_map.get(target).for_each(|commands| {
                        for command in commands {
                            if Self::condition_checker(&rule.when,&command.status_map) {
                                let command_name = &rule.command.name;
                                match command.command_map.get(command_name){
                                    Some(t) => targets.push(t.frp.clone_ref()),
                                    None    => warning!(&logger,
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
        let handle   = Handle::new(shortcut.rule);
        let instance = handle.downgrade();
        self.rule_map.borrow_mut().entry(shortcut.key_mask).or_default().push(instance);
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

    fn downgrade(&self) -> Instance {
        let rule = Rc::downgrade(&self.rule);
        Instance {rule}
    }
}

#[derive(Clone,CloneRef,Debug)]
struct Instance {
    rule : Weak<Rule>
}

impl Instance {
    fn upgrade(&self) -> Option<Handle> {
        self.rule.upgrade().map(|rule| Handle {rule})
    }
}



// ================
// === Shortcut ===
// ================

/// A keyboard shortcut, a `keyboard::KeyMask` associated with a `Rule`.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct Shortcut {
    #[shrinkwrap(main_field)]
    rule     : Rule,
    key_mask : keyboard::KeyMask,
}

impl Shortcut {
    /// Constructor. Version without condition checker.
    pub fn new<M,T,C>(key_mask:M, target:T, command:C) -> Self
        where M:Into<keyboard::KeyMask>, T:Into<String>, C:Into<Command> {
        let rule     = Rule::new(target,command);
        let key_mask = key_mask.into();
        Self {rule,key_mask}
    }

    /// Constructor.
    pub fn new_when<M,T,C>(key_mask:M, target:T, command:C, condition:Condition) -> Self
    where M:Into<keyboard::KeyMask>, T:Into<String>, C:Into<Command> {
        let rule     = Rule::new_when(target,command,condition);
        let key_mask = key_mask.into();
        Self {rule,key_mask}
    }
}



// ============
// === Rule ===
// ============

/// A shortcut rule. Consist of target identifier (`command::ProviderTag`), a `Command` that will
/// be evaluated on the target, and a `Condition` which needs to be true in order for the command
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

// TODO: Uncomment and handle more complex cases. Left here to show the intention of future dev.
/// Condition expression.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub enum Condition {
    Ok,
    Simple (String),
    // Or     (Box<Condition>, Box<Condition>),
    // And    (Box<Condition>, Box<Condition>),
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
    fn self_shortcut_when<M,C>(key_mask:M, command:C, condition:Condition) -> Shortcut
        where M:Into<keyboard::KeyMask>, C:Into<Command> {
        Shortcut::new_when(key_mask,Self::label(),command,condition)
    }

    /// Helper for defining shortcut targeting this object. Version which does not accept
    /// condition checker.
    fn self_shortcut<M,C>(key_mask:M, command:C) -> Shortcut
        where M:Into<keyboard::KeyMask>, C:Into<Command> {
        Shortcut::new(key_mask,Self::label(),command)
    }
}

/// Default implementation for all objects. It allows implementing this trait only by objects which
/// want to use it.
impl<T:command::Provider> DefaultShortcutProvider for T {
    default fn default_shortcuts() -> Vec<Shortcut> {
        default()
    }
}
