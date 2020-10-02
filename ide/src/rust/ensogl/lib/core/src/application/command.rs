//! Definition of commands, labeled FPR endpoints useful when implementing actions which can be
//! altered at runtime, like a keyboard shortcut management.

use crate::prelude::*;
use crate::frp;



// ================
// === Provider ===
// ================

/// A class of components which expose commands. A command is a labeled action represented as a
/// no argument FRP endpoint (`frp::Source`). Useful when implementing actions which can be altered
/// at runtime, like a keyboard shortcut management. `Provider` defines a class of elements, like
/// a class of text editors or class of tree views. A particular instance is represented as
/// `ProviderInstance`.
///
/// Please note that command `Provider`s and command `ProviderInstance`s should be explicitly
/// registered in the command `Registry` as soon as possible in order to share information about
/// the available commands. This information may be useful even before the first instance of a
/// particular provider is created, for example in order to provide user with hints about possible
/// API endpoints when defining keyboard shortcuts.
pub trait Provider : FrpNetworkProvider + CommandApi + StatusApi {
    /// Identifier of the command provider class.
    fn label() -> &'static str;
}

/// FRP endpoint for `Command`.
pub type CommandEndpoint = FrpEndpointDefinition<frp::Source>;

/// FRP endpoint for `Status`.
pub type StatusEndpoint  = FrpEndpointDefinition<frp::Sampler<bool>>;

/// FRP Network provider. Used to check whether FRP bindings are still alive.
pub trait FrpNetworkProvider {
    /// The underlying frp network accessor.
    fn network(&self) -> &frp::Network;
}



// ======================
// === API Definition ===
// ======================

/// Command API, a set of labeled command endpoints and labeled command docs. Both functions
/// should return the same set of labels. Although it could be designed in a safer way, it would
/// be much more trickier to use. You should not define it by hand. Instead use the provided
/// `def_command_api` macro.
#[allow(missing_docs)]
pub trait CommandApi : Sized {
    fn command_api_docs() -> Vec<EndpointDocs>    { default() }
    fn command_api(&self) -> Vec<CommandEndpoint> { default() }
}

/// Status API, a set of labeled status endpoints and labeled status docs. Both functions
/// should return the same set of labels. Although it could be designed in a safer way, it would
/// be much more trickier to use. You should not define it by hand. Instead use the provided
/// `def_status_api` macro.
#[allow(missing_docs)]
pub trait StatusApi : Sized {
    fn status_api_docs() -> Vec<EndpointDocs>;
    fn status_api(&self) -> Vec<StatusEndpoint>;
}

impl<T:Sized> StatusApi for T {
    default fn status_api_docs() -> Vec<EndpointDocs>   { default() }
    default fn status_api(&self) -> Vec<StatusEndpoint> { default() }
}



// ========================
// === ProviderInstance ===
// ========================

/// Instance of command `Provider`. It contains bindings to all FRP endpoints defined by the
/// `Provider`. See the docs of `Provider` to learn more.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct ProviderInstance {
    pub network     : frp::WeakNetwork,
    pub command_map : HashMap<String,Command>,
    pub status_map  : HashMap<String,Status>,
}

impl ProviderInstance {
    /// Check whether the underlying object is still alive.
    pub fn check_alive(&self) -> bool {
        self.network.upgrade().is_some()
    }
}



// =====================
// === FRP Endpoints ===
// =====================

/// FRP endpoint and a caption.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct FrpEndpoint<Frp> {
    pub caption : String,
    pub frp     : Frp,
}

/// Command is a labeled `frp::Source`.
pub type Command = FrpEndpoint<frp::Source>;

/// Status is a labeled `frp::Sampler<bool>`. It is useful for implementing keyboard shortcut rules.
pub type Status = FrpEndpoint<frp::Sampler<bool>>;



// ================================
// === FRP Endpoint Definitions ===
// ================================

/// Labeled FRP endpoint.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct FrpEndpointDefinition<Frp> {
    pub label : String,
    pub frp   : Frp
}

/// A pair of label and caption for a particular FRP endpoint.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct EndpointDocs {
    pub label   : String,
    pub caption : String,
}

impl<Frp> FrpEndpointDefinition<Frp> {
    /// Constructor.
    pub fn new<L,F>(label:L, frp:F) -> Self
        where L:Into<String>, F:Into<Frp> {
        let label = label.into();
        let frp   = frp.into();
        Self {label,frp}
    }
}

impl EndpointDocs {
    /// Constructor.
    pub fn new<L,C>(label:L, caption:C) -> Self
        where L:Into<String>, C:Into<String> {
        let label   = label.into();
        let caption = caption.into();
        Self {label,caption}
    }
}



// ================
// === Registry ===
// ================

/// A command registry. Allows registering command `Providers` and corresponding
/// `ProviderInstance`s. See docs of `Provider` to learn more.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct Registry {
    pub logger    : Logger,
    pub instances : Rc<RefCell<HashMap<String,Vec<ProviderInstance>>>>,
}

impl Registry {
    /// Constructor.
    pub fn create(logger:impl AnyLogger) -> Self {
        let logger    = Logger::sub(logger,"views");
        let instances = default();
        Self {logger,instances}
    }

    /// Registers the command `Provider`.
    pub fn register<V:Provider>(&self) {
        let label  = V::label();
        let exists = self.instances.borrow().get(label).is_some();
        if exists {
            warning!(&self.logger, "The view '{label}' was already registered.")
        } else {
            self.instances.borrow_mut().insert(label.into(),default());
        }
    }

    /// Registers the command `ProviderInstance`.
    pub fn register_instance<T:Provider>(&self, target:&T) {
        let label   = T::label();
        let network = T::network(target).downgrade();
        let command_doc_map : HashMap<String,String> = T::command_api_docs().into_iter().map(|t| {
            (t.label,t.caption)
        }).collect();
        let command_map = T::command_api(target).into_iter().map(|t| {
            let caption = command_doc_map.get(&t.label).unwrap().clone(); // fixme unwrap
            let frp     = t.frp;
            let endpoint = FrpEndpoint {caption,frp};
            (t.label,endpoint)
        }).collect();

        let status_doc_map : HashMap<String,String> = T::status_api_docs().into_iter().map(|t| {
            (t.label,t.caption)
        }).collect();
        let status_map = T::status_api(target).into_iter().map(|t| {
            let caption = status_doc_map.get(&t.label).unwrap().clone(); // fixme unwrap
            let frp     = t.frp;
            let endpoint = FrpEndpoint {caption,frp};
            (t.label,endpoint)
        }).collect();

        let instance = ProviderInstance {network,command_map,status_map};
        let was_registered = self.instances.borrow().get(label).is_some();
        if !was_registered {
            self.register::<T>();
            warning!(&self.logger,
                "The command provider '{label}' was created but never registered. You should \
                always register available command providers as soon as possible to spread the \
                information about their API.");
        };
        self.instances.borrow_mut().get_mut(label).unwrap().push(instance);
    }
}



// ==============
// === Macros ===
// ==============

/// Defines new `Status` API. See docs of `StatusApi` to learn more.
#[macro_export]
macro_rules! def_status_api {
    ( $name:ident
        $(
            #[doc=$($doc:tt)*]
            $field:ident
        ),* $(,)?
    ) => {
        #[derive(Debug,Clone,CloneRef)]
        pub struct $name {
            $(#[doc=$($doc)*] pub $field : frp::Sampler<bool>),*
        }

        impl application::command::StatusApi for $name {
            fn status_api_docs() -> Vec<application::command::EndpointDocs> {
                vec! [$(application::command::EndpointDocs::new(stringify!($field),$($doc)*)),*]
            }

            fn status_api(&self) -> Vec<application::command::StatusEndpoint> {
                vec! [$(
                    application::command::StatusEndpoint::new(stringify!($field),&self.$field))
                ,*]
            }
        }
    };
}

/// Defines new `Command` API. See docs of `CommandApi` to learn more.
#[macro_export]
macro_rules! def_command_api {
    ( $([$($opts:tt)*])? $name:ident
        $(
            #[doc=$($doc:tt)*]
            $field:ident
        ),* $(,)?
    ) => {
        /// Commands definition.
        #[derive(Debug,Clone,CloneRef)]
        pub struct $name {
            $(#[doc=$($doc)*] pub $field : frp::Source),*
        }

        impl application::command::CommandApi for $name {
            fn command_api_docs() -> Vec<application::command::EndpointDocs> {
                vec! [$(application::command::EndpointDocs::new(stringify!($field),$($doc)*)),*]
            }

            fn command_api(&self) -> Vec<application::command::CommandEndpoint> {
                vec! [$(
                    application::command::CommandEndpoint::new(stringify!($field),&self.$field))
                ,*]
            }
        }

        impl $name {
            /// Constructor.
            pub fn new(network:&frp::Network) -> Self {
                frp::extend! { $($($opts)*)? network
                    $($field <- source();)*
                }
                Self { $($field),* }
            }

            $(
                #[allow(missing_docs)]
                pub fn $field(&self) {
                    self.$field.emit(())
                }
            )*
        }
    };
}
