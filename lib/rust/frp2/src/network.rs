//! Implementation of the FRP network, a group of FRP nodes.

use crate::prelude::*;

use crate::runtime::with_runtime;
use crate::runtime::NetworkId;


// ===============
// === Network ===
// ===============

/// Alias for [`Network`] with the default parametrization;
pub type Network_ = Network;

/// An FRP network, a group of FRP nodes.
///
/// # WARNING: Be careful when cloning the network.
/// As long as the network lives, all nodes in the network will live as well. Cloning the network
/// and passing it to FRP node closure will cause memory leaks. The ability to clone the network is
/// provided for backward compatibility only and will be removed in the future.
#[derive(CloneRef, Debug, Default, Deref)]
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Network<Model = ()> {
    rc: Rc<NetworkModel<Model>>,
}

impl<Model: Default> Network<Model> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

/// Internal representation of [`Network`].
#[derive(Debug)]
pub struct NetworkModel<Model> {
    pub(crate) id:    NetworkId,
    pub(crate) model: Rc<OptRefCell<Model>>,
}

impl<Model: Default> NetworkModel<Model> {
    #[inline(never)]
    fn new() -> Self {
        let id = with_runtime(|rt| rt.new_network());
        let model = default();
        NetworkModel { id, model }
    }
}

impl<Model: Default> Default for NetworkModel<Model> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Model> Drop for NetworkModel<Model> {
    fn drop(&mut self) {
        with_runtime(|rt| rt.drop_network(self.id));
    }
}
