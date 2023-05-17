use crate::{
    effect::{AnyEffect, EffectId},
    network::NetworkId,
    signal::SignalId,
};
use slotmap::{SecondaryMap, SlotMap};
use std::{
    any::Any,
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, HashSet},
    rc::Rc,
};

#[derive(Default)]
struct Dirty {
    // todo: compress into a bitset
    effects: VecSet<EffectId>,
}

impl Dirty {
    fn run_clean(&mut self, rt: &Runtime, tracking: &RefCell<DepTracking>) {
        for effect in self.effects.iter() {
            let notified_again = tracking.borrow().effects.contains(&effect);
            if !notified_again {
                rt.run_effect(*effect);
            }
        }
        self.effects.clear();
    }
}

/// Efficient insertion of unique elements preserving order.
#[derive(Debug, Default)]
struct VecSet<T> {
    vec: Vec<T>,
    set: HashMap<T, ()>,
}

impl<T> std::ops::Deref for VecSet<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl<T> VecSet<T> {
    fn push_if_unique(&mut self, value: T) -> bool
    where
        T: std::cmp::Eq + std::hash::Hash + Copy,
    {
        match self.set.entry(value) {
            Entry::Occupied(_) => false,
            Entry::Vacant(e) => {
                e.insert(());
                self.vec.push(value);
                true
            }
        }
    }

    fn contains(&self, value: &T) -> bool
    where
        T: std::cmp::Eq + std::hash::Hash,
    {
        self.set.contains_key(value)
    }

    fn clear(&mut self) {
        self.vec.clear();
        self.set.clear();
    }
}

#[derive(Default)]
struct DepTracking {
    notify_has_effect: HashSet<SignalId>,
    effects: VecSet<EffectId>,
    signal_subs: SecondaryMap<SignalId, HashSet<EffectId>>,
    effect_deps: SecondaryMap<EffectId, VecSet<SignalId>>,
}

impl DepTracking {
    fn cleanup_deps(&mut self, id: EffectId) {
        if let Some(deps) = self.effect_deps.get_mut(id) {
            for dep in deps.iter() {
                if let Some(subs) = self.signal_subs.get_mut(*dep) {
                    self.notify_has_effect.remove(dep);
                    subs.remove(&id);
                }
            }
            deps.clear();
        }
    }

    /// Set dependencies for an effect. Deps must be unique.
    fn set_deps(&mut self, id: EffectId, deps: VecSet<SignalId>) {
        for dep in deps.iter() {
            if let Some(subs) = self.signal_subs.entry(*dep) {
                subs.or_default().insert(id);
            }
        }
        self.effect_deps.insert(id, deps);
    }

    fn depend(&mut self, id: EffectId, signal: SignalId) {
        if let Some(deps) = self.effect_deps.entry(id) {
            if deps.or_default().push_if_unique(signal) {
                if let Some(subs) = self.signal_subs.entry(signal) {
                    subs.or_default().insert(id);
                }
            }
        }
    }

    fn notify(&mut self, signal: SignalId) {
        if self.notify_has_effect.insert(signal) {
            if let Some(subs) = self.signal_subs.get(signal) {
                for sub in subs {
                    self.effects.push_if_unique(*sub);
                }
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.effects.is_empty()
    }

    fn mark_dirty(&mut self, flags: &mut Dirty) {
        std::mem::swap(&mut flags.effects, &mut self.effects);
    }
}

#[derive(Debug)]
struct ObserverContext {
    effect: EffectId,
    // deps: VecSet<SignalId>,
}

#[derive(Default)]
pub(super) struct Runtime {
    observer: RefCell<Option<ObserverContext>>,
    networks: RefCell<SlotMap<NetworkId, RefCell<NetworkData>>>,
    signals: RefCell<SlotMap<SignalId, Rc<RefCell<dyn Any>>>>,
    tracking: RefCell<DepTracking>,
    dirty: RefCell<Dirty>,
    effects: RefCell<SlotMap<EffectId, Rc<dyn AnyEffect>>>,
}

impl Runtime {
    pub(super) fn depend(&self, signal: SignalId) {
        // let mut observer = self.observer.borrow_mut();
        // if let Some(ctx) = &mut *observer {
        //     ctx.deps.push_if_unique(signal);
        // }
        let observer = self.observer.borrow();
        if let Some(ctx) = &*observer {
            self.tracking.borrow_mut().depend(ctx.effect, signal);
        }
    }

    pub(super) fn notify(&self, signal: SignalId) {
        self.tracking.borrow_mut().notify(signal);
    }

    pub(super) fn get_signal_data(&self, signal: SignalId) -> Rc<RefCell<dyn Any>> {
        let signals = self.signals.borrow();
        let data = signals.get(signal).cloned().expect("signal not found");
        data
    }

    fn as_observer(&self, effect: EffectId, f: impl FnOnce()) {
        let ctx = ObserverContext { effect };
        let prev_observer = self.observer.replace(Some(ctx));
        f();
        self.observer.replace(prev_observer);

    }

    pub fn run_effect(&self, id: EffectId) {
        let effect = {
            let effects = self.effects.borrow();
            effects.get(id).cloned()
        };
        if let Some(effect) = effect {
            self.tracking.borrow_mut().cleanup_deps(id);
            // let ctx = ObserverContext { effect: id };
            // let prev_observer = self.observer.replace(Some(ctx));
            self.as_observer(id, || effect.run());
            // effect.run();
            // self.observer.replace(prev_observer);
            // self.tracking
            //     .borrow_mut()
            //     .set_deps(id, collected.unwrap().deps);
        } else {
            panic!("effect not found")
        }
    }
}

#[derive(Default)]
struct NetworkData {
    signals: HashSet<SignalId>,
    effects: HashSet<EffectId>,
}

impl Runtime {
    pub(super) fn new_network(&self) -> NetworkId {
        self.networks.borrow_mut().insert(Default::default())
    }

    pub(super) fn new_signal(&self, network: NetworkId, value: Rc<RefCell<dyn Any>>) -> SignalId {
        let id = self.signals.borrow_mut().insert(value);
        if let Some(net) = self.networks.borrow().get(network) {
            net.borrow_mut().signals.insert(id);
        }
        id
    }

    pub(super) fn new_effect(&self, network: NetworkId, effect: Rc<dyn AnyEffect>) -> EffectId {
        let id = self.effects.borrow_mut().insert(effect);
        if let Some(net) = self.networks.borrow().get(network) {
            net.borrow_mut().effects.insert(id);
        }
        id
    }
}

pub fn run_until_idle() {
    with_runtime(|rt| {
        let mut dirty = rt.dirty.borrow_mut();
        loop {
            println!("========");
            let mut tracking = rt.tracking.borrow_mut();
            if tracking.is_empty() {
                break;
            }
            tracking.mark_dirty(&mut dirty);
            drop(tracking);
            dirty.run_clean(rt, &rt.tracking);
        }
    })
}

thread_local! {
    static RUNTIME: Runtime = Runtime::default();
}

pub(super) fn with_runtime<T>(f: impl FnOnce(&Runtime) -> T) -> T {
    RUNTIME.with(f)
}
