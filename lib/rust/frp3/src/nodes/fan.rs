use crate::prelude::*;

use std::any::TypeId;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::runtime::with_runtime;
use crate::runtime::DynStream;
use crate::runtime::WeakNetwork;
use crate::AnyRelaxed;

use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn fan(&self) -> Fan {
        let any = self.any_mut_relaxed::<dyn Data>();
        let outputs = any.alloc(self.rt(), default());
        let net = self.downgrade();
        Fan { net, any, outputs }
    }
}

#[derive(Debug, Clone)]
pub struct Fan {
    net:     WeakNetwork,
    any:     AnyRelaxed<dyn Data>,
    outputs: BumpRc<RefCell<HashMap<TypeId, DynStream>>>,
}

impl Fan {
    pub fn attach_in_rt<S: AsStream>(&self, rt: Rt, src: S) {
        let dyn_src = src.as_stream().into_dyn();
        self.any.attach_in_rt(rt, dyn_src)
    }

    pub fn attach<S: AsStream>(&self, src: S) {
        with_runtime(|rt| self.attach_in_rt(rt, src))
    }

    #[track_caller]
    pub fn emit_in_rt<T: ?Sized + Data>(&self, rt: Rt, event: &T) {
        self.any.emit_in_rt(rt, event.as_dyn())
    }

    #[track_caller]
    pub fn emit<T: ?Sized + Data>(&self, event: &T) {
        self.any.emit(event.as_dyn())
    }

    #[track_caller]
    pub fn output<T: Data>(&self) -> Stream<T> {
        use std::collections::hash_map::Entry;
        let mut outputs = self.outputs.borrow_mut();
        match outputs.entry(TypeId::of::<T>()) {
            Entry::Occupied(e) => e.get().downcast().expect("invalid fan output stream type"),
            Entry::Vacant(e) =>
                if let Some(net) = with_runtime(|rt| self.net.upgrade(rt)) {
                    let stream: Stream<T> = net
                        .filter_map_ref(self.any, |dyn_data| dyn_data.as_any().downcast_ref::<T>());
                    e.insert(stream.into_dyn_checked());
                    stream
                } else {
                    Stream::null()
                },
        }
    }
}

impl AsStream for Fan {
    type Event = dyn Data;

    fn as_stream(&self) -> Stream<dyn Data> {
        self.any.as_stream()
    }
}
