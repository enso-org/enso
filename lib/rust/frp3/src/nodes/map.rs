use super::prelude::*;

impl Network {
    #[track_caller]
    // Special kind of mapper that calls a method, but returns nothing.
    pub fn eval<T, F>(&self, src: T, f: F)
    where
        T: AsStream,
        F: Fn(&T::Event) + 'static,
    {
        self.new_node()
            .with_input_fn(move |_, event| f(event))
            .with_attached(src.as_stream());
    }

    #[track_caller]
    pub fn map<F, Out, E>(&self, ev: E, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, (), Out, Out, AsOwn, MapHandler<F>>,
    {
        self.map_with(ev, (), f)
    }

    #[track_caller]
    pub fn map_ref<F, Out, E>(&self, ev: E, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, (), Out, Out, AsRef, MapHandler<F>>,
    {
        self.map_with_ref(ev, (), f)
    }

    #[track_caller]
    pub fn filter_map<F, Out, E>(&self, ev: E, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, (), Out, Option<Out>, AsOwn, MapHandler<F>>,
    {
        self.filter_map_with(ev, (), f)
    }
    #[track_caller]
    pub fn filter_map_ref<F, Out, E>(&self, ev: E, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, (), Out, Option<Out>, AsOptRef, MapHandler<F>>,
    {
        self.filter_map_with_ref(ev, (), f)
    }

    #[track_caller]
    pub fn map_with<F, Out, E, B>(&self, ev: E, behaviors: B, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, B, Out, Out, AsOwn, MapHandler<F>>,
    {
        MapNode::new(self, ev, behaviors, MapHandler(f), |rt, emitter, value| {
            emitter.emit_event(rt, &value)
        })
    }

    #[track_caller]
    pub fn map_with_ref<F, Out, E, B>(&self, ev: E, behaviors: B, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, B, Out, Out, AsRef, MapHandler<F>>,
    {
        MapNode::new(self, ev, behaviors, MapHandler(f), |rt, emitter, value| {
            emitter.emit_event(rt, value)
        })
    }

    #[track_caller]
    pub fn filter_map_with<F, Out, E, B>(&self, ev: E, behaviors: B, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, B, Out, Option<Out>, AsOwn, MapHandler<F>>,
    {
        MapNode::new(self, ev, behaviors, MapHandler(f), |rt, emitter, value| {
            value.map(|v| emitter.emit_event(rt, &v));
        })
    }

    #[track_caller]
    pub fn filter_map_with_ref<F, Out, E, B>(&self, ev: E, behaviors: B, f: F) -> Stream<Out>
    where
        Out: Data,
        MapNode: MapImpl<E, B, Out, Option<Out>, AsOptRef, MapHandler<F>>,
    {
        MapNode::new(self, ev, behaviors, MapHandler(f), |rt, emitter, value| {
            value.map(|v| emitter.emit_event(rt, v));
        })
    }
}

pub trait RefTy<'a, T> {
    type V;
}

pub enum AsOwn {}
pub enum AsRef {}
pub enum AsOptRef {}

impl<'a, T: 'static> RefTy<'a, T> for AsRef {
    type V = &'a T;
}

impl<'a, T: 'static> RefTy<'a, T> for AsOwn {
    type V = T;
}

impl<'a, T: 'static> RefTy<'a, Option<T>> for AsOptRef {
    type V = Option<&'a T>;
}

pub enum MapNode {}

pub trait MapImpl<Event, B, Out, FnOut, Mode, H>
where
    Mode: for<'a> RefTy<'a, FnOut>,
{
    #[track_caller]
    fn new(
        network: &Network,
        ev: Event,
        behaviors: B,
        h: H,
        f: impl Fn(Rt, Emitter<Out>, <Mode as RefTy<FnOut>>::V) + 'static,
    ) -> Stream<Out>;
}

impl<E, B, Out, FnOut, Mode, H> MapImpl<E, B, Out, FnOut, Mode, H> for MapNode
where
    Out: Data,
    E: AsStream,
    B: AsBehaviors,
    H: 'static,
    Mode: for<'a> RefTy<'a, FnOut>,
    H: HandlerImpl<FnOut, Mode, E::Event, B::Rcs>,
    B::Rcs: std::fmt::Debug,
{
    #[track_caller]
    fn new(
        network: &Network,
        ev: E,
        behaviors: B,
        h: H,
        f: impl Fn(Rt, Emitter<Out>, <Mode as RefTy<FnOut>>::V) + 'static,
    ) -> Stream<Out> {
        let ev = ev.as_stream();
        let behaviors = behaviors.as_behaviors(network.rt());

        let node = network.new_node().with_output();
        let emitter = node.emitter();
        let node = node.with_input_fn(move |rt, v1| {
            if let Some(vals) = B::get(rt, &behaviors) {
                h.handle(v1, vals, |v| f(rt, emitter, v))
            }
        });
        node.with_attached(ev).stream()
    }
}

pub struct MapHandler<F>(F);
pub trait HandlerImpl<Out, Mode, Ev, Beh>
where
    Ev: ?Sized,
    Mode: for<'a> RefTy<'a, Out>,
{
    fn handle(&self, ev: &Ev, bs: Beh, f: impl Fn(<Mode as RefTy<'_, Out>>::V));
}

macro_rules! impl_all_node_tuple {
    ($_:ident $($t:ident)*) => {

        impl<Out, Event, Mode,Func, $($t,)*> HandlerImpl<Out, Mode, Event, ($(BumpRc<$t>,)*)> for MapHandler<Func>
        where
            Out: Data,
            Event: ?Sized,
            for<'a> Mode: RefTy<'a, Out>,
            for<'a> Func: Fn(&'a Event $(, &'a $t)*) -> <Mode as RefTy<'a, Out>>::V,
        {
            #[allow(non_snake_case)]
            fn handle(&self, ev: &Event, bs: ($(BumpRc<$t>,)*), f: impl Fn(<Mode as RefTy<'_, Out>>::V)) {
                let ($($t,)*) = bs;
                f((&self.0)(ev $(, &$t)*))
            }
        }
    };
}
impl_tuples!(f = [[impl_all_node_tuple]]);

// ==================
// === Compat API ===
// ==================

// Implement map2, map3, etc., where each behavior is a separate argument.
// That way we can remain compatible with old API.
macro_rules! impl_map_compat {
    (
        forward=[[ [$name:ident $($_:tt)*] $($_1:tt)* ]]
        reverse=[[ $([$_2:ident $src:ident $ty:ident])* ]]
    ) => {
        #[track_caller]
        pub fn $name<Func, Out, Event $(, $ty)*>(
            &self,
            src1: Event,
            $($src: $ty,)*
            f: Func
        ) -> Stream<Out>
        where
            Out: Data,
            $($ty: AsBehavior, )*
            MapNode: MapImpl<Event, ($($ty,)*), Out, Out, AsOwn, MapHandler<Func>>,
        {
            self.map_with(src1, ($($src,)*), f)
        }
    };
}

impl Network {
    _impl_tuples! {
        f=[[_impl_tuples_both_ways]]
        args=[[
            f=[[impl_map_compat]]
        ]]
        [map7 src7 F]
        [map6 src6 E]
        [map5 src5 D]
        [map4 src4 C]
        [map3 src3 B]
        [map2 src2 A]
    }
}
