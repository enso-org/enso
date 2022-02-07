//! FRP bindings to the animation engine.

pub mod delayed;
pub mod hysteretic;

use crate::prelude::*;

use crate::animation::physics::inertia;
use crate::data::mix;

use enso_frp as frp;



// =================
// === Animation ===
// =================

// crate::define_endpoints! { <T>
//     Input {
//         target    (f32),
//         precision (f32),
//         skip      (),
//     }
//     Output {
//         value  (f32),
//         on_end (inertia::EndStatus),
//     }
// }

/// Simulator used to run the animation.
pub type AnimationSimulator<T> = inertia::DynSimulator<mix::Repr<T>>;

/// Smart animation handler. Contains of dynamic simulation and frp endpoint. Whenever a new value
/// is computed, it is emitted via the endpoint.
#[derive(CloneRef, Derivative, Debug)]
#[derivative(Clone(bound = ""))]
#[allow(missing_docs)]
pub struct Animation<T: mix::Mixable + frp::Data> {
    pub target:    frp::Any<T>,
    pub precision: frp::Any<f32>,
    pub skip:      frp::Any,
    pub value:     frp::Stream<T>,
}

#[allow(missing_docs)]
impl<T: mix::Mixable + frp::Data> Animation<T>
where mix::Repr<T>: inertia::Value
{
    /// Constructor. The initial value of the animation is set to `default`.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            value_src <- any_mut::<T>();
        }
        let on_step = Box::new(f!((t) value_src.emit(mix::from_space::<T>(t))));
        let simulator = AnimationSimulator::<T>::new(on_step, (), ());
        // FIXME[WD]: The precision should become default and should be increased in all simulators
        //            that work with pixels. The reason is that by default the simulator should
        //            give nice results for animations in the range of 0 .. 1, while it should not
        //            make too many steps when animating bigger values (like pixels).
        simulator.set_precision(0.001);
        frp::extend! { network
            target    <- any_mut::<T>();
            precision <- any_mut::<f32>();
            skip      <- any_mut::<()>();
            eval  target    ((t) simulator.set_target_value(mix::into_space(t.clone())));
            eval  precision ((t) simulator.set_precision(*t));
            eval_ skip      (simulator.skip());
        }
        let value = value_src.into();
        network.store(&simulator);
        Self { target, precision, skip, value }
    }

    /// Constructor. The initial value is provided explicitly.
    pub fn new_with_init(network: &frp::Network, init: T) -> Self {
        let this = Self::new(network);
        this.target.emit(init);
        this.skip.emit(());
        this
    }

    /// Constructor. There is no initial value. The first emitted `target` value will be used
    /// without animation.
    pub fn new_non_init(network: &frp::Network) -> Self {
        let this = Self::new(network);
        frp::extend! { network
            init      <- any_mut();
            on_init   <- this.target.gate_not(&init);
            init      <+ this.target.constant(true);
            this.skip <+ on_init.constant(());
        }
        this
    }
}



// ============================
// === DEPRECATED Animation ===
// ============================

/// Smart animation handler. Contains of dynamic simulation and frp endpoint. Whenever a new value
/// is computed, it is emitted via the endpoint.
///
/// # DEPRECATION
/// This component is deprecated. Use `Animation` instead, which exposes much more FRP-oriented API
/// than this component. The transition to new version should be straightforward but requires some
/// attention. The functionalities should be the same and should be accessible by similar API with
/// two differences:
///
/// 1. The API bases on FRP now, which means that the usage can probably be refactored to look
///    much nicer.
///
/// 2. After setting the value for the first time, the value is provided as the output value without
///    any animation. This is different behavior from the previous implementation, where even
///    setting the value for the first time would create animation between `default()` value and the
///    new target. If your code depends on this behavior, it needs to be changed.
#[derive(CloneRef, Derivative, Debug, Shrinkwrap)]
#[derivative(Clone(bound = ""))]
#[allow(missing_docs)]
#[allow(non_camel_case_types)]
pub struct DEPRECATED_Animation<T: mix::Mixable> {
    #[shrinkwrap(main_field)]
    pub simulator: inertia::DynSimulator<T::Repr>,
    pub value:     frp::Stream<T>,
}

#[allow(missing_docs)]
impl<T: mix::Mixable + frp::Data> DEPRECATED_Animation<T>
where mix::Repr<T>: inertia::Value
{
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            def target = source::<T>();
        }
        let on_step = Box::new(f!((t) target.emit(mix::from_space::<T>(t))));
        let simulator = inertia::DynSimulator::<T::Repr>::new(on_step, (), ());
        let value = target.into();
        Self { simulator, value }
    }

    pub fn set_value(&self, value: T) {
        let animation_space_repr = value.into();
        self.simulator.set_value(animation_space_repr.value);
    }

    pub fn value(&self) -> T {
        let value = self.simulator.value();
        mix::from_space(value)
    }

    pub fn set_target_value(&self, target_value: T) {
        let state: mix::Space<_> = target_value.into();
        self.simulator.set_target_value(state.value);
    }

    pub fn target_value(&self) -> T {
        let value = self.simulator.target_value();
        mix::from_space(value)
    }
}
