use crate::runtime::KindBehavior;

use super::prelude::*;

impl Network {
    #[track_caller]
    fn all_common<B, F, FBuild, Out>(
        &self,
        behaviors: B::Behaviors,
        init: Out,
        f_build: FBuild,
    ) -> Behavior<Out>
    where
        Out: Data,
        FBuild: FnOnce(Emitter<Out, KindBehavior>, B::Behaviors) -> F,
        F: Fn(Rt) + 'static,
        B: AsBehaviors,
    {
        let node = self.new_node().with_value(init);
        let emitter = node.emitter();
        let f = f_build(emitter, behaviors);
        let node = node.with_input_fn(move |rt, _: &dyn Data| f(rt));
        B::attach_dyn(self.rt(), &behaviors, node.consumer());
        node.behavior()
    }

    #[track_caller]
    pub fn all<B: AsBehaviors>(&self, behaviors: B) -> Behavior<B::Values> {
        let behaviors = behaviors.as_behaviors(self.rt());
        let Some(value) = B::try_value(self.rt(), &behaviors) else { return Behavior::null() };
        self.all_common::<B, _, _, _>(behaviors, value, |emitter, behaviors| {
            move |rt| {
                if let Some(values) = B::try_value(rt, &behaviors) {
                    emitter.emit_event(rt, &values);
                }
            }
        })
    }

    #[track_caller]
    pub fn all_vec<B: AsBehaviors>(
        &self,
        behaviors: B,
    ) -> Behavior<<B::Values as TupleAsArray>::Array>
    where
        B::Values: TupleAsArray,
        <B::Values as TupleAsArray>::Array: Clone,
    {
        let behaviors = behaviors.as_behaviors(self.rt());
        let Some(values) = B::try_value(self.rt(), &behaviors) else { return Behavior::null() };
        let init_vec = values.as_array();

        self.all_common::<B, _, _, _>(behaviors, init_vec, |emitter, behaviors| {
            move |rt| {
                if let Some(values) = B::try_value(rt, &behaviors) {
                    let vec = values.as_array();
                    emitter.emit_event(rt, &vec);
                }
            }
        })
    }
}
