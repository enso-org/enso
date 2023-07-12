use ensogl_core::display::shape::compound::rectangle::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::world::with_context;
use ensogl_core::Animation;



// ===================
// === Placeholder ===
// ===================

ensogl_core::define_endpoints_2! {
    Input {
        set_target_size (f32),
        skip_animation(),
    }
    Output {
        target_size(f32),
    }
}

#[derive(Debug, Clone, CloneRef, From)]
pub enum Placeholder {
    Strong(StrongPlaceholder),
    Weak(WeakPlaceholder),
}

impl Placeholder {
    pub fn new() -> Self {
        StrongPlaceholder::new().into()
    }

    pub fn new_with_size(size: f32) -> Self {
        StrongPlaceholder::new_with_size(size).into()
    }

    pub fn display_object(&self) -> Option<display::object::Instance> {
        match self {
            Self::Strong(p) => Some(p.display_object().clone_ref()),
            Self::Weak(p) => p.upgrade().map(|t| t.display_object().clone_ref()),
        }
    }

    pub fn is_strong(&self) -> bool {
        matches!(self, Self::Strong(_))
    }

    pub fn is_weak(&self) -> bool {
        matches!(self, Self::Weak(_))
    }

    pub fn target_size(&self) -> f32 {
        match self {
            Self::Strong(p) => p.target_size.value(),
            Self::Weak(p) => p.upgrade().map(|t| t.target_size.value()).unwrap_or_default(),
        }
    }

    pub fn exists(&self) -> bool {
        match self {
            Self::Strong(_) => true,
            Self::Weak(p) => p.upgrade().is_some(),
        }
    }

    pub fn upgrade(&self) -> Option<StrongPlaceholder> {
        match self {
            Self::Strong(p) => Some(p.clone_ref()),
            Self::Weak(p) => p.upgrade(),
        }
    }

    pub fn collapse(&mut self) {
        if let Self::Strong(p) = self {
            p.collapse();
            *self = Self::Weak(p.downgrade());
        }
    }
}

impl Default for Placeholder {
    fn default() -> Self {
        Self::new()
    }
}

/// A space holder for a list item. It is used to keep the list layout consistent when items are
/// added or removed.
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct StrongPlaceholder {
    model: Rc<PlaceholderModel>,
}

/// Internal model of [`StrongPlaceholder`].
#[allow(missing_docs)]
#[derive(Debug, Deref)]
pub struct PlaceholderModel {
    #[deref]
    pub frp:    Frp,
    root:       display::object::Instance,
    /// A self-reference used to keep the [`WeakPlaceholder`] alive until the collapsing animation
    /// ends.
    self_ref:   Rc<RefCell<Option<StrongPlaceholder>>>,
    collapsing: Rc<Cell<bool>>,
    size:       Animation<f32>,
    _deubg_viz: Option<Rectangle>,
}

impl PlaceholderModel {
    fn new() -> Self {
        let frp = Frp::new();
        let root = display::object::Instance::new_named("Placeholder");
        let self_ref = default();
        let collapsing = default();
        let size = Animation::<f32>::new(frp.network());
        let _deubg_viz = crate::DEBUG_PLACEHOLDERS_VIZ.then(|| {
            let viz = RoundedRectangle(10.0).build(|t| {
                t.set_size(Vector2::new(0.0, 10.0))
                    .allow_grow_x()
                    .set_color(color::Rgba::new(1.0, 0.0, 0.0, 0.3))
                    .set_border_and_inset(2.0)
                    .set_border_color(color::Rgba::new(1.0, 0.0, 0.0, 1.0));
            });
            root.add_child(&viz);
            with_context(|ctx| {
                ctx.layers.above_nodes.add(&viz);
            });
            viz
        });
        Self { frp, root, self_ref, collapsing, size, _deubg_viz }
    }
}

impl StrongPlaceholder {
    /// Constructor.
    pub fn new() -> Self {
        let model = PlaceholderModel::new();
        let model = Rc::new(model);
        let root = model.root.clone_ref();
        let collapsing = &model.collapsing;
        let self_ref = &model.self_ref;

        let network = &model.frp.network();
        let size = &model.size;
        size.simulator.update_spring(|s| s * crate::DEBUG_ANIMATION_SPRING_FACTOR);
        frp::extend! { network
            size.target <+ model.frp.private.input.set_target_size;
            size.skip <+ model.frp.private.input.skip_animation;
            model.frp.private.output.target_size <+ model.frp.private.input.set_target_size;

            eval size.value ((t) { root.set_size_x(*t); });
            eval_ size.on_end ([collapsing, self_ref] {
                if collapsing.get() {
                    self_ref.borrow_mut().take();
                }
            });
        }
        Self { model }
    }

    /// Constructor with a given size. The size will not be animated.
    pub fn new_with_size(size: f32) -> Self {
        let placeholder = Self::new();
        placeholder.set_target_size(size);
        placeholder.skip_animation();
        placeholder
    }

    /// Update the current placeholder size without affecting the target animation size.
    pub fn update_size(&self, f: impl FnOnce(f32) -> f32) {
        self.size.simulator.update_value(f);
    }

    /// Collapse the placeholder. Set the target size to 0 and keep a self reference until the size
    /// animation is running, so any weak instance of this placeholder will be kept alive.
    pub fn collapse(&self) {
        let model = self.model.clone();
        *self.self_ref.borrow_mut() = Some(StrongPlaceholder { model });
        self.set_target_size(0.0);
        self.collapsing.set(true);
    }

    /// Drop self-reference. This will cause the placeholder to be immediately removed if only weak
    /// references are kept alive.
    pub fn drop_self_ref(&self) {
        *self.self_ref.borrow_mut() = None;
    }

    /// Reuse the placeholder. This can be called after calling [`Self::collapse`] to undo its
    /// effect. In particular, after the size animation is finished, the self reference will not be
    /// removed.
    pub fn reuse(&self) {
        self.drop_self_ref();
        self.collapsing.set(false);
    }

    /// Downgrade the placeholder to a weak reference.
    pub fn downgrade(&self) -> WeakPlaceholder {
        WeakPlaceholder { model: Rc::downgrade(&self.model) }
    }
}

impl Default for StrongPlaceholder {
    fn default() -> Self {
        Self::new()
    }
}

impl display::Object for StrongPlaceholder {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}



// =======================
// === WeakPlaceholder ===
// =======================

/// Weak version of [`StrongPlaceholder`].
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct WeakPlaceholder {
    model: Weak<PlaceholderModel>,
}

impl WeakPlaceholder {
    /// Upgrade the weak placeholder to a strong reference.
    pub fn upgrade(&self) -> Option<StrongPlaceholder> {
        self.model.upgrade().map(|model| StrongPlaceholder { model })
    }
}
