//! A single entry in [`crate::list_view::ListView`].
pub mod list;

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_hardcoded_theme as theme;
use ensogl_text as text;



// =================
// === Constants ===
// =================

/// Padding inside entry in pixels.
pub const PADDING: f32 = 14.0;
/// The overall entry's height (including padding).
pub const HEIGHT: f32 = 30.0;



// ==================================
// === Type Aliases and Reexports ===
// ==================================

/// Entry id. 0 is the first entry in component.
pub type Id = usize;

pub use list::List;



// =============
// === Trait ===
// =============

/// An object which can be entry in [`crate::ListView`] component.
///
/// The entries should not assume any padding - it will be granted by ListView itself. The Display
/// Object position of this component is docked to the middle of left entry's boundary. It differs
/// from usual behaviour of EnsoGl components, but makes the entries alignment much simpler.
///
/// This trait abstracts over model and its updating in order to support re-using shapes and gui
/// components, so they are not deleted and created again. The ListView component does not create
/// Entry object for each entry provided, and during scrolling, the instantiated objects will be
/// reused: they position will be changed and they will be updated using `update` method.
pub trait Entry: CloneRef + Debug + display::Object + 'static {
    /// The model of this entry. The entry should be a representation of data from the Model.
    /// For example, the entry being just a caption can have [`String`] as its model - the text to
    /// be displayed.
    type Model: Debug + Default;

    /// An Object constructor.
    fn new(app: &Application) -> Self;

    /// Update content with new model.
    fn update(&self, model: &Self::Model);

    /// Set the layer of all [`text::Area`] components inside. The [`text::Area`] component is
    /// handled in a special way, and is often in different layer than shapes. See TODO comment
    /// in [`text::Area::add_to_scene_layer`] method.
    fn set_label_layer(&self, label_layer: &display::scene::Layer);
}


// =======================
// === Implementations ===
// =======================

// === Label ===

/// The [`Entry`] being a single text field displaying String.
#[derive(Clone, CloneRef, Debug)]
pub struct Label {
    display_object: display::object::Instance,
    label:          text::Area,
    network:        enso_frp::Network,
    style_watch:    StyleWatchFrp,
}

impl Entry for Label {
    type Model = String;

    fn new(app: &Application) -> Self {
        let logger = Logger::new("list_view::entry::Label");
        let display_object = display::object::Instance::new(logger);
        let label = app.new_view::<ensogl_text::Area>();
        let network = frp::Network::new("list_view::entry::Label");
        let style_watch = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let color = style_watch.get_color(theme::widget::list_view::text);
        let size = style_watch.get_number(theme::widget::list_view::text::size);

        display_object.add_child(&label);
        frp::extend! { network
            init <- source::<()>();
            color <- all(&color,&init)._0();
            size  <- all(&size,&init)._0();

            label.set_default_color     <+ color;
            label.set_default_text_size <+ size.map(|v| text::Size(*v));
            eval size ((size) label.set_position_y(size/2.0));
        }
        init.emit(());
        Self { display_object, label, network, style_watch }
    }

    fn update(&self, model: &Self::Model) {
        self.label.set_content(model);
    }

    fn set_label_layer(&self, label_layer: &display::scene::Layer) {
        self.label.add_to_scene_layer(label_layer);
    }
}

impl display::Object for Label {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}


// === HighlightedLabel ===

/// The model for [`HighlightedLabel`], being an entry displayed as a single label with highlighted
/// some parts of text.
#[derive(Clone, Debug, Default)]
pub struct GlyphHighlightedLabelModel {
    /// Displayed text.
    pub label:       String,
    /// A list of ranges of highlighted bytes.
    pub highlighted: Vec<text::Range<text::Bytes>>,
}

/// The [`Entry`] similar to the [`Label`], but allows highlighting some parts of text.
#[derive(Clone, CloneRef, Debug)]
pub struct GlyphHighlightedLabel {
    inner:     Label,
    highlight: frp::Source<Vec<text::Range<text::Bytes>>>,
}

impl Entry for GlyphHighlightedLabel {
    type Model = GlyphHighlightedLabelModel;

    fn new(app: &Application) -> Self {
        let inner = Label::new(app);
        let network = &inner.network;
        let highlight_color =
            inner.style_watch.get_color(theme::widget::list_view::text::highlight);
        let label = &inner.label;

        frp::extend! { network
            highlight <- source::<Vec<text::Range<text::Bytes>>>();
            highlight_changed <- all(highlight,highlight_color);
            eval highlight_changed ([label]((highlight,color)) {
                for range in highlight {
                   label.set_color_bytes(range,color);
                }
            });
        }
        Self { inner, highlight }
    }

    fn update(&self, model: &Self::Model) {
        self.inner.update(&model.label);
        self.highlight.emit(&model.highlighted);
    }

    fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.inner.set_label_layer(layer);
    }
}

impl display::Object for GlyphHighlightedLabel {
    fn display_object(&self) -> &display::object::Instance {
        self.inner.display_object()
    }
}



// =======================
// === Model Providers ===
// =======================

// === The Trait ===

/// The Model Provider for ListView's entries of type `E`.
///
/// The [`crate::ListView`] component does not display all entries at once, instead it lazily ask
/// for models of entries when they're about to be displayed. So setting the select content is
/// essentially providing an implementor of this trait.
pub trait ModelProvider<E>: Debug {
    /// Number of all entries.
    fn entry_count(&self) -> usize;

    /// Get the model of entry with given id. The implementors should return `None` only when
    /// requested id greater or equal to entries count.
    fn get(&self, id: Id) -> Option<E::Model>
    where E: Entry;
}


// === AnyModelProvider ===

/// A wrapper for shared instance of some Provider of models for `E` entries.
#[derive(Debug, Shrinkwrap)]
pub struct AnyModelProvider<E>(Rc<dyn ModelProvider<E>>);

impl<E> Clone for AnyModelProvider<E> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<E> CloneRef for AnyModelProvider<E> {
    fn clone_ref(&self) -> Self {
        Self(self.0.clone_ref())
    }
}

impl<E> AnyModelProvider<E> {
    /// Create from typed provider.
    pub fn new<T: ModelProvider<E> + 'static>(provider: T) -> Self {
        Self(Rc::new(provider))
    }
}

impl<E, T: ModelProvider<E> + 'static> From<Rc<T>> for AnyModelProvider<E> {
    fn from(provider: Rc<T>) -> Self {
        Self(provider)
    }
}

impl<E> Default for AnyModelProvider<E> {
    fn default() -> Self {
        Self::new(EmptyProvider)
    }
}


// === EmptyProvider ===

/// An Entry Model Provider giving no entries.
///
/// This is the default provider for new select components.
#[derive(Clone, CloneRef, Copy, Debug)]
pub struct EmptyProvider;

impl<E> ModelProvider<E> for EmptyProvider {
    fn entry_count(&self) -> usize {
        0
    }
    fn get(&self, _: usize) -> Option<E::Model>
    where E: Entry {
        None
    }
}


// === ModelProvider for Vectors ===

impl<E, T> ModelProvider<E> for Vec<T>
where
    E: Entry,
    T: Debug + Clone + Into<E::Model>,
{
    fn entry_count(&self) -> usize {
        self.len()
    }

    fn get(&self, id: usize) -> Option<E::Model> {
        Some(<[T]>::get(self, id)?.clone().into())
    }
}


// === SingleMaskedProvider ===

/// An Entry Model Provider that wraps a `AnyModelProvider` and allows the masking of a single item.
#[derive(Clone, Debug)]
pub struct SingleMaskedProvider<E> {
    content: AnyModelProvider<E>,
    mask:    Cell<Option<Id>>,
}

impl<E: Debug> ModelProvider<E> for SingleMaskedProvider<E> {
    fn entry_count(&self) -> usize {
        match self.mask.get() {
            None => self.content.entry_count(),
            Some(_) => self.content.entry_count().saturating_sub(1),
        }
    }

    fn get(&self, ix: usize) -> Option<E::Model>
    where E: Entry {
        let internal_ix = self.unmasked_index(ix);
        self.content.get(internal_ix)
    }
}

impl<E> SingleMaskedProvider<E> {
    /// Return the index to the unmasked underlying data. Will only be valid to use after
    /// calling `clear_mask`.
    ///
    /// Transform index of an element visible in the menu, to the index of the all the objects,
    /// accounting for the removal of the selected item.
    ///
    /// Example:
    /// ```text
    /// Mask              `Some(1)`
    /// Masked indices    [0,     1, 2]
    /// Unmasked Index    [0, 1,  2, 3]
    /// -------------------------------
    /// Mask              `None`
    /// Masked indices    [0, 1, 2, 3]
    /// Unmasked Index    [0, 1, 2, 3]
    /// ```
    pub fn unmasked_index(&self, ix: Id) -> Id {
        match self.mask.get() {
            None => ix,
            Some(id) if ix < id => ix,
            Some(_) => ix + 1,
        }
    }

    /// Mask out the given index. All methods will now skip this item and the `SingleMaskedProvider`
    /// will behave as if it was not there.
    ///
    /// *Important:* The index is interpreted according to the _masked_ position of elements.
    pub fn set_mask(&self, ix: Id) {
        let internal_ix = self.unmasked_index(ix);
        self.mask.set(Some(internal_ix));
    }

    /// Mask out the given index. All methods will now skip this item and the `SingleMaskedProvider`
    /// will behave as if it was not there.
    ///
    /// *Important:* The index is interpreted according to the _unmasked_ position of elements.
    pub fn set_mask_raw(&self, ix: Id) {
        self.mask.set(Some(ix));
    }

    /// Clear the masked item.
    pub fn clear_mask(&self) {
        self.mask.set(None)
    }
}

impl<E> From<AnyModelProvider<E>> for SingleMaskedProvider<E> {
    fn from(content: AnyModelProvider<E>) -> Self {
        let mask = default();
        SingleMaskedProvider { content, mask }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_masked_provider() {
        let test_data = vec!["A", "B", "C", "D"];
        let test_models = test_data.into_iter().map(|label| label.to_owned()).collect_vec();
        let provider = AnyModelProvider::<Label>::new(test_models);
        let provider: SingleMaskedProvider<Label> = provider.into();

        assert_eq!(provider.entry_count(), 4);
        assert_eq!(provider.get(0).unwrap(), "A");
        assert_eq!(provider.get(1).unwrap(), "B");
        assert_eq!(provider.get(2).unwrap(), "C");
        assert_eq!(provider.get(3).unwrap(), "D");

        provider.set_mask_raw(0);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap(), "B");
        assert_eq!(provider.get(1).unwrap(), "C");
        assert_eq!(provider.get(2).unwrap(), "D");

        provider.set_mask_raw(1);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap(), "A");
        assert_eq!(provider.get(1).unwrap(), "C");
        assert_eq!(provider.get(2).unwrap(), "D");

        provider.set_mask_raw(2);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap(), "A");
        assert_eq!(provider.get(1).unwrap(), "B");
        assert_eq!(provider.get(2).unwrap(), "D");

        provider.set_mask_raw(3);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap(), "A");
        assert_eq!(provider.get(1).unwrap(), "B");
        assert_eq!(provider.get(2).unwrap(), "C");
    }
}
