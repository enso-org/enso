//! Style watch utility. Allows querying of the current style for information.

use crate::prelude::*;

use crate::control::callback;
use crate::data::color;
use crate::display::style;
use crate::display::style::data::DataMatch;
use crate::display::style::Path;

use enso_frp as frp;



// =================
// === Constants ===
// =================

/// Key that is used to look for a dim variant of a color in the theme.
const THEME_KEY_DIMMED: &str = "dimmed";
const VARIANT_PATH_PREFIX: &str = "variant";
// TODO[MM]: Replace with `theme::..` syntax. Right now this can't be done though, as this would
// require a cyclic import from the `ensogl-theme` crate.
const COLOR_LIGHTNESS_FACTOR_PATH: &str = "theme.vars.colors.dimming.lightness_factor";
const COLOR_CHROMA_FACTOR_PATH: &str = "theme.vars.colors.dimming.chroma_factor";
const FALLBACK_COLOR: color::Rgba = color::Rgba::new(1.0, 0.0, 0.0, 0.5);



// =====================
// === StyleWatchFrp ===
// =====================

/// FRP-based style watch utility. Whenever a style sheet value is accessed, the value reference is
/// being remembered and tracked. Whenever it changes, the FRP event is emitted.
#[derive(Clone, CloneRef, Derivative)]
#[derivative(Debug)]
pub struct StyleWatchFrp {
    network:  frp::Network,
    sheet:    style::Sheet,
    vars:     Rc<RefCell<Vec<style::Var>>>,
    handles:  Rc<RefCell<Vec<callback::Handle>>>,
    #[derivative(Debug = "ignore")]
    callback: Rc<RefCell<Box<dyn Fn()>>>,
}

impl StyleWatchFrp {
    /// Constructor.
    #[allow(trivial_casts)]
    pub fn new(sheet: &style::Sheet) -> Self {
        let network = frp::Network::new("style_watch");
        let sheet = sheet.clone_ref();
        let vars = default();
        let handles = default();
        let callback = Rc::new(RefCell::new(Box::new(|| {}) as Box<dyn Fn()>));
        Self { network, sheet, vars, handles, callback }
    }

    fn get_internal(
        &self,
        path: impl Into<Path>,
    ) -> (frp::Source<Option<style::Data>>, Option<style::Data>) {
        let network = &self.network;
        frp::extend! { network
            source <- source::<Option<style::Data>>();
        }
        let path = path.into();
        let var = self.sheet.var(path);
        let current = var.value();
        let handle = var.on_change(f!((data:&Option<style::Data>) source.emit(data.clone())));
        self.vars.borrow_mut().push(var);
        self.handles.borrow_mut().push(handle);
        (source, current)
    }

    /// Queries style sheet value for a value.
    pub fn get(&self, path: impl Into<Path>) -> frp::Sampler<Option<style::Data>> {
        let network = &self.network;
        let (source, current) = self.get_internal(path);
        frp::extend! { network
            sampler <- source.sampler();
        }
        source.emit(current);
        sampler
    }

    /// Queries style sheet for a generic type. Emits a warning and returns a fallback value if not
    /// found.
    pub fn access<T: ThemeAccess>(&self, path: impl Into<Path>) -> frp::Sampler<T> {
        let path = path.into();
        let network = &self.network;
        let path_str = path.to_string();
        let (source, current) = self.get_internal(path);
        frp::extend! { network
            sampler <- source.map(move |t| T::from_style_data(&path_str, t)).sampler();
        }
        source.emit(current);
        sampler
    }

    /// Queries style sheet value for a number. Emits a warning and returns 0.0 if not found.
    pub fn get_number(&self, path: impl Into<Path>) -> frp::Sampler<f32> {
        self.access(path)
    }

    /// Queries style sheet color, if not found fallbacks to [`FALLBACK_COLOR`] and emits a warning.
    pub fn get_color(&self, path: impl Into<Path>) -> frp::Sampler<color::Rgba> {
        self.access(path)
    }

    /// Queries style sheet color, if not found fallbacks to [`FALLBACK_COLOR`] and emits a warning.
    pub fn get_color_lcha(&self, path: impl Into<Path>) -> frp::Sampler<color::Lcha> {
        self.access(path)
    }

    /// Queries the style sheet for a text. Emits a warning and returns empty string if not found.
    pub fn get_text(&self, path: impl Into<Path>) -> frp::Sampler<ImString> {
        self.access(path)
    }

    /// Queries style sheet number.
    pub fn get_number_or(&self, path: impl Into<Path>, fallback: f32) -> frp::Sampler<f32> {
        let network = &self.network;
        let (source, current) = self.get_internal(path);
        frp::extend! { network
            sampler <- source.map(move |t| t.number().unwrap_or(fallback)).sampler();
        }
        source.emit(current);
        sampler
    }
}

/// Defines a way for a value of given type to be accessed from the style sheet.
pub trait ThemeAccess: Debug + Clone + Default + 'static {
    /// Convert raw style data to a value of given type. Uses `path_str` to report a warning in case
    /// of an unexpected data type.
    fn from_style_data(path_str: &str, data: &Option<style::Data>) -> Self;
}

impl ThemeAccess for f32 {
    fn from_style_data(path_str: &str, data: &Option<style::Data>) -> Self {
        data.number().unwrap_or_else(|| {
            warn!("Tried to access undefined number from theme: {path_str}");
            0.0
        })
    }
}

impl ThemeAccess for Vector2 {
    fn from_style_data(path_str: &str, data: &Option<style::Data>) -> Self {
        data.vector().unwrap_or_else(|| {
            warn!("Tried to access undefined vector from theme: {path_str}");
            default()
        })
    }
}

impl ThemeAccess for ImString {
    fn from_style_data(path_str: &str, data: &Option<style::Data>) -> Self {
        data.im_string_or_else(|| {
            warn!("Tried to access undefined text from theme: {path_str}");
            default()
        })
    }
}

impl ThemeAccess for color::Rgba {
    fn from_style_data(path_str: &str, data: &Option<style::Data>) -> Self {
        data.color().unwrap_or_else(|| {
            warn!("Tried to access undefined color from theme: {path_str}");
            FALLBACK_COLOR
        })
    }
}

impl ThemeAccess for color::Lcha {
    fn from_style_data(path_str: &str, data: &Option<style::Data>) -> Self {
        <color::Rgba as ThemeAccess>::from_style_data(path_str, data).into()
    }
}

// ==================
// === StyleWatch ===
// ==================

/// Style watch utility. It's reference is passed to shapes defined with the `shape`
/// macro. Whenever a style sheet value is accessed, the value reference is being remembered and
/// tracked. Whenever it changes, the `callback` runs. The callback should trigger shape redraw.
#[derive(Clone, CloneRef, Debug)]
pub struct StyleWatch {
    data: Rc<RefCell<StyleWatchData>>,
}

#[derive(Derivative)]
#[derivative(Debug)]
struct StyleWatchData {
    sheet:    style::Sheet,
    vars:     HashSet<style::Var>,
    handles:  Vec<callback::Handle>,
    #[derivative(Debug = "ignore")]
    callback: Rc<dyn Fn()>,
}

impl StyleWatch {
    /// Constructor.
    #[allow(trivial_casts)]
    pub fn new(sheet: &style::Sheet) -> Self {
        let sheet = sheet.clone_ref();
        let vars = default();
        let handles = default();
        let callback = Rc::new(|| {}) as Rc<dyn Fn()>;
        let data = StyleWatchData { sheet, vars, handles, callback };
        Self { data: Rc::new(RefCell::new(data)) }
    }

    /// Resets the state of style manager. Should be used on each new shape definition. It is
    /// called automatically when used by `shape`.
    pub fn reset(&self) {
        let mut data = self.data.borrow_mut();
        data.vars = default();
        data.handles = default();
    }

    /// Sets the callback which will be used when dependent styles change.
    pub fn set_on_style_change<F: 'static + Fn()>(&self, callback: F) {
        self.data.borrow_mut().callback = Rc::new(callback);
    }

    /// Queries style sheet value for a value.
    pub fn get(&self, path: impl Into<Path>) -> Option<style::Data> {
        let mut data = self.data.borrow_mut();
        let path = path.into();
        let var = data.sheet.var(path);
        let value = var.value();
        if !data.vars.contains(&var) {
            let callback = data.callback.clone_ref();
            let handle = var.on_change(move |_: &Option<style::Data>| callback());
            data.vars.insert(var);
            data.handles.push(handle);
        }
        value
    }

    /// Queries style sheet number value, if not found gets fallback.
    pub fn get_number_or(&self, path: impl Into<Path>, fallback: f32) -> f32 {
        self.get(path).number().unwrap_or(fallback)
    }

    /// Queries style sheet number value, if not found computes it from a closure.
    pub fn get_number_or_else<F>(&self, path: impl Into<Path>, fallback: F) -> f32
    where F: FnOnce() -> f32 {
        self.get(path).number().unwrap_or_else(fallback)
    }

    /// Queries style sheet number value. Returns 0 if not found.
    pub fn get_number(&self, path: impl Into<Path>) -> f32 {
        let path = path.into();
        self.get(path.clone()).number().unwrap_or_else(|| {
            warn!("Tried to access undefined number from theme: {}", path);
            0.0
        })
    }

    /// A debug check of how many stylesheet variables are registered in this style watch.
    pub fn debug_var_count(&self) -> usize {
        self.data.borrow().vars.len()
    }
}



// ====================
// === Color Styles ===
// ====================

impl StyleWatch {
    /// Queries style sheet color, if not found fallbacks to [`FALLBACK_COLOR`].
    pub fn get_color<T: Into<Path>>(&self, path: T) -> color::Rgba {
        self.get(path).color().unwrap_or(FALLBACK_COLOR)
    }

    // /// Return the dimmed version for either a `Path` or a specific color.
    // pub fn get_color_dim<T:Into<Path>>(&self, path:T) -> color::Rgba {
    //     self.get_color_from_path_dim(path)
    // }

    // /// Queries style sheet color, if not found fallbacks to red.
    // fn get_color_from_path_dim<T:Into<Path>>(&self, path:T) -> color::Lcha {
    //     let path = path.into();
    //     match self.try_get_color_variant(path.clone(),THEME_KEY_DIMMED) {
    //         None        => {
    //             let base_color = self.get_color(path);
    //             self.make_color_dim(base_color)
    //         },
    //         Some(color) => color,
    //     }
    // }
    //
    // /// Create a dimmed version of the given color value. The exact values to be used for dimming
    // /// are derived from the theme.
    // fn make_color_dim<T:Into<color::Lcha>+From<color::Lcha>>(&self, color:T) -> T {
    //     let color : color::Lcha    = color.into();
    //     let color_lightness_factor = self.get_number_or(COLOR_LIGHTNESS_FACTOR_PATH, 0.0);
    //     let color_chroma_factor    = self.get_number_or(COLOR_CHROMA_FACTOR_PATH, 0.0);
    //     let lightness              = color.lightness * color_lightness_factor;
    //     let chroma                 = color.chroma * color_chroma_factor;
    //     let color                  = color::Lcha::new(lightness,chroma,color.hue,color.alpha);
    //     color.into()
    // }

    /// Return the path where we look for alternative shades or scheme variants of a color in the
    /// theme (for example, "dimmed").
    fn color_variant_path(path: Path, extension: String) -> Path {
        let segments_rev = path.rev_segments;
        let mut segments = segments_rev.into_iter().rev().collect_vec();
        segments.pop();
        segments.push(VARIANT_PATH_PREFIX.to_string());
        segments.push(extension);
        Path::from_segments(segments)
    }

    fn try_get_color_variant<T: Into<Path>>(&self, path: T, id: &str) -> Option<color::Rgba> {
        let path = Self::color_variant_path(path.into(), id.to_string());
        self.get(path).color()
    }
}
