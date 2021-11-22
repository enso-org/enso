//! Log formatter implementation.

pub mod js_console;
pub mod native_console;

pub use js_console::JsConsole;
pub use native_console::NativeConsole;

use crate::entry::Entry;
use crate::entry::GenericEntry;



// =========================
// === Default Formatter ===
// =========================

/// Default log formatter.
pub type Default = JsConsole;



// =================
// === Formatter ===
// =================

/// Output of a formatter as a dependent type of the formatter type. Each formatter defines its
/// output type. For example, formatters highly tailored for JavaScript console may output a special
/// console formatting values.
#[allow(missing_docs)]
pub trait Output {
    type Output;
}

/// A formatter allows formatting the incoming entry according to specific rules. The output is
/// optional, as not all entries need to be formatted. For example, some loggers might want to
/// display a visual indicator when  a group is closed, while others will use API for that.
///
/// ## WARNING
/// This trait should be implemented automatically. See the macro `define_levels_group` to learn
/// more.
#[allow(missing_docs)]
pub trait GenericDefinition<Level>: Output {
    fn generic_format(entry: &Entry<Level>) -> Option<Self::Output>;
}

/// A formatter narrowed to a specific type. While `Definition` can be parametrized with a generic
/// type, like `AllPossibleLevels`, this trait is parametrized with a specific level only, like
/// `level::Error`. Read docs of `Definition` to learn more.
#[allow(missing_docs)]
pub trait Definition<Level>: Output {
    fn format(entry: &GenericEntry) -> Option<Self::Output>;
}

/// Alias to `Definition::format` allowing providing the type parameters on call side.
pub fn format<Fmt, Level>(entry: &GenericEntry) -> Option<Fmt::Output>
where Fmt: Definition<Level> {
    <Fmt>::format(entry)
}
