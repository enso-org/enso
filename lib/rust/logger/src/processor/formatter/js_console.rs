//! JavaScript console formatter implementation.

use crate::prelude::*;

use crate::entry::level;
use crate::entry::GenericEntry;
use crate::processor::formatter;



// =================
// === JsConsole ===
// =================

/// A nicely looking, colorful, basic formatter for a JavaScript console.
#[derive(Clone, Copy, Debug, Default)]
pub struct JsConsole;

impl formatter::Output for JsConsole {
    type Output = js_sys::Array;
}

impl JsConsole {
    fn format_color(path: &str, color: Option<&str>, msg: String) -> js_sys::Array {
        let msg = format!("%c {} %c {}", path, msg).into();
        let css1 = "color:#ffffff;background:dimgray;border-radius:4px".into();
        let css2 = color.map(|c| iformat!("color:{c}")).unwrap_or_default().into();
        let arr = js_sys::Array::new();
        arr.push(&msg);
        arr.push(&css1);
        arr.push(&css2);
        arr
    }
}


// === Impls ===

impl formatter::Definition<level::Warning> for JsConsole {
    fn format(entry: &GenericEntry) -> Option<Self::Output> {
        entry
            .content
            .message()
            .map(|msg| Self::format_color(&entry.path, Some("orange"), format!("[W] {}", msg)))
    }
}

impl formatter::Definition<level::Error> for JsConsole {
    fn format(entry: &GenericEntry) -> Option<Self::Output> {
        entry
            .content
            .message()
            .map(|msg| Self::format_color(&entry.path, Some("orangered"), format!("[E] {}", msg)))
    }
}

impl<Level> formatter::Definition<Level> for JsConsole {
    default fn format(entry: &GenericEntry) -> Option<Self::Output> {
        entry.content.message().map(|msg| Self::format_color(&entry.path, None, msg.to_owned()))
    }
}
