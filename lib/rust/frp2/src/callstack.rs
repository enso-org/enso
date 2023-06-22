//! Call stack tracing for FRP events propagation. Used for debugging purposes only. It is fully
//! optimized away if the `stack-trace` feature is not enabled.

// === Non-Standard Linter Configuration ===
#![allow(dead_code)]
#![allow(missing_docs)]

use crate::prelude::*;



// FIXME: This code is not finished, to be finished in the next PR, issue:
// https://github.com/enso-org/enso/issues/7043


#[derive(Clone, Copy, Debug, Zeroable)]
pub struct Label(#[cfg(feature = "stack-trace")] &'static str);

#[derive(Clone, Copy, Debug, Default, Zeroable)]
pub struct Hidden(#[cfg(feature = "stack-trace")] bool);

impl Hidden {
    fn new() -> Self {
        Self(
            #[cfg(feature = "stack-trace")]
            true,
        )
    }
}

#[allow(clippy::derivable_impls)]
impl Default for Label {
    fn default() -> Self {
        Self(
            #[cfg(feature = "stack-trace")]
            "unnamed",
        )
    }
}

impl From<&'static str> for Label {
    #[inline(always)]
    fn from(_value: &'static str) -> Self {
        Self(
            #[cfg(feature = "stack-trace")]
            _value,
        )
    }
}

impl Display for Label {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(feature = "stack-trace")]
        _f.write_str(self.0)?;

        Ok(())
    }
}

#[cfg(feature = "stack-trace")]
#[derive(Clone, Copy, Debug, Default, Zeroable)]
pub struct Location(u32, ZeroableStaticStr);

#[cfg(not(feature = "stack-trace"))]
#[derive(Clone, Copy, Debug, Default, Zeroable)]
pub struct Location();

impl Location {
    #[track_caller]
    #[inline(always)]
    pub fn caller_at_line(_line: u32) -> Self {
        #[cfg(feature = "stack-trace")]
        {
            Self(_line, std::panic::Location::caller().file())
        }
        #[cfg(not(feature = "stack-trace"))]
        {
            Self()
        }
    }

    #[track_caller]
    #[inline(always)]
    pub fn caller() -> Self {
        #[cfg(feature = "stack-trace")]
        {
            let caller = std::panic::Location::caller();
            Self(caller.line(), caller.file())
        }
        #[cfg(not(feature = "stack-trace"))]
        {
            Self()
        }
    }
}

impl Display for Location {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(feature = "stack-trace")]
        write!(_f, "{}:{}", self.1, self.0)?;

        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Default, Zeroable)]
pub struct DefInfo {
    pub label:    Label,
    pub location: Location,
    pub hidden:   Hidden,
}

impl Label {
    #[inline(always)]
    pub fn dropped() -> Self {
        "<DROPPED>".into()
    }

    #[inline(always)]
    pub fn auto_sampler() -> Self {
        "auto-sampler".into()
    }
}

impl DefInfo {
    #[track_caller]
    #[inline(always)]
    pub fn labelled(line_number: u32, label: Label) -> Self {
        Self { label, location: Location::caller_at_line(line_number), hidden: default() }
    }

    #[track_caller]
    #[inline(always)]
    pub fn emit() -> Self {
        Self::labelled(std::panic::Location::caller().line(), "emit".into())
    }

    #[track_caller]
    #[inline(always)]
    pub fn unlabelled() -> Self {
        Self { label: Label::default(), location: Location::caller(), hidden: default() }
    }

    #[track_caller]
    #[inline(always)]
    pub fn hidden() -> Self {
        Self { label: Label::default(), location: Location::caller(), hidden: Hidden::new() }
    }
}

// =================
// === CallStack ===
// =================
/// A call stack trace for FRP events.
#[derive(Debug, Default)]
pub struct CallStack {
    _frames: RefCell<Vec<DefInfo>>,
}

impl CallStack {
    #[inline(always)]
    pub fn push(&self, _def: DefInfo) {
        #[cfg(feature = "stack-trace")]
        self._frames.borrow_mut().push(_def);
    }

    #[inline(always)]
    pub fn pop(&self) {
        #[cfg(feature = "stack-trace")]
        self._frames.borrow_mut().pop();
    }

    #[inline(always)]
    pub fn with<T>(&self, def: DefInfo, f: impl FnOnce() -> T) -> T {
        self.push(def);
        let result = f();
        self.pop();
        result
    }
}

impl Display for CallStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(feature = "stack-trace")]
        {
            const LOTS_OF_SPACES: &str = "                                ";
            let max_len =
                self._frames.borrow().iter().map(|def| def.label.0.len()).max().unwrap_or(0);

            f.write_str("      Stack trace:\n")?;
            for def in self._frames.borrow().iter().rev() {
                if def.hidden.0 {
                    continue;
                }

                let label = def.label.0;
                let Location(line, file) = def.location;
                let pad = &LOTS_OF_SPACES[0..(max_len - label.len()).min(LOTS_OF_SPACES.len())];
                write!(f, "        {label}{pad} at {file}:{line}\n",)?;
            }
            Ok(())
        }

        #[cfg(not(feature = "stack-trace"))]
        {
            f.write_str("      stack-trace feature disabled")
        }
    }
}
