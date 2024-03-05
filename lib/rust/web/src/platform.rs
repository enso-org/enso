//! This module provides helpers for platform specific logic.

use std::convert::TryFrom;



// ================
// === Platform ===
// ================

/// This enumeration lists all the supported platforms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum Platform {
    Android,
    FreeBSD,
    IOS,
    Linux,
    MacOS,
    OpenBSD,
    Windows,
}
pub use Platform::*;

#[allow(missing_docs)]
impl Platform {
    pub fn is_android(self) -> bool {
        self == Android
    }
    pub fn is_freebsd(self) -> bool {
        self == FreeBSD
    }
    pub fn is_ios(self) -> bool {
        self == IOS
    }
    pub fn is_linux(self) -> bool {
        self == Linux
    }
    pub fn is_macos(self) -> bool {
        self == MacOS
    }
    pub fn is_openbsd(self) -> bool {
        self == OpenBSD
    }
    pub fn is_windows(self) -> bool {
        self == Windows
    }
}

/// An error indicating that the platform was not recognized.
#[derive(Clone, Copy, Debug)]
pub struct UnknownPlatform;

impl TryFrom<&str> for Platform {
    type Error = UnknownPlatform;
    #[allow(clippy::if_same_then_else)]
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let name = s.to_lowercase();
        if name.contains("darwin") {
            Ok(MacOS)
        } else if name.contains("mac") {
            Ok(MacOS)
        } else if name.contains("linux") {
            Ok(Linux)
        }
        // CAREFUL: this matches also "darwin" (that's why it's declared below the "darwin" match).
        else if name.contains("win") {
            Ok(Windows)
        } else if name.contains("ios") {
            Ok(IOS)
        } else if name.contains("iphone") {
            Ok(IOS)
        } else if name.contains("ipad") {
            Ok(IOS)
        } else if name.contains("android") {
            Ok(Android)
        } else if name.contains("freebsd") {
            Ok(FreeBSD)
        } else if name.contains("openbsd") {
            Ok(OpenBSD)
        } else if name.contains("bsd") {
            Ok(FreeBSD)
        } else {
            Err(UnknownPlatform)
        }
    }
}

impl TryFrom<String> for Platform {
    type Error = UnknownPlatform;
    fn try_from(s: String) -> Result<Self, Self::Error> {
        Platform::try_from(s.as_str())
    }
}



// ================================
// === Compile Time Redirection ===
// ================================

/// Queries which platform we are on.
#[cfg(target_arch = "wasm32")]
pub fn current() -> Option<Platform> {
    current_wasm()
}

/// Queries which platform we are on.
#[cfg(not(target_arch = "wasm32"))]
pub fn current() -> Option<Platform> {
    current_native()
}



// ====================
// === Current WASM ===
// ====================

/// Queries which platform we are on.
#[allow(clippy::if_same_then_else)]
#[cfg(target_arch = "wasm32")]
pub fn current_wasm() -> Option<Platform> {
    use super::window;
    let navigator = window.navigator();
    let platform = navigator.platform().unwrap_or_default().to_lowercase();
    let agent = navigator.user_agent().unwrap_or_default().to_lowercase();
    Platform::try_from(platform).or_else(|_| Platform::try_from(agent)).ok()
}



// ======================
// === Current Native ===
// ======================

#[cfg(target_os = "android")]
fn current_native() -> Option<Platform> {
    Some(Android)
}
#[cfg(target_os = "ios")]
fn current_native() -> Option<Platform> {
    Some(IOS)
}
#[cfg(target_os = "linux")]
fn current_native() -> Option<Platform> {
    Some(Linux)
}
#[cfg(target_os = "macos")]
fn current_native() -> Option<Platform> {
    Some(MacOS)
}
#[cfg(target_os = "windows")]
fn current_native() -> Option<Platform> {
    Some(Windows)
}

#[cfg(not(any(
    target_arch = "wasm32",
    target_os = "android",
    target_os = "ios",
    target_os = "linux",
    target_os = "macos",
    target_os = "windows"
)))]
fn current_native() -> Option<Platform> {
    None
}



// =============
// === Tests ===
// =============

#[cfg(all(test, any(target_os = "linux", target_os = "windows", target_os = "macos")))]
mod test {
    use super::*;

    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn platform() {
        assert_eq!(current(), current_native())
    }
}
