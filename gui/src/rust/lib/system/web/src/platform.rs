//! This module provides helpers for platform specific logic.

// ================
// === Platform ===
// ================

/// This enumeration lists all the supported platforms.
#[derive(Debug,Clone,Copy,PartialEq)]
pub enum Platform {
    Android,
    IOS,
    Linux,
    MacOS,
    Windows,

    Unknown
}
pub use Platform::*;

/// Queries which platform we are on.
#[cfg(target_arch = "wasm32")]
pub fn current() -> Platform {
    use super::window;
    let window    = window();
    let navigator = window.navigator();
    let platform  = navigator.platform().unwrap_or_else(|_| "Unknown".into()).to_lowercase();
    let agent     = navigator.user_agent().unwrap_or_else(|_| "Unknown".into()).to_lowercase();

    if platform.find("mac").is_some() {
        Platform::MacOS
    } else if platform.find("win").is_some() {
        Platform::Windows
    } else if platform.find("ipad").is_some() || platform.find("iphone").is_some() {
        Platform::IOS
    } else if platform.find("android").is_some() || agent.find("android").is_some() {
        Platform::Android
    } else if platform.find("linux").is_some() {
        Platform::Linux
    } else {
        Platform::Unknown
    }
}

/// Queries which platform we are on.
#[cfg(not(target_arch="wasm32"))]
pub fn current() -> Platform {
    target_os()
}



// =================
// === Target OS ===
// =================

#[cfg(target_os="android")] fn target_os() -> Platform { Android }
#[cfg(target_os="ios")]     fn target_os() -> Platform { IOS }
#[cfg(target_os="linux")]   fn target_os() -> Platform { Linux }
#[cfg(target_os="macos")]   fn target_os() -> Platform { MacOS }
#[cfg(target_os="windows")] fn target_os() -> Platform { Windows }

#[cfg(not(any(
    target_arch = "wasm32",
    target_os   = "android",
    target_os   = "ios",
    target_os   = "linux",
    target_os   = "macos",
    target_os   = "windows"
)))] fn target_os() -> Platform { Unknown }



// =============
// === Tests ===
// =============

#[cfg(all(test,any(target_os="linux",target_os="windows",target_os="macos")))]
mod test {
    use super::*;

    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn platform() {
        assert_eq!(current(),target_os())
    }
}
