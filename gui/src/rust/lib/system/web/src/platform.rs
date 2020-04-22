//! This module provides helpers for platform specific logic.

use super::window;

/// This enumeration lists all the supported platforms.
#[derive(Debug,Clone,Copy,PartialEq)]
pub enum Platform {
    Linux,
    Android,
    Windows,
    MacOS,
    IOS,
    Unknown
}

impl Platform {
    /// Queries which platform we are on.
    pub fn query() -> Self {
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
}

#[cfg(all(test,any(host_os="linux",host_os="windows",host_os="macos")))]
mod test {
    use super::Platform;

    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[cfg(host_os = "linux")]
    fn host_platform() -> Platform {
        Platform::Linux
    }

    #[cfg(host_os = "windows")]
    fn host_platform() -> Platform {
        Platform::Windows
    }

    #[cfg(host_os = "macos")]
    fn host_platform() -> Platform {
        Platform::MacOS
    }

    #[wasm_bindgen_test]
    fn platform() {
        assert_eq!(Platform::query(), host_platform())
    }
}
