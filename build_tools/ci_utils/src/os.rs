// ==============
// === Export ===
// ==============

pub mod target;

pub use target::TARGET_ARCH;
pub use target::TARGET_OS;



pub fn default_shell() -> impl crate::Shell {
    #[cfg(target_os = "windows")]
    return crate::programs::PwSh;

    #[cfg(not(target_os = "windows"))]
    return crate::programs::Bash;
}
