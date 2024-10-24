// ==============
// === Export ===
// ==============

pub mod win;



#[cfg(target_os = "windows")]
pub const DEFAULT_SHELL: crate::programs::Cmd = crate::programs::Cmd;

#[cfg(not(target_os = "windows"))]
pub const DEFAULT_SHELL: crate::programs::Bash = crate::programs::Bash;
