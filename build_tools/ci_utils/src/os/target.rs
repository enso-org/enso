//! Constants describing the target operating system and architecture.

use std::fmt;



pub const TARGET_ARCH: Arch = Arch::from_str(std::env::consts::ARCH);
pub const TARGET_OS: OS = OS::from_str(std::env::consts::OS);

#[derive(Debug, PartialEq, Eq, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum Arch {
    AArch64,
    Wasm32,
    X86,
    X86_64,
}

impl Arch {
    pub const fn from_str(s: &str) -> Arch {
        match s.as_bytes() {
            b"aarch64" => Arch::AArch64,
            b"wasm32" => Arch::Wasm32,
            b"x86" => Arch::X86,
            b"x86_64" => Arch::X86_64,
            _ => panic!("Unsupported target architecture."),
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Arch::AArch64 => "aarch64",
            Arch::Wasm32 => "wasm32",
            Arch::X86 => "x86",
            Arch::X86_64 => "amd64",
        }
    }
}

impl fmt::Display for Arch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum OS {
    Windows,
    Linux,
    MacOS,
}

impl fmt::Display for OS {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl OS {
    pub const fn from_str(s: &str) -> OS {
        match s.as_bytes() {
            b"windows" => OS::Windows,
            b"linux" => OS::Linux,
            b"macos" => OS::MacOS,
            _ => panic!("Unsupported target OS."),
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            OS::Windows => "windows",
            OS::Linux => "linux",
            OS::MacOS => "macos",
        }
    }

    pub const fn exe_suffix(self) -> &'static str {
        match self {
            OS::Windows => ".exe",
            OS::Linux => "",
            OS::MacOS => "",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_on_target() {
        assert_eq!(std::env::consts::EXE_SUFFIX, TARGET_OS.exe_suffix());
    }
}
