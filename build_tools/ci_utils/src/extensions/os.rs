use crate::prelude::*;



/// A bunch of constant literals associated with a given OS. Follows the convention of constants
/// defined in [`std::env::consts`] module.
#[const_trait]
pub trait OsExt: Copy {
    fn exe_suffix(self) -> &'static str;
    fn exe_extension(self) -> &'static str;
    fn dll_prefix(self) -> &'static str;
    fn dll_extension(self) -> &'static str;
    fn dll_suffix(self) -> &'static str;
}

impl const OsExt for OS {
    fn exe_suffix(self) -> &'static str {
        match self {
            OS::Windows => ".exe",
            OS::Linux => "",
            OS::MacOS => "",
            _ => todo!(),
        }
    }

    fn exe_extension(self) -> &'static str {
        match self {
            OS::Windows => "exe",
            OS::Linux => "",
            OS::MacOS => "",
            _ => todo!(),
        }
    }

    fn dll_prefix(self) -> &'static str {
        match self {
            OS::Windows => "",
            OS::Linux => "lib",
            OS::MacOS => "lib",
            _ => todo!(),
        }
    }

    fn dll_extension(self) -> &'static str {
        match self {
            OS::Windows => "dll",
            OS::Linux => "so",
            OS::MacOS => "dylib",
            _ => todo!(),
        }
    }

    fn dll_suffix(self) -> &'static str {
        match self {
            OS::Windows => ".dll",
            OS::Linux => ".so",
            OS::MacOS => ".dylib",
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_on_target() {
        assert_eq!(std::env::consts::DLL_EXTENSION, TARGET_OS.dll_extension());
        assert_eq!(std::env::consts::DLL_PREFIX, TARGET_OS.dll_prefix());
        assert_eq!(std::env::consts::DLL_SUFFIX, TARGET_OS.dll_suffix());
        assert_eq!(std::env::consts::EXE_EXTENSION, TARGET_OS.exe_extension());
        assert_eq!(std::env::consts::EXE_SUFFIX, TARGET_OS.exe_suffix());
    }
}
