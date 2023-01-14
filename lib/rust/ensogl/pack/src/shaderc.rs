use crate::prelude::*;
use ide_ci::goodie::GoodieDatabase;

pub fn download_urls() -> Result<Url> {
    match TARGET_OS {
        OS::Linux =>
            "https://storage.googleapis.com/shaderc/badges/build_link_linux_gcc_release.html",
        OS::MacOS =>
            "https://storage.googleapis.com/shaderc/badges/build_link_macos_clang_release.html",
        OS::Windows =>
            "https://storage.googleapis.com/shaderc/badges/build_link_windows_vs2017_release.html",
        _ => bail!("Unsupported OS: {}.", TARGET_OS),
    }
    .try_into()
    .anyhow_err()
}

// pub struct Shaderc;
//
// impl Goodie for Shaderc {
//     const NAME: &'static str = "shaderc";
//     type Instance = ();
//
//     async fn is_already_available(&self) -> anyhow::Result<bool> {
//         todo!()
//     }
//
//     async fn lookup(&self, database: &GoodieDatabase) -> Result<Self::Instance> {
//         todo!()
//     }
//
//     async fn install(&self, database: &GoodieDatabase) -> Result<Self::Instance> {
//         todo!()
//     }
// }
