

// use crate::prelude::*;
//
// use crate::goodie::GoodieDatabase;
//
// use crate::fs::expect_dir;
// use crate::fs::expect_file;
// use crate::io::download_and_extract;
// use crate::programs::Bash;
// use lazy_static::lazy_static;
// use std::env::consts::EXE_EXTENSION;
// use std::env::consts::EXE_SUFFIX;
//
// lazy_static! {
//     pub static ref PROGRAM_NAME: String = format!("{}-musl-gcc{}", filename_stem(), EXE_SUFFIX);
// }
//
// pub struct Gcc;
//
// impl Program for Gcc {
//     fn executable_name(&self) -> &'static str {
//         &PROGRAM_NAME
//     }
// }
//
// pub struct Musl;
//
// pub struct Instance {
//     directory: PathBuf,
// }
//
// impl crate::goodie::Instance for Instance {
//     fn add_to_environment(&self) -> anyhow::Result<()> {
//         std::env::set_var("TOOLCHAIN_DIR", &self.directory);
//         crate::env::prepend_to_path(self.directory.join("bin"))
//     }
// }
//
// #[async_trait]
// impl Goodie for Musl {
//     const NAME: &'static str = "musl libc toolchain";
//     type Instance = Instance;
//
//     async fn is_already_available(&self) -> Result<bool> {
//         Ok(Gcc.lookup().is_ok())
//     }
//
//     async fn lookup(&self, database: &GoodieDatabase) -> Result<Self::Instance> {
//         database.find_dir("musl").map(|directory| Instance { directory })
//     }
//
//     async fn install(&self, database: &GoodieDatabase) -> Result<Self::Instance> {
//         // Reportedly for my "convenience". :(
//         let archive_format = if TARGET_OS == OS::Windows { "zip" } else { "tgz" };
//         let url = format!(
//             "https://more.musl.cc/10.2.1/x86_64-linux-musl/{}.{}",
//             filename_stem(),
//             archive_format
//         );
//         // let url = format!("https://musl.cc/{}.{}", filename_stem(), archive_format);
//         let downloaded_dir = database.root_directory.join(filename_stem());
//         let target_dir = database.root_directory.join("musl");
//         crate::fs::reset_dir(&downloaded_dir)?;
//         crate::fs::reset_dir(&target_dir)?;
//         // let result = (async move || -> Result {
//         crate::io::download_and_extract(url.clone(), &database.root_directory).await?;
//         add_zlib(&downloaded_dir).await?;
//         // Ok(())
//         // })().await;
//         // if result.is_err() {
//         //     crate::io::remove_dir_if_exists(&downloaded_dir)?;
//         //     crate::io::remove_dir_if_exists(&target_dir)?;
//         // };
//         // result?;
//         std::fs::rename(downloaded_dir, target_dir)?;
//         self.lookup(database).await
//     }
// }
//
// pub async fn add_zlib(musl_toolchain: &Path) -> Result {
//     let temp = tempfile::tempdir()?;
//     let zlib_url = Url::from_str("http://www.zlib.net/zlib-1.2.11.tar.gz")?;
//     let zlib_dirname = PathBuf::from("zlib-1.2.11");
//     download_and_extract(zlib_url, &temp).await?;
//     let zlib_path = temp.path().join(zlib_dirname);
//     expect_dir(&zlib_path)?;
//     let gcc_path = musl_toolchain.join_iter(["bin",
// "gcc"]).with_appended_extension(EXE_EXTENSION);     expect_file(&gcc_path)?;
//
//     Bash.run_command()?
//         .arg("./configure --prefix=$TOOLCHAIN_DIR --static && make && make install")
//         .env("CC", &gcc_path)
//         .env("TOOLCHAIN_DIR", musl_toolchain)
//         .current_dir(&zlib_path)
//         .run_ok()
//         .await?;
//
//     Ok(())
// }
//
// pub fn target_path() -> String {
//     let os_name = match TARGET_OS {
//         OS::Linux => "linux",
//         OS::Windows => "w64",
//         other_os => unimplemented!("System `{}` is not supported!", other_os),
//     };
//
//     let arch_name = match TARGET_ARCH {
//         Arch::X86_64 => "x86_64",
//         Arch::AArch64 => "aarch64",
//         other_arch => unimplemented!("Architecture `{}` is not supported!", other_arch),
//     };
//
//     let name: &[&str] = if TARGET_OS == OS::Windows { &[] } else { &["musl"] };
//     [arch_name, os_name].iter().chain(name).join("-")
// }
//
// pub fn filename_stem() -> String {
//     format!("{}-native", target_path())
// }
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[tokio::test]
//     async fn musl_get_test() -> Result {
//         let db = GoodieDatabase::new()?;
//         db.require(&Musl).await?;
//         Ok(())
//     }
// }
