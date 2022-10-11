

// use crate::goodie::GoodieDatabase;
// use crate::prelude::*;
// use crate::programs;
//
// pub struct Sbt;
//
// pub struct Instance {
//     directory: PathBuf,
// }
//
// impl crate::goodie::Instance for Instance {
//     fn add_to_environment(&self) -> anyhow::Result<()> {
//         crate::env::prepend_to_path(self.directory.join("bin"))
//     }
// }
//
// #[async_trait]
// impl Goodie for Sbt {
//     const NAME: &'static str = "SBT";
//     type Instance = Instance;
//
//     async fn is_already_available(&self) -> Result<bool> {
//         Ok(programs::Sbt.lookup().is_ok())
//     }
//
//     async fn lookup(&self, database: &GoodieDatabase) -> Result<Self::Instance> {
//         database.find_dir("sbt").map(|directory| Instance { directory })
//     }
//
//     async fn install(&self, database: &GoodieDatabase) -> Result<Self::Instance> {
//         let url = "https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.tgz";
//         crate::io::download_and_extract(url.clone(), &database.root_directory).await?;
//         self.lookup(database).await
//     }
// }
