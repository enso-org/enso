use crate::prelude::*;

use crate::fs::create_dir_if_missing;



#[async_trait]
pub trait Goodie {
    // Assumed to be unique among types.
    // TODO requirement should be lifted, consider something safer
    const NAME: &'static str;
    type Instance: Instance + Sized;

    async fn is_already_available(&self) -> anyhow::Result<bool>;
    async fn lookup(&self, database: &GoodieDatabase) -> Result<Self::Instance>;
    async fn install(&self, database: &GoodieDatabase) -> Result<Self::Instance>;
}

pub trait Instance {
    fn add_to_environment(&self) -> anyhow::Result<()>;
}


#[derive(Clone, Debug)]
pub struct GoodieDatabase {
    pub root_directory: PathBuf,
}
impl GoodieDatabase {
    pub fn new() -> anyhow::Result<Self> {
        let home = dirs::home_dir().ok_or_else(|| anyhow!("Cannot figure out home directory."))?;
        let path = home.join(".enso-ci");
        create_dir_if_missing(&path)?;
        Ok(GoodieDatabase { root_directory: path })
    }

    pub async fn require(&self, goodie: &impl Goodie) -> Result {
        if goodie.is_already_available().await? {
            Ok(())
        } else if let Ok(instance) = goodie.lookup(self).await {
            instance.add_to_environment()
        } else {
            let instance = goodie.install(self).await?;
            instance.add_to_environment()
        }
    }

    pub fn find_dir(&self, directory_name: impl AsRef<Path>) -> Result<PathBuf> {
        let expected_dir_name = directory_name.as_ref();
        for entry in crate::fs::read_dir(&self.root_directory)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() && entry.path().file_name().contains(&expected_dir_name)
            {
                return Ok(entry.path());
            }
        }
        bail!("no directory by name {} in the database.", expected_dir_name.display())
    }
}
