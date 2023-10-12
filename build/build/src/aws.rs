use crate::prelude::*;

use crate::version::Kind;

use anyhow::Context;
use aws_sdk_s3::model::ObjectCannedAcl;
use aws_sdk_s3::types::ByteStream;
use s3::BucketContext;


// ==============
// === Export ===
// ==============

pub mod ecr;
pub mod env;
pub mod s3;



/// The upper limit on number of nightly editions that are stored in the bucket.
pub const NIGHTLY_EDITIONS_LIMIT: usize = 20;

pub const EDITIONS_BUCKET_NAME: &str = "editions.release.enso.org";

pub const MANIFEST_FILENAME: &str = "manifest.yaml";



#[derive(Clone, Debug, Display, Serialize, Deserialize, Deref)]
pub struct Edition(pub String);

impl AsRef<str> for Edition {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl<T: Into<String>> From<T> for Edition {
    fn from(value: T) -> Self {
        Edition(value.into())
    }
}

impl Edition {
    pub fn is_nightly(&self) -> bool {
        // TRANSITION: old nightlies
        self.0.contains("nightly")
            || Version::find_in_text(self)
                .as_ref()
                .map_or(false, |version| Kind::Nightly.matches(version))
    }
}



#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Manifest {
    /// Sequence of edition names.
    pub editions: Vec<Edition>,
}

impl Manifest {
    pub fn with_new_nightly(
        &self,
        new_nightly: Edition,
        nightlies_count_limit: usize,
    ) -> (Manifest, Vec<&Edition>) {
        let (nightlies, non_nightlies) =
            self.editions.iter().partition::<Vec<_>, _>(|e| e.is_nightly());
        let nightlies_count_to_remove = (1 + nightlies.len()).saturating_sub(nightlies_count_limit);
        debug!(
            "Will remove {} nightly editions from {} found.",
            nightlies_count_to_remove,
            nightlies.len()
        );
        let (nightlies_to_remove, nightlies_to_keep) =
            nightlies.split_at(nightlies_count_to_remove);

        let mut new_editions = non_nightlies;
        new_editions.extend(nightlies_to_keep);
        new_editions.push(&new_nightly);

        let new_manifest = Manifest { editions: new_editions.into_iter().cloned().collect() };
        (new_manifest, nightlies_to_remove.to_vec())
    }
}

pub async fn update_manifest(repo_context: &impl IsRepo, edition_file: &Path) -> Result {
    let bucket_context = BucketContext {
        client:     aws_sdk_s3::Client::new(&aws_config::load_from_env().await),
        bucket:     EDITIONS_BUCKET_NAME.to_string(),
        upload_acl: ObjectCannedAcl::PublicRead,
        key_prefix: Some(repo_context.name().to_string()),
    };

    let new_edition_name = Edition(
        edition_file
            .file_stem()
            .context("Edition file path is missing filename stem!")?
            .as_str()
            .to_string(),
    );
    ide_ci::fs::expect_file(edition_file)?;

    let manifest = bucket_context.get_yaml::<Manifest>(MANIFEST_FILENAME).await?;
    debug!("Got manifest index from S3: {:#?}", manifest);


    let (new_manifest, nightlies_to_remove) =
        manifest.with_new_nightly(new_edition_name, NIGHTLY_EDITIONS_LIMIT);
    for nightly_to_remove in nightlies_to_remove {
        debug!("Should remove {}", nightly_to_remove);
    }

    let new_edition_filename =
        edition_file.file_name().context("Edition file path is missing filename!")?;
    bucket_context
        .put(new_edition_filename.as_str(), ByteStream::from_path(&edition_file).await?)
        .await?;

    bucket_context.put_yaml("manifest.yaml", &new_manifest).await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;


    // #[tokio::test]
    // async fn aaa() -> Result {
    //     let repo = RepoContext::from_str("enso-org/enso")?;
    //     let paths =
    //         Paths::new_version(r"H:\NBO\enso", Version::parse("2022.1.1-nightly.2022-01-28")?)?;
    //     update_manifest(&repo, &paths).await?;
    //     Ok(())
    // }

    #[test]
    fn updating_manifest() -> Result {
        let old_nightly = serde_yaml::from_str::<Manifest>(
            r"editions:
- '2021.11'
- 2021.13-SNAPSHOT
- 2021.14-SNAPSHOT
- 2021.15-SNAPSHOT
- 2021.12-SNAPSHOT
- nightly-2021-08-12
- nightly-2021-08-12.1
- nightly-2021-08-16
- nightly-2021-09-03
",
        )?;
        old_nightly.with_new_nightly(Edition("foo_bar".into()), 20);

        Ok(())
    }
}
