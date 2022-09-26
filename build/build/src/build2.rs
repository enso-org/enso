// use crate::prelude::*;
// use ide_ci::cache;
//
// use crate::project;
// use crate::source::CiRunSource;
// use crate::source::ExternalSource;
// use crate::source::OngoingCiRunSource;
// use crate::source::ReleaseSource;
// use ide_ci::cache::Cache;
//
// pub trait BuildContext: Clone + Send + Sync + 'static {
//     fn cache(&self) -> &Cache;
//     fn octocrab(&self) -> &Octocrab;
//
//
//     fn get_external<Project: Build>(
//         &self,
//         project: Project,
//         source: ExternalSource<Target>,
//         destination: PathBuf,
//     ) -> BoxFuture<'static, Result<Project::Artifact>> {
//         let span = info_span!("Getting artifact from an external source");
//         let destination2 = destination.clone();
//         let get_job = match source {
//             ExternalSource::OngoingCiRun(OngoingCiRunSource { artifact_name }) =>
//                 ide_ci::actions::artifacts::retrieve_compressed_directory(
//                     artifact_name,
//                     destination2,
//                 )
//                 .boxed(),
//             ExternalSource::CiRun(ci_run) => self.download_artifact(ci_run, destination2),
//             ExternalSource::LocalFile(source_path) =>
//                 ide_ci::fs::mirror_directory(source_path, destination2).boxed(),
//             ExternalSource::Release(release) => self.download_asset(release, destination2),
//         }
//         .instrument(span);
//         async move {
//             get_job.await?;
//             project.adapt_artifact(destination).await
//         }
//         .boxed()
//     }
//
//     fn download_artifact(
//         &self,
//         ci_run: CiRunSource,
//         output_path: impl AsRef<Path> + Send + Sync + 'static,
//     ) -> BoxFuture<'static, Result> {
//         let CiRunSource { run_id, artifact_name, repository, octocrab } = ci_run;
//         let span = info_span!("Downloading CI Artifact.", %artifact_name, %repository, target =
// output_path.as_str());         let this = self.clone();
//         async move {
//             let artifact =
//                 repository.find_artifact_by_name(&octocrab, run_id, &artifact_name).await?;
//             info!("Will download artifact: {:#?}", artifact);
//             let artifact_to_get = cache::artifact::ExtractedArtifact {
//                 client: octocrab,
//                 key:    cache::artifact::Key { artifact_id: artifact.id, repository },
//             };
//             let artifact = this.cache().get(artifact_to_get).await?;
//             let inner_archive_path =
//                 artifact.join(&artifact_name).with_appended_extension("tar.gz");
//             ide_ci::archive::extract_to(&inner_archive_path, &output_path).await
//         }
//         .instrument(span)
//         .boxed()
//     }
//
//     fn download_asset(
//         &self,
//         source: ReleaseSource,
//         destination: PathBuf,
//     ) -> BoxFuture<'static, Result> {
//         // source.asset_id
//         let span = info_span!("Downloading built target from a release asset.",
//                 asset_id = source.asset_id.0,
//                 repo = %source.repository);
//         let cache = self.cache().clone();
//         async move {
//             let ReleaseSource { asset_id, octocrab, repository } = &source;
//             let archive_source = repository.download_asset_job(octocrab, *asset_id);
//             let extract_job = cache::archive::ExtractedArchive {
//                 archive_source,
//                 path_to_extract: crate::project::path_to_extract(),
//             };
//             let directory = cache.get(extract_job).await?;
//             ide_ci::fs::remove_if_exists(&destination)?;
//             ide_ci::fs::symlink_auto(&directory, &destination)?;
//             Ok(())
//         }
//         .instrument(span)
//         .boxed()
//     }
// }
//
// /// Build targets, like GUI or Project Manager.
// ///
// /// Built target generates artifacts that can be stored as a release asset or CI run artifacts.
// pub trait Build: Clone + Debug + Sized + Send + Sync + 'static {
//     /// All the data needed to build this target that are not placed in `self`.
//     type BuildInput: Debug + Send + 'static;
//
//     /// A location-like value with the directory where the artifacts are placed.
//     type Artifact: project::IsArtifact;
//
//     /// Identifier used when uploading build artifacts to run.
//     ///
//     /// Note that this is not related to the assets name in the release.
//     fn artifact_name(&self) -> String;
//
//     /// Create a full artifact description from an on-disk representation.
//     fn adapt_artifact(self, path: impl AsRef<Path>) -> BoxFuture<'static,
// Result<Self::Artifact>>;
//
//     /// Produce an artifact from build inputs.
//     fn build(
//         &self,
//         input: Self::BuildInput,
//         output_path: impl AsRef<Path> + Send + Sync + 'static,
//     ) -> BoxFuture<'static, Result<Self::Artifact>>;
//
//
//
//     // fn find_asset(&self, _assets: Vec<Asset>) -> Result<Asset> {
//     //     todo!("Not implemented for target {self:?}!")
//     // }
// }
//
// #[cfg(test)]
// pub mod tests {
//     use super::*;
//
//     #[derive(Clone, Debug)]
//     pub struct BuildContext {
//         octocrab: Octocrab,
//         cache:    Cache,
//     }
//     impl super::BuildContext for BuildContext {
//         fn cache(&self) -> &Cache {
//             &self.cache
//         }
//
//         fn octocrab(&self) -> &Octocrab {
//             &self.octocrab
//         }
//     }
//
//     // pub struct Wasm;
//     //
//     // impl Build for Wasm {
//     //     type BuildInput = project::wasm::BuildInput;
//     //     type Artifact = PathBuf;
//     //
//     //     fn artifact_name(&self) -> String {
//     //         todo!()
//     //     }
//     //
//     //     fn adapt_artifact(
//     //         self,
//     //         path: impl AsRef<Path>,
//     //     ) -> BoxFuture<'static, Result<Self::Artifact>> {
//     //         todo!()
//     //     }
//     //
//     //     fn build(
//     //         &self,
//     //         input: Self::BuildInput,
//     //         output_path: impl AsRef<Path> + Send + Sync + 'static,
//     //     ) -> BoxFuture<'static, Result<Self::Artifact>> {
//     //         todo!()
//     //     }
//     // }
// }
