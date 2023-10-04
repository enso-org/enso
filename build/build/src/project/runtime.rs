//! Wrappers over the Rust part of the IDE codebase.

use crate::prelude::*;

use crate::engine::package::IsPackage;
use crate::engine::BuildConfigurationFlags;
use crate::paths::generated::EnginePackage;
use crate::paths::TargetTriple;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::source::WithDestination;
use crate::version::Versions;



const ARTIFACT_NAME: &str = "runtime";

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BuildInput {
    pub versions: Versions,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Runtime;

#[async_trait]
impl IsTarget for Runtime {
    type BuildInput = BuildInput;
    type Artifact = Artifact;

    fn artifact_name(&self) -> String {
        ARTIFACT_NAME.into()
    }

    fn adapt_artifact(self, path: impl AsRef<Path>) -> BoxFuture<'static, Result<Self::Artifact>> {
        ready(Ok(Artifact::new(path.as_ref()))).boxed()
    }

    fn build_internal(
        &self,
        context: Context,
        job: WithDestination<Self::BuildInput>,
    ) -> BoxFuture<'static, Result<Self::Artifact>> {
        let config = BuildConfigurationFlags {
            build_engine_package: true,
            generate_java_from_rust: true,
            ..default()
        };
        let this = *self;
        let WithDestination { inner, destination } = job;
        let triple = TargetTriple::new(inner.versions);
        let context = crate::engine::RunContext::new(context, config, triple, None);
        context
            .and_then_async(|context| async move {
                let artifacts = context.build().await?;
                let engine_package =
                    artifacts.engine_package.context("Failed to find engine package artifacts.")?;
                ide_ci::fs::mirror_directory(engine_package.dir(), &destination).await?;
                this.adapt_artifact(engine_package.dir()).await
            })
            .boxed()
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub struct Artifact(EnginePackage);

impl Artifact {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self(EnginePackage::new_root(path))
    }

    pub fn into_inner(self) -> EnginePackage {
        self.0
    }
}

impl AsRef<Path> for Artifact {
    fn as_ref(&self) -> &Path {
        self.0.as_path()
    }
}

impl IsArtifact for Artifact {}



////////////////////////////////////////////////////////////////////////////////////////////
