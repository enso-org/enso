//! Building dynamic assets (assets which require the application to be run to generate their
//! sources).
//!
//! The essential operation, producing a directory of outputs from a directory of inputs, is
//! implemented by each builder (e.g. [`Builder::Shader`], [`Builder::Font`]).
//!
//! As builders can take some time to run, a caching mechanism is used to avoid unnecessary
//! rebuilds. Caching is achieved by making populating-the-output-directory an idempotent process:
//! Paths within the output directory are dependent on the *content* of the corresponding input
//! files, so that if a calculated output path already exists, it is already up-to-date; otherwise,
//! it must be built. This design may be familiar to users of the Nix or Guix package managers.

use crate::Paths;
use enso_prelude::anyhow;
use ide_ci::prelude::*;
use ide_ci::programs::shaderc::Glslc;
use ide_ci::programs::shaderc::SpirvOpt;
use ide_ci::programs::spirv_cross::SpirvCross;
use std::hash::Hasher;



// =============
// === Build ===
// =============

/// Bring the dynamic assets up-to-date, for the current asset sources. This consists of:
/// - Scan the asset source directory tree, hashing the input files.
/// - Update the assets:
///   - For each asset-source directory, determine an output directory based on the inputs name and
///     the hashes of its files.
///   - If that output directory doesn't exist, run the builder (determined by the top-level
///     directory the in which the asset was found, e.g. `shader`) and populate the directory.
///   - Generate a manifest, identifying the current assets and paths to their sources.
pub async fn build(paths: &Paths) -> Result<()> {
    info!("Building dynamic assets.");
    let sources = survey_asset_sources(paths)?;
    let assets = update_assets(paths, &sources).await?;
    let manifest = serde_json::to_string(&assets)?;
    ide_ci::fs::tokio::write(&paths.target.ensogl_pack.dist.dynamic_assets.manifest, manifest)
        .await?;
    gc_assets(paths, &assets)?;
    Ok(())
}



// ===============
// === Builder ===
// ===============

/// Identifies an asset type, which determines how it is built.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "lowercase")]
enum Builder {
    Font,
    Shader,
}

impl Builder {
    fn dir_name<'a>(self) -> &'a str {
        self.into()
    }

    async fn build_asset(
        self,
        input_dir: &Path,
        input_files: &[String],
        output_dir: &Path,
        tmp: &Path,
    ) -> Result<()> {
        match self {
            Builder::Font => build_font(input_dir, input_files, output_dir).await,
            Builder::Shader => build_shader(input_dir, input_files, output_dir, tmp).await,
        }
    }
}
impl TryFrom<&str> for Builder {
    type Error = anyhow::Error;
    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        match value {
            "font" => Ok(Builder::Font),
            "shader" => Ok(Builder::Shader),
            other => Err(anyhow!("Unknown builder: {other:?}")),
        }
    }
}
impl From<Builder> for &'static str {
    fn from(value: Builder) -> Self {
        match value {
            Builder::Font => "font",
            Builder::Shader => "shader",
        }
    }
}



// ====================
// === Build Inputs ===
// ====================

/// The inputs to a builder.
struct AssetSources {
    asset_key:   String,
    input_files: Vec<String>,
    inputs_hash: u64,
}

impl AssetSources {
    /// The output directory name for the asset.
    fn dir_name(&self) -> String {
        let key = &self.asset_key;
        let hash = self.inputs_hash;
        format!("{key}-{hash:x}")
    }
}



// =====================
// === Build Outputs ===
// =====================

/// The outputs of a builder.
#[derive(Serialize)]
struct Asset {
    dir:   String,
    files: Vec<String>,
}

/// The outputs of all builders.
type AssetManifest = BTreeMap<Builder, BTreeMap<String, Asset>>;



// ================
// === Building ===
// ================

/// Scan the sources found in the asset sources directory.
///
/// Returns, for each [`Builder`] (e.g. shader or font), for each asset directory found, an
/// [`AssetSources`] object identifying the asset key (i.e. the name of its directory), its input
/// files, and a hash covering all its input files.
fn survey_asset_sources(paths: &Paths) -> Result<HashMap<Builder, Vec<AssetSources>>> {
    let dir = ide_ci::fs::read_dir(&paths.target.ensogl_pack.dynamic_assets)?;
    let mut asset_sources: HashMap<_, Vec<_>> = HashMap::new();
    let mut buf = Vec::new();
    for entry in dir {
        let entry = entry?;
        let builder = Builder::try_from(entry.file_name().to_string_lossy().as_ref())?;
        let builder_dir = ide_ci::fs::read_dir(entry.path())?;
        let builder_sources = asset_sources.entry(builder).or_default();
        for entry in builder_dir {
            let entry = entry?;
            let asset_key = entry.file_name().to_string_lossy().to_string();
            let dir = ide_ci::fs::read_dir(entry.path())?;
            let mut file_hashes = BTreeMap::new();
            for entry in dir {
                let entry = entry?;
                let file_name = entry.file_name().to_string_lossy().to_string();
                let path = entry.path();
                buf.clear();
                ide_ci::fs::open(path)?.read_to_end(&mut buf)?;
                let mut file_hasher = std::collections::hash_map::DefaultHasher::new();
                buf.hash(&mut file_hasher);
                file_hashes.insert(file_name, file_hasher.finish());
            }
            let mut asset_hasher = std::collections::hash_map::DefaultHasher::new();
            file_hashes.hash(&mut asset_hasher);
            let inputs_hash = asset_hasher.finish();
            let input_files = file_hashes.into_keys().collect();
            builder_sources.push(AssetSources { asset_key, input_files, inputs_hash });
        }
    }
    Ok(asset_sources)
}

/// Generate any assets not found up-to-date in the cache.
///
/// If an output directory already exists, it can be assumed to be up-to-date (because output path
/// is dependent on the input data), and is used as-is. Otherwise, [`build_asset`] runs the
/// appropriate builder to generate the output directory. In either case, a summary of the files
/// present in the output directory is produced; these summaries are assembled into an
/// [`AssetManifest`].
///
/// When asset builders need to be invoked, they are all run in parallel.
async fn update_assets(
    paths: &Paths,
    sources: &HashMap<Builder, Vec<AssetSources>>,
) -> Result<AssetManifest> {
    let out = &paths.target.ensogl_pack.dist.dynamic_assets;
    ide_ci::fs::create_dir_if_missing(out)?;
    let mut assets: AssetManifest = BTreeMap::new();
    let mut deferred_assets: BTreeMap<Builder, Vec<_>> = BTreeMap::new();
    for (&builder, builder_sources) in sources {
        let out = out.join(builder.dir_name());
        ide_ci::fs::create_dir_if_missing(&out)?;
        for source_specification in builder_sources {
            let out = out.join(source_specification.dir_name());
            let key = source_specification.asset_key.clone();
            match std::fs::try_exists(&out)? {
                false => {
                    info!("Rebuilding asset: `{}`.", out.display());
                    let builder_assets = deferred_assets.entry(builder).or_default();
                    let build = build_asset(paths, builder, source_specification);
                    builder_assets.push(async move { Ok((key, build.await?)) });
                }
                true => {
                    debug!("Skipping clean asset: `{}`.", out.display());
                    let builder_assets = assets.entry(builder).or_default();
                    let asset = survey_asset(paths, builder, source_specification)?;
                    builder_assets.insert(key, asset);
                }
            };
        }
    }
    for (builder, deferred_assets) in deferred_assets.into_iter() {
        let deferred_assets = futures::future::join_all(deferred_assets).await;
        let deferred_assets: Result<Vec<_>> = deferred_assets.into_iter().collect();
        assets.entry(builder).or_default().extend(deferred_assets?);
    }
    Ok(assets)
}

/// Generate an asset from the given sources.
///
/// Set up paths (as described in the [`crate`] docs): run the appropriate [`Builder`]; move its
/// output from a temporary path into its final location (note that outputs are not built directly
/// in their final location, because directories found in the output tree are assumed to
/// accurately represent the results of running the specified builder for the specified inputs;
/// creating the output directory in its complete state ensures that if a build process is
/// interrupted, incomplete artifacts are never used).
async fn build_asset(
    paths: &Paths,
    builder: Builder,
    source_specification: &AssetSources,
) -> Result<Asset> {
    let input_dir = paths
        .target
        .ensogl_pack
        .dynamic_assets
        .join(builder.dir_name())
        .join(&source_specification.asset_key);
    let tmp_output_dir = paths
        .target
        .ensogl_pack
        .dist
        .dynamic_assets
        .join(builder.dir_name())
        .join(&source_specification.asset_key);
    tokio::fs::create_dir(&tmp_output_dir).await?;
    let work_path = paths
        .target
        .ensogl_pack
        .dynamic_assets
        .join(builder.dir_name())
        .join(format!("{}.work", source_specification.asset_key));
    builder
        .build_asset(&input_dir, &source_specification.input_files, &tmp_output_dir, &work_path)
        .await?;
    let output_dir = paths
        .target
        .ensogl_pack
        .dist
        .dynamic_assets
        .join(builder.dir_name())
        .join(source_specification.dir_name());
    tokio::fs::rename(tmp_output_dir, output_dir).await?;
    survey_asset(paths, builder, source_specification)
}

/// Identify the files present in an asset directory.
fn survey_asset(
    paths: &Paths,
    builder: Builder,
    source_specification: &AssetSources,
) -> Result<Asset> {
    let dir = source_specification.dir_name();
    let path = paths.target.ensogl_pack.dist.dynamic_assets.join(builder.dir_name()).join(&dir);
    let mut files = Vec::new();
    for entry in ide_ci::fs::read_dir(&path)? {
        files.push(entry?.file_name().to_string_lossy().to_string());
    }
    Ok(Asset { dir, files })
}

/// Remove any assets not present in the manifest.
fn gc_assets(paths: &Paths, assets: &AssetManifest) -> Result<()> {
    let is_not_manifest = |entry: &std::io::Result<std::fs::DirEntry>| {
        entry
            .as_ref()
            .map(|entry| entry.path() != paths.target.ensogl_pack.dist.dynamic_assets.manifest)
            .unwrap_or(true)
    };
    for entry in paths.target.ensogl_pack.dist.dynamic_assets.read_dir()?.filter(is_not_manifest) {
        let entry = entry?;
        let path = entry.path();
        let builder = Builder::try_from(entry.file_name().to_string_lossy().as_ref()).ok();
        let assets = builder.and_then(|builder| assets.get(&builder));
        match assets {
            Some(assets) => {
                let assets: HashSet<_> = assets.values().map(|asset| asset.dir.as_ref()).collect();
                for entry in path.read_dir()? {
                    let entry = entry?;
                    let path = entry.path();
                    if !assets.contains(entry.file_name().to_string_lossy().as_ref()) {
                        info!("Cleaning unused asset at `{}`.", path.display());
                        ide_ci::fs::remove_if_exists(path)?;
                    }
                }
            }
            _ => {
                info!("Cleaning unused builder at `{}`.", path.display());
                ide_ci::fs::remove_if_exists(path)?;
            }
        }
    }
    Ok(())
}



// =============
// === Fonts ===
// =============

async fn build_font(input_dir: &Path, input_files: &[String], output_dir: &Path) -> Result<()> {
    for file_name in input_files {
        crate::copy(input_dir.join(file_name), output_dir.join(file_name))?;
    }
    Ok(())
}



// ===============
// === Shaders ===
// ===============

/// Build optimized shaders by using `glslc`, `spirv-opt` and `spirv-cross`.
async fn build_shader(
    input_dir: &Path,
    input_files: &[String],
    output_dir: &Path,
    work_dir: &Path,
) -> Result<()> {
    ide_ci::fs::tokio::create_dir_if_missing(work_dir).await?;
    info!("Optimizing `{}`.", input_dir.file_name().unwrap_or_default().to_string_lossy());
    for glsl_file_name in input_files {
        let glsl_path = input_dir.join(glsl_file_name);
        let work_path = work_dir.join(glsl_file_name);
        let stage_path = work_path.with_extension("");
        let stage =
            stage_path.file_name().ok_or_else(|| anyhow!("Empty stage path."))?.to_string_lossy();
        let spv_path = stage_path.with_appended_extension("spv");
        let spv_opt_path = stage_path.with_appended_extension("opt.spv");
        let glsl_opt_path = stage_path.with_appended_extension("opt.glsl");
        let glsl_opt_dist_path = output_dir.join(glsl_file_name);
        let spv_path = spv_path.as_str();
        let glsl_path = glsl_path.as_str();
        let shader_stage = &format!("-fshader-stage={stage}");
        let glslc_args = ["--target-env=opengl", shader_stage, "-o", spv_path, glsl_path];
        let spirv_opt_args = ["-O", "-o", spv_opt_path.as_str(), spv_path.as_str()];
        let spirv_cross_args = ["--output", glsl_opt_path.as_str(), spv_opt_path.as_str()];
        Glslc.cmd()?.args(glslc_args).run_ok().await?;
        SpirvOpt.cmd()?.args(spirv_opt_args).run_ok().await?;
        SpirvCross.cmd()?.args(spirv_cross_args).run_ok().await?;

        let content =
            ide_ci::fs::tokio::read_to_string(&glsl_opt_path).await?.replace("\r\n", "\n");
        let extract_err = || format!("Failed to process shader '{}'.", glsl_opt_path.as_str());
        let code = extract_main_shader_code(&content).with_context(extract_err)?;
        ide_ci::fs::tokio::write(&glsl_opt_dist_path, code).await?;
    }
    Ok(())
}

/// Read the optimized shader code, extract the main function body and preserve all top-level
/// variable declarations.
fn extract_main_shader_code(code: &str) -> Result<String> {
    let main_start_str = "void main()\n{";
    let main_end_str = "}";
    let main_fn_find_err = "Failed to find main function.";
    let main_start = code.find(main_start_str).with_context(|| main_fn_find_err)?;
    let main_end = code.rfind(main_end_str).with_context(|| main_fn_find_err)?;
    let before_main = &code[..main_start];
    let declarations: Vec<&str> = before_main
        .lines()
        .filter_map(|line| {
            let version_def = line.starts_with("#version ");
            let precision_def = line.starts_with("precision ");
            let layout_def = line.starts_with("layout(");
            let def = version_def || precision_def || layout_def;
            (!def).then_some(line)
        })
        .collect();
    let declarations = declarations.join("\n");
    let main_content = &code[main_start + main_start_str.len()..main_end];
    Ok(format!("{declarations}\n{main_content}"))
}
