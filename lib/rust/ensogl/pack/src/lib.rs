#![feature(async_closure)]

pub use ide_ci::prelude;
use ide_ci::prelude::*;

use crate::shaderc::programs::glslc::Glslc;
use crate::shaderc::programs::spirv_opt::SpirvOpt;
use crate::spirvcross::program::SpirvCross;
use ide_ci::program::EMPTY_ARGS;
use ide_ci::programs::wasm_pack::WasmPackCommand;
use manifest_dir_macros::path;
use std::env;
use std::path::Path;
use std::path::PathBuf;

pub mod shaderc;
pub mod spirvcross;


// pub fn execute<T: AsRef<OsStr>>(exe: &str, args: &[T]) {
//     let args_vec: Vec<_> = args.iter().map(|t| format!("{:?}", t.as_ref())).collect();
//     println!("Executing: {} {}", exe, args_vec.join(" "));
//     let mut command =
//         Command::new(exe).args(args).spawn().expect("Failed to start external executable");
//     if !command.wait().unwrap().success() {
//         panic!("Failed to execute external executable.");
//     }
// }

pub async fn workspace_dir() -> Result<PathBuf> {
    let output = ide_ci::program::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output_ok()
        .await?
        .stdout;
    let cargo_path = Path::new(std::str::from_utf8(&output)?.trim());
    Ok(cargo_path.try_parent()?.to_owned())
}
//
// pub fn root_dir() -> &'static Path {
//     let current_cargo_path = Path::new(path!("Cargo.toml"));
//     current_cargo_path.parent().unwrap()
// }

// fn with_pwd<T>(path: &Path, f: impl FnOnce() -> T) -> T {
//     let current_dir = std::env::current_dir().unwrap();
//     std::env::set_current_dir(path).unwrap();
//     let out = f();
//     std::env::set_current_dir(current_dir).unwrap();
//     out
// }

async fn compile_js(target_dir: &Path, main: &str, out: &Path) -> Result {
    info!("Compiling {main}.");
    ide_ci::programs::Npx
        .cmd()?
        .args(&[
            "--yes",
            "esbuild",
            main,
            "--bundle",
            // FIXME:
            "--sourcemap=inline",
            "--platform=node",
            &format!("--outfile={}", out.display()),
        ])
        .current_dir(target_dir)
        .run_ok()
        .await
}

async fn compile_ts(js_dir: &Path, main: &str, out: &Path) -> Result {
    let run_script = async move |script_name, script_args: &[&str]| {
        ide_ci::programs::Npm
            .cmd()?
            .run(script_name, script_args)
            .current_dir(js_dir)
            .run_ok()
            .await
    };

    info!("Type checking TypeScript sources.");
    run_script("typecheck", &EMPTY_ARGS).await?;

    info!("Linting TypeScript sources.");
    run_script("lint", &EMPTY_ARGS).await?;

    info!("Building TypeScript sources.");
    run_script("build", &["--", &format!("--outdir={}", out.display())]).await
}

/// The arguments to `wasm-pack build` that `ensogl-pack` wants to customize.
pub struct WasmPackOutputs {
    /// Value to passed as `--out-dir` to `wasm-pack`.
    pub out_dir:  PathBuf,
    /// Value to passed as `--out-name` to `wasm-pack`.
    pub out_name: String,
}

/// Wrapper over `wasm-pack build` command.
///
/// # Arguments
/// * `outputs` - The outputs that'd be usually given to `wasm-pack build` command.
/// * `provider` - Function that generates an invocation of the `wasm-pack build` command that has
///   applied given (customized) output-related arguments.
pub async fn build(
    outputs: WasmPackOutputs,
    provider: impl FnOnce(WasmPackOutputs) -> Result<WasmPackCommand>,
) -> Result {
    // ide_ci::env::prepend_to_path(r"C:\varia\install\bin")?;
    let current_cargo_path = Path::new(path!("Cargo.toml"));
    let root_dir = current_cargo_path.try_parent()?;
    info!("{:?}", root_dir);


    let workspace_dir = workspace_dir().await?;
    let target_dir = workspace_dir.join("target").join("ensogl-pack");
    let replaced_args =
        WasmPackOutputs { out_dir: target_dir.clone(), out_name: "pkg".to_string() };
    let target_dist_dir = target_dir.join("dist");

    let WasmPackOutputs { out_name, out_dir } = outputs;

    info!("Obtaining the wasm-pack command.");
    let mut command = provider(replaced_args).context("Failed to obtain wasm-pack command.")?;
    command.run_ok().await?;

    let out_dir = Path::new(&out_dir);
    let js_dir = root_dir.join("js");
    let node_modules_dir = js_dir.join("node_modules");
    let app_js_path = target_dist_dir.join("app.js");
    let shader_extractor_path = target_dist_dir.join("shader-extractor.js");

    // FIXME? [mwu] What if dependencies are updated without deleting node_modules?
    if !node_modules_dir.is_dir() {
        ide_ci::programs::Npm.cmd()?.install().current_dir(&js_dir).run_ok().await?;
        // with_pwd(&js_dir, || execute("npm", &["install"]));
    }
    compile_ts(&js_dir, "src/index.ts", &target_dist_dir).await?;
    compile_js(&target_dir, "pkg.js", &target_dist_dir.join("main.js")).await?;
    // with_pwd(&target_dir, || compile_js("pkg.js", &target_dist_dir.join("main.js")));
    //
    // println!(
    //     "Copying {:?} to {:?}",
    //     target_dir.join("pkg.wasm"),
    //     target_dist_dir.join(&format!("main.wasm"))
    // );
    ide_ci::fs::copy(target_dir.join("pkg_bg.wasm"), target_dist_dir.join("main.wasm"))?;
    info!("Source files:");
    let paths = ide_ci::fs::read_dir(&target_dist_dir)?;
    for path in paths {
        println!("Name: {}", path?.path().display())
    }


    info!("Extracting shaders from generated WASM file.");
    let shaders_src_dir = target_dir.join("shaders");
    ide_ci::programs::Node
        .cmd()?
        .arg(&shader_extractor_path)
        .arg("--extract-shaders")
        .arg(&shaders_src_dir)
        .run_ok()
        .await?;

    info!("Optimizing extracted shaders.");
    let dist_shaders_dir = target_dist_dir.join("shaders");
    let dist_shaders_list_path = dist_shaders_dir.join("list.txt");
    ide_ci::fs::create_dir_if_missing(&dist_shaders_dir)?;

    let stages = ["vertex", "fragment"];
    let shaders_list_path = shaders_src_dir.join("list.txt");
    let shaders_list = ide_ci::fs::read_to_string(&shaders_list_path)?;
    let shaders_prefixes: Vec<_> = shaders_list.lines().collect();
    for shader_prefix in shaders_prefixes {
        info!("Optimizing '{:?}'.", shader_prefix);
        for stage in stages {
            let base_path = shaders_src_dir.join(shader_prefix).display().to_string();
            let stage_path = format!("{base_path}.{stage}");
            let stage_glsl_path = format!("{}.glsl", stage_path);
            let stage_spv_path = format!("{}.spv", stage_path);
            let stage_spv_opt_path = format!("{}.opt.spv", stage_path);
            let stage_glsl_opt_path = format!("{}.opt.glsl", stage_path);
            let stage_glsl_opt_dist_path =
                dist_shaders_dir.join(&format!("{shader_prefix}.{stage}.glsl"));

            Glslc
                .cmd()?
                .args(&[
                    "--target-env=opengl",
                    &format!("-fshader-stage={stage}"),
                    "-o",
                    &stage_spv_path,
                    &stage_glsl_path,
                ])
                .run_ok()
                .await?;
            SpirvOpt
                .cmd()?
                .args(&["-O", "-o", &stage_spv_opt_path, &stage_spv_path])
                .run_ok()
                .await?;

            SpirvCross
                .cmd()?
                .args(&["--output", &stage_glsl_opt_path, &stage_spv_opt_path])
                .run_ok()
                .await?;

            let content = ide_ci::fs::read_to_string(&stage_glsl_opt_path)?.replace("\r\n", "\n");
            let main_start_str = "void main()\n{";
            let main_end_str = "}";
            let main_start = content.find(main_start_str).with_context(|| {
                format!(
                    "Failed to find main start string '{}' in shader '{}'. Text:\n{:?}",
                    main_start_str, stage_glsl_opt_path, content
                )
            })?;
            let main_end = content.rfind(main_end_str).with_context(|| {
                format!(
                    "Failed to find main end string '{}' in shader '{}'. Text:\n{:?}",
                    main_end_str, stage_glsl_opt_path, content
                )
            })?;
            let before_main = &content[..main_start];
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
            let main_content = &content[main_start + main_start_str.len()..main_end];
            let code = format!("{}\n{}", declarations, main_content);

            ide_ci::fs::write(&stage_glsl_opt_dist_path, code)?;
        }
    }

    ide_ci::fs::write(&dist_shaders_list_path, &shaders_list)?;


    info!("DONE!");

    ide_ci::fs::copy(&target_dist_dir, &out_dir)?;

    let paths = ide_ci::fs::read_dir(&out_dir)?;
    info!("Copied files:");
    for path in paths {
        info!("Name: {}", path?.path().display())
    }

    //
    // println!("{:?}", out_dir);
    // println!("{:?}", out_name);
    // println!("{:?}", args);
    //
    //
    //
    // println!("target_dir: {}", target_dir.display());
    Ok(())
}
