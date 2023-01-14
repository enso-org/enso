#![feature(async_closure)]

pub use ide_ci::prelude;
use ide_ci::prelude::*;
use ide_ci::program::EMPTY_ARGS;
use manifest_dir_macros::path;
use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

pub mod shaderc;


pub fn execute<T: AsRef<OsStr>>(exe: &str, args: &[T]) {
    let args_vec: Vec<_> = args.iter().map(|t| format!("{:?}", t.as_ref())).collect();
    println!("Executing: {} {}", exe, args_vec.join(" "));
    let mut command =
        Command::new(exe).args(args).spawn().expect("Failed to start external executable");
    if !command.wait().unwrap().success() {
        panic!("Failed to execute external executable.");
    }
}

pub fn modify_arg_1(
    args: &mut Vec<String>,
    arg: &str,
    f: impl Fn(&str) -> Option<String>,
) -> Option<String> {
    let mut old_value = None;
    let eq_arg = format!("{}=", arg);
    let index = args.iter().position(|a| a == arg);
    let eq_index = args.iter().position(|a| a.starts_with(&eq_arg));
    if let Some(index) = index {
        if index + 1 < args.len() {
            old_value = Some(args[index + 1].clone());
            match f(&args[index + 1]) {
                Some(new_value) => args[index + 1] = new_value,
                None => {
                    args.remove(index + 1);
                }
            }
        } else {
            old_value = Some("".to_string());
            if let Some(new_value) = f("") {
                args.push(new_value);
            }
        }
    } else if let Some(index) = eq_index {
        let value = args[index].split_at(eq_arg.len()).1;
        old_value = Some(value.to_string());
        match f(value) {
            Some(new_value) => args[index] = format!("{}{}", eq_arg, new_value),
            None => {
                args.remove(index);
            }
        }
    }
    old_value
}

fn workspace_dir() -> PathBuf {
    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    let cargo_path = Path::new(std::str::from_utf8(&output).unwrap().trim());
    cargo_path.parent().unwrap().to_path_buf()
}

pub fn root_dir() -> &'static Path {
    let current_cargo_path = Path::new(path!("Cargo.toml"));
    current_cargo_path.parent().unwrap()
}

// fn with_pwd<T>(path: &Path, f: impl FnOnce() -> T) -> T {
//     let current_dir = std::env::current_dir().unwrap();
//     std::env::set_current_dir(path).unwrap();
//     let out = f();
//     std::env::set_current_dir(current_dir).unwrap();
//     out
// }

async fn compile_js(target_dir: &Path, main: &str, out: &Path) -> Result {
    println!("Compiling {main}.");
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

    println!("Type checking TypeScript sources.");
    run_script("typecheck", &EMPTY_ARGS).await?;

    println!("Linting TypeScript sources.");
    run_script("lint", &EMPTY_ARGS).await?;

    println!("Building TypeScript sources.");
    run_script("build", &["--", &format!("--outfile={}", out.display())]).await
}

async fn main_lib(mut args: Vec<String>) -> Result {
    ide_ci::env::prepend_to_path(r"C:\varia\install\bin")?;
    setup_logging()?;
    let is_build = args.contains(&"build".to_string());
    println!("{:?}", workspace_dir());
    if is_build {
        let current_cargo_path = Path::new(path!("Cargo.toml"));
        let root_dir = current_cargo_path.parent().unwrap();
        println!("{:?}", root_dir);

        // let target_dir = tempdir().expect("Failed to create temporary directory");
        let workspace_dir = workspace_dir();
        let target_dir = workspace_dir.join("target").join("ensogl-pack");
        let target_dist_dir = target_dir.join("dist");

        let out_dir =
            modify_arg_1(&mut args, "--out-dir", |_| Some(target_dir.display().to_string()));
        let out_dir = out_dir
            .or_else(|| modify_arg_1(&mut args, "-d", |_| Some(target_dir.display().to_string())));
        let out_name = modify_arg_1(&mut args, "--out-name", |_| Some("pkg".to_string()));

        println!("Executing wasm-pack with args: {:?}", args);
        ide_ci::programs::WasmPack.cmd()?.args(&args).run_ok().await?;
        // execute("wasm-pack", &args);


        // FIXME:
        let out_dir = out_dir.unwrap();
        let out_name = out_name.unwrap();
        let out_dir = Path::new(&out_dir);
        let js_dir = root_dir.join("js");
        let node_modules_dir = js_dir.join("node_modules");
        let app_js_path = target_dist_dir.join("app.js");

        // FIXME? [mwu] What if dependencies are updated without deleting node_modules?
        if !node_modules_dir.is_dir() {
            ide_ci::programs::Npm.cmd()?.install().current_dir(&js_dir).run_ok().await?;
            // with_pwd(&js_dir, || execute("npm", &["install"]));
        }
        compile_ts(&js_dir, "src/index.ts", &app_js_path).await?;
        compile_js(&target_dir, "pkg.js", &target_dist_dir.join("main.js")).await?;
        // with_pwd(&target_dir, || compile_js("pkg.js", &target_dist_dir.join("main.js")));
        //
        // println!(
        //     "Copying {:?} to {:?}",
        //     target_dir.join("pkg.wasm"),
        //     target_dist_dir.join(&format!("main.wasm"))
        // );
        ide_ci::fs::copy(target_dir.join("pkg_bg.wasm"), target_dist_dir.join("main.wasm"))?;
        println!("Source files:");
        let paths = ide_ci::fs::read_dir(&target_dist_dir)?;
        for path in paths {
            println!("Name: {}", path?.path().display())
        }


        println!("Extracting shaders from generated WASM file.");
        let shaders_src_dir = target_dir.join("shaders");
        ide_ci::programs::Node
            .cmd()?
            .args([app_js_path.as_str(), "--extract-shaders", shaders_src_dir.as_str()])
            .run_ok()
            .await?;

        println!("Optimizing extracted shaders.");
        let dist_shaders_dir = target_dist_dir.join("shaders");
        let dist_shaders_list_path = dist_shaders_dir.join("list.txt");
        ide_ci::fs::create_dir_if_missing(&dist_shaders_dir)?;

        let stages = ["vertex", "fragment"];
        let shaders_list_path = shaders_src_dir.join("list.txt");
        let shaders_list = std::fs::read_to_string(&shaders_list_path).unwrap();
        let shaders_prefixes: Vec<_> = shaders_list.lines().collect();
        for shader_prefix in shaders_prefixes {
            println!("Optimizing '{:?}'.", shader_prefix);
            for stage in stages {
                let base_path = shaders_src_dir.join(shader_prefix).display().to_string();
                let stage_path = format!("{base_path}.{stage}");
                let stage_glsl_path = format!("{}.glsl", stage_path);
                let stage_spv_path = format!("{}.spv", stage_path);
                let stage_spv_opt_path = format!("{}.opt.spv", stage_path);
                let stage_glsl_opt_path = format!("{}.opt.glsl", stage_path);
                let stage_glsl_opt_dist_path =
                    dist_shaders_dir.join(&format!("{shader_prefix}.{stage}.glsl"));

                execute("glslc", &[
                    "--target-env=opengl",
                    &format!("-fshader-stage={stage}"),
                    "-o",
                    &stage_spv_path,
                    &stage_glsl_path,
                ]);
                execute("spirv-opt", &["-O", "-o", &stage_spv_opt_path, &stage_spv_path]);
                execute("spirv-cross", &["--output", &stage_glsl_opt_path, &stage_spv_opt_path]);

                let content =
                    ide_ci::fs::read_to_string(&stage_glsl_opt_path)?.replace("\r\n", "\n");
                let main_start_str = "void main()\n{";
                let main_end_str = "}";
                let main_start = content.find(main_start_str).with_context(|| {
                    format!(
                        "Failed to find main start string '{}' in shader '{}'.",
                        main_start_str, stage_glsl_opt_path
                    )
                })?;
                let main_end = content.rfind(main_end_str).with_context(|| {
                    format!(
                        "Failed to find main end string '{:?}' in shader '{}'.",
                        main_end_str, stage_glsl_opt_path
                    )
                })?;
                let main_content = &content[main_start + main_start_str.len()..main_end];

                ide_ci::fs::write(&stage_glsl_opt_dist_path, main_content)?;
            }
        }

        ide_ci::fs::write(&dist_shaders_list_path, &shaders_list)?;


        println!("DONE!");

        ide_ci::fs::copy(&target_dist_dir, &out_dir)?;

        let paths = ide_ci::fs::read_dir(&out_dir)?;
        println!("Copied files:");
        for path in paths {
            println!("Name: {}", path?.path().display())
        }

        //
        // println!("{:?}", out_dir);
        // println!("{:?}", out_name);
        // println!("{:?}", args);
        //
        //
        //
        // println!("target_dir: {}", target_dir.display());
    } else {
        ide_ci::programs::WasmPack.cmd()?.args(&args).run_ok().await?;
    }
    Ok(())
}
