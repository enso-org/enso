use manifest_dir_macros::path;
use std::env;
use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use tempfile::tempdir;

pub fn execute<T: AsRef<OsStr>>(exe: &str, args: &[T]) {
    let mut command =
        Command::new(exe).args(args).spawn().expect("Failed to start external executable");
    if !command.wait().unwrap().success() {
        panic!("Failed to execute external executable.");
    }
}

pub fn modify_arg_1(
    mut args: &mut Vec<String>,
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

fn with_pwd<T>(path: &Path, f: impl FnOnce() -> T) -> T {
    let current_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(path).unwrap();
    let out = f();
    std::env::set_current_dir(current_dir).unwrap();
    out
}

fn compile_js(main: &str, out: &Path) {
    println!("Compiling {main}.");
    execute("npx", &[
        "--yes",
        "esbuild",
        main,
        "--bundle",
        // FIXME: enable, but only on final binary, otherwise it will mangle names and we will not
        // connect there
        // "--minify",
        // FIXME:
        "--sourcemap=inline",
        "--platform=node",
        &format!("--outfile={}", out.display()),
    ]);
}

fn compile_ts(main: &str, out: &Path) {
    println!("Type checking TypeScript sources.");
    execute("npx", &["tsc", "-noEmit"]);
    compile_js(main, out);
}

fn main() {
    let mut args: Vec<String> = env::args().skip(1).collect();
    let is_build = args.contains(&"build".to_string());
    println!("{:?}", workspace_dir());
    if is_build {
        let current_cargo_path = Path::new(path!("Cargo.toml"));
        let root_dir = current_cargo_path.parent().unwrap();
        println!("{:?}", root_dir);

        // let build_dir = tempdir().expect("Failed to create temporary directory");
        let workspace_dir = workspace_dir();
        let build_dir = workspace_dir.join("target").join("ensogl-pack");
        let dist_dir = build_dir.join("dist");

        let out_dir =
            modify_arg_1(&mut args, "--out-dir", |_| Some(build_dir.display().to_string()));
        let out_dir = out_dir
            .or_else(|| modify_arg_1(&mut args, "-d", |_| Some(build_dir.display().to_string())));
        let out_name = modify_arg_1(&mut args, "--out-name", |_| Some("pkg".to_string()));
        // FIXME:
        let out_dir = out_dir.unwrap();
        let out_name = out_name.unwrap();

        with_pwd(&root_dir.join("js"), || compile_ts("src/index.ts", &dist_dir.join("index.js")));
        with_pwd(&build_dir, || compile_js("pkg.js", &dist_dir.join("snippets.js")));

        // FIXME: change it to move
        std::fs::copy(build_dir.join("pkg.wasm"), dist_dir.join(&format!("main.wasm")));



        // execute("wasm-pack", &args);


        println!("{:?}", out_dir);
        println!("{:?}", out_name);
        println!("{:?}", args);



        println!("build_dir: {}", build_dir.display());
    }
}
