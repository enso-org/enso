//! Generate IntelliJ run configurations and place them in the .idea/runConfigurations directory.

// === Features ===
#![feature(exit_status_error)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(keyword_idents)]
#![deny(macro_use_extern_crate)]
#![deny(missing_abi)]
#![deny(pointer_structural_match)]
#![deny(unsafe_op_in_unsafe_fn)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(absolute_paths_not_starting_with_crate)]
#![warn(elided_lifetimes_in_paths)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_crate_dependencies)]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![warn(variant_size_differences)]
#![warn(unreachable_pub)]

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Debug;
use std::fs;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;
use std::time::Duration;
use std::time::Instant;
use toml::Value;


// ==================
// === Processing ===
// ==================

/// A path to rust source annotated with information whether it is a main or a library main source
/// file.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct RustSourcePath {
    path: PathBuf,
}


fn process_paths<T: AsRef<Path>>(paths: &[T]) {
    let start_time = Instant::now();
    let run_config_path = Path::new("./.idea/runConfigurations");
    fs::remove_dir_all(run_config_path).ok();
    fs::create_dir(run_config_path).unwrap();
    let paths = discover_paths(paths);
    let paths: HashMap<String, Option<Crate>> = paths
        .into_iter()
        .map(|(k, v)| (k, Some(v)))
        .chain(std::iter::once(("all".to_string(), None)))
        .collect();

    println!("Generating run configurations.");
    let mut base_command_configs = vec![
        ("clippy", TargetConfig::default_clippy_set(), false),
        ("test", TargetConfig::default_test_set(), false),
    ];

    for (name, crate_def) in &paths {
        let mut command_configs = base_command_configs.clone();
        if let Some(crate_def) = crate_def {
            if crate_def.is_bin {
                command_configs.push(("run", TargetConfig::default_run_set(), true));
            };
        }
        for (command, configs, is_bin) in &command_configs {
            for config in configs {
                let file_name_suffix = config.to_file_name_suffix();
                let file_name = format!("{command}-{name}{file_name_suffix}.xml");
                let path = run_config_path.join(&file_name);
                let xml = generate_xml(command, crate_def.as_ref(), config, *is_bin);
                fs::write(&path, xml).expect("Unable to write the run configuration to {path:?}.");
            }
        }
    }
    let time = start_time.elapsed();
    println!("Done in {time:?}.");
}

/// Discover all paths containing Rust sources, recursively.
fn discover_paths<T: AsRef<Path>>(paths: &[T]) -> HashMap<String, Crate> {
    let start = Instant::now();
    println!("Searching for crates.");
    let mut map = Default::default();
    for path in paths {
        let path = path.as_ref();
        discover_paths_internal(&mut map, path, path);
    }
    let duration = start.elapsed();
    println!("Found {} crates in {:?}.", map.len(), duration);
    map
}

fn discover_paths_internal(map: &mut HashMap<String, Crate>, root_path: &Path, path: &Path) {
    let md = fs::metadata(path);
    let md = md.unwrap_or_else(|_| panic!("Could get metadata of {}", path.display()));
    if md.is_dir() {
        let file_name = path.file_name().map(|t| t.to_string_lossy().to_string());
        if file_name.map(|t| DIR_BLACK_LIST.contains(&t.as_ref())).unwrap_or(false) {
            return;
        }
        let sub_paths = fs::read_dir(path).unwrap();
        for sub_path in sub_paths {
            discover_paths_internal(map, root_path, &sub_path.unwrap().path())
        }
    } else if path.extension() == Some(OsStr::new("toml")) {
        let file_name = path.file_name().and_then(|s| s.to_str());
        if file_name == Some("Cargo.toml") {
            let content = fs::read_to_string(path).unwrap();
            let value = content.parse::<Value>().unwrap();

            if let Some(package) = value.get("package") {
                let name = package.as_table().unwrap()["name"].as_str().unwrap().to_string();

                if let Some(parent_path) = path.parent() {
                    if PATH_BLACK_LIST.contains(&&*parent_path.to_string_lossy()) {
                        return;
                    }

                    let mut is_bin = parent_path.join("src").join("main.rs").exists();

                    let root_path = root_path.into();
                    let path = path.into();
                    map.insert(name.clone(), Crate { name, root_path, path, is_bin });
                }
            }
        }
    }
}


const PATH_BLACK_LIST: &'static [&'static str] = &["./build", "./integration-test"];
const DIR_BLACK_LIST: &'static [&'static str] = &["target", "dist"];

fn main() {
    // process_paths(&["./app", "./lib/rust", "./build"]);
    process_paths(&["."]);
}

#[derive(Debug, Clone, Copy)]
struct TargetConfig {
    pub wasm:    bool,
    pub macos:   bool,
    pub linux:   bool,
    pub windows: bool,
}

impl TargetConfig {
    fn new_wasm() -> Self {
        Self { wasm: true, macos: false, linux: false, windows: false }
    }

    fn default_clippy_set() -> Vec<Self> {
        vec![
            Self { wasm: true, macos: false, linux: false, windows: false },
            Self { wasm: true, macos: true, linux: false, windows: false },
            Self { wasm: true, macos: false, linux: true, windows: false },
            Self { wasm: true, macos: false, linux: false, windows: true },
        ]
    }

    fn default_test_set() -> Vec<Self> {
        vec![Self { wasm: false, macos: false, linux: false, windows: false }]
    }

    fn default_run_set() -> Vec<Self> {
        vec![Self { wasm: false, macos: false, linux: false, windows: false }]
    }

    fn to_name_vector(&self) -> Vec<&'static str> {
        let mut vec = vec![];
        if self.wasm {
            vec.push("wasm");
        }
        if self.macos {
            vec.push("macos");
        }
        if self.linux {
            vec.push("linux");
        }
        if self.windows {
            vec.push("windows");
        }
        if vec.is_empty() {
            vec.push("native");
        }
        vec
    }

    fn to_target_vector(&self) -> Vec<&'static str> {
        let mut vec = vec![];
        if self.wasm {
            vec.push("wasm32-unknown-unknown");
        }
        if self.macos {
            vec.push("aarch64-apple-darwin");
        }
        if self.linux {
            vec.push("x86_64-unknown-linux-gnu");
        }
        if self.windows {
            vec.push("x86_64-pc-windows-msvc");
        }
        vec
    }

    fn to_file_name_suffix(&self) -> String {
        format!("-{}", self.to_name_vector().join("-"))
    }

    fn name_suffix(&self) -> String {
        format!(" [{}]", self.to_name_vector().join(", "))
    }

    fn target_dir(&self) -> String {
        format!("--target-dir dist/intellij_{}", self.to_name_vector().join("_"))
    }

    fn targets(&self) -> String {
        let targets = self.to_target_vector();
        if targets.is_empty() {
            "".into()
        } else {
            let opts = targets.into_iter().map(|t| format!("--target {t}"));
            format!("-Zmultitarget {}", opts.collect::<Vec<String>>().join(" "))
        }
    }
}

pub struct Crate {
    pub name:      String,
    pub root_path: PathBuf,
    pub path:      PathBuf,
    pub is_bin:    bool,
}

const FOLDER_NAME_RULES: &'static [(&'static str, &'static str)] = &[
    ("build/", "🚧 Build          "),
    ("tools/", "🔨 Tools         "),
    ("lib/rust/ensogl/example/", "🧸 Example    "),
    ("lib/rust/ensogl/", "🎨 EnsoGl      "),
    ("lib/rust/ensogl", "🎨 EnsoGl"),
    ("lib/rust/parser/", "👓 Parser       "),
    ("lib/rust/parser", "👓 Parser"),
    ("lib/rust/", "📚 Lib             "),
    ("app/gui/", "🚥 GUI            "),
    ("app/gui", "🚥 GUI"),
];

impl Crate {
    pub fn folder_name(&self) -> String {
        let rel = self.path.parent().unwrap().strip_prefix(&self.root_path).unwrap();
        let mut path = rel.to_string_lossy().to_string();
        for (prefix, new_prefix) in FOLDER_NAME_RULES {
            if path.starts_with(prefix) {
                path = format!("{}{}", new_prefix, &path[prefix.len()..]);
                break;
            }
        }
        path
    }
}

fn generate_xml(
    command: &str,
    crate_target: Option<&Crate>,
    target_config: &TargetConfig,
    binary: bool,
) -> String {
    let package_name = crate_target.map(|t| t.name.clone()).unwrap_or_default();
    let name = format!("{command}/{package_name}{}", target_config.name_suffix());
    let folder = crate_target.map(|t| t.folder_name()).unwrap_or_else(|| "🌎 World".into());
    let target_dir = target_config.target_dir();
    let targets = target_config.targets();
    let flags = if binary { "" } else { "--all-targets" };
    let package = crate_target
        .map(|t| {
            if binary {
                format!("--package {} --bin {}", t.name, t.name)
            } else {
                format!("--package {}", t.name)
            }
        })
        .unwrap_or_default();
    format!(
        r###"
<component name="#ProjectRunConfigurationManager">
  <configuration 
    default="false" 
    name="{name}"
    type="CargoCommandRunConfiguration" 
    factoryName="Cargo Command"
    folderName="{folder}"
  >
    <option name="command" value="{command} {flags} {targets} {target_dir} {package}" />
    <option name="workingDirectory" value="file://$PROJECT_DIR$" />
    <option name="channel" value="DEFAULT" />
    <option name="requiredFeatures" value="true" />
    <option name="allFeatures" value="false" />
    <option name="emulateTerminal" value="false" />
    <option name="withSudo" value="false" />
    <option name="buildTarget" value="REMOTE" />
    <option name="backtrace" value="SHORT" />
    <envs />
    <option name="isRedirectInput" value="false" />
    <option name="redirectInputPath" value="" />
    <method v="2">
      <option name="CARGO.BUILD_TASK_PROVIDER" enabled="true" />
    </method>
  </configuration>
</component>
    "###
    )
}
