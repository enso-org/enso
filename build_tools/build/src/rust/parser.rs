use super::*;

use crate::paths::generated::RepoRoot;

use ide_ci::programs::cargo;
use ide_ci::programs::java;
use ide_ci::programs::javac;
use ide_ci::programs::Cargo;
use ide_ci::programs::Java;
use ide_ci::programs::Javac;
use std::fs;
use std::path::Path;



const GENERATOR_CRATE_NAME: &str = "enso-parser-generate-java";
const GENERATOR_BIN_NAME: &str = GENERATOR_CRATE_NAME;
const TEST_GENERATOR_BIN_NAME: &str = "java-tests";
const GENERATED_CODE_NAMESPACE: [&str; 3] = ["org", "enso", "syntax2"];
const GENERATED_TEST_CLASS: &str = "GeneratedFormatTests";
const JAVA_EXTENSION: &str = "java";

pub fn cargo_run_generator_cmd(repo_root: &Path, binary_name: &str) -> Result<Command> {
    let mut ret = Cargo.cmd()?;
    ret.current_dir(repo_root)
        .apply(&cargo::Command::Run)
        .apply(&cargo::Options::Package(GENERATOR_CRATE_NAME.into()))
        .apply(&cargo::RunOption::Bin(binary_name.into()));
    Ok(ret)
}

#[context("Failed to generate Java code of the new parse bindings.")]
pub async fn generate_java_to(repo_root: &Path, output_path: &Path) -> Result {
    // Generator requires that output directory exists.
    // Also, we remove its previous content so the old artifacts don't pollute the output.
    ide_ci::fs::tokio::reset_dir(&output_path).await?;
    cargo_run_generator_cmd(repo_root, GENERATOR_BIN_NAME)?
        .arg("--")
        .arg(output_path)
        .run_ok()
        .await?;

    Ok(())
}

pub async fn generate_java(repo_root: &RepoRoot) -> Result {
    let output_path = repo_root.target.generated_java.join_iter(GENERATED_CODE_NAMESPACE);
    generate_java_to(repo_root, &output_path).await
}

#[context("Running self-tests for the generated Java sources failed.")]
pub async fn run_self_tests(repo_root: &RepoRoot) -> Result {
    let base = &repo_root.target.generated_java;
    let lib = &repo_root.lib.rust.parser.generate_java.java;
    let external_dependencies_file = lib
        .ancestors()
        .nth(2)
        .unwrap()
        .join("target")
        .join("streams")
        .join("runtime")
        .join("managedClasspath")
        .join("_global")
        .join("streams")
        .join("export");

    let dependencies_from_string = fs::read_to_string(&external_dependencies_file)?;
    let dependencies_from_string = dependencies_from_string.trim_ascii_end();
    let package = repo_root.target.generated_java.join_iter(GENERATED_CODE_NAMESPACE);
    let test = package.join(GENERATED_TEST_CLASS).with_extension(JAVA_EXTENSION);
    let test_class =
        GENERATED_CODE_NAMESPACE.into_iter().chain(Some(GENERATED_TEST_CLASS)).join(".");

    let tests_code = cargo_run_generator_cmd(repo_root, TEST_GENERATOR_BIN_NAME)?
        .output_ok()
        .await?
        .into_stdout_string()?;
    ide_ci::fs::tokio::write(&test, tests_code).await?;

    Javac
        .cmd()?
        .apply(&javac::Classpath::new([
            lib.as_path(),
            base.as_path(),
            Path::new(dependencies_from_string),
        ]))
        .apply(&javac::Options::Directory(base.into()))
        .arg(&test)
        .run_ok()
        .await?;

    Java.cmd()?.apply(&java::Classpath::new([&base])).arg(&test_class).run_ok().await?;

    Ok(())
}
