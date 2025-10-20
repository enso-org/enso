use crate::prelude::*;

use crate::ci_gen::not_a_fork;
use crate::engine;
use crate::paths;

use ide_ci::actions::workflow::definition::env_expression;
use ide_ci::actions::workflow::definition::Shell;
use ide_ci::actions::workflow::definition::Step;
use ide_ci::actions::workflow::definition::Target;
use ide_ci::cache::goodie::graalvm;



pub fn test_reporter(
    step_name: impl Into<String>,
    report_name: impl Into<serde_yaml::Value>,
    path: impl Into<serde_yaml::Value>,
) -> Step {
    Step {
        name: Some(step_name.into()),
        uses: Some("dorny/test-reporter@v1".into()),
        // The action does not support running on forks.
        r#if: Some(format!("(success() || failure()) && {}", not_a_fork())),
        ..default()
    }
    .with_custom_argument("reporter", "java-junit")
    .with_custom_argument("path", path)
    .with_custom_argument("path-replace-backslashes", true)
    .with_custom_argument("max-annotations", 50) // 50 is the max
    .with_custom_argument("name", report_name)
}

pub fn stdlib_test_reporter((os, arch): Target, graal_edition: graalvm::Edition) -> Step {
    let step_name = "Standard Library Test Reporter";
    let report_name = format!("Standard Library Tests Report ({graal_edition}, {os}, {arch})");
    let path = format!("{}/*/*.xml", env_expression(&paths::ENSO_TEST_JUNIT_DIR));
    test_reporter(step_name, report_name, path)
}

pub fn engine_test_reporter((os, arch): Target, graal_edition: graalvm::Edition) -> Step {
    let step_name = "Engine Test Reporter";
    let report_name = format!("Engine Tests Report ({graal_edition}, {os}, {arch})");
    let path = format!("{}/*.xml", env_expression(&paths::ENSO_TEST_JUNIT_DIR));
    test_reporter(step_name, report_name, path)
}

pub fn extra_stdlib_test_reporter((os, arch): Target, graal_edition: graalvm::Edition) -> Step {
    let step_name = "Extra Library Test Reporter";
    let report_name = format!("Extra Library Tests Report ({graal_edition}, {os}, {arch})");
    let path = format!("{}/*/*.xml", env_expression(&paths::ENSO_TEST_JUNIT_DIR));
    test_reporter(step_name, report_name, path)
}

/// Upload heap dump of a crashed JVM on OutOfMemoryError.
/// Note that there may be multiple `*.hprof` files if multiple processes crashed.
/// `artifact_name` should be unique for each job in the whole workflow.
pub fn heapdump_upload(artifact_name: impl Into<String>) -> Step {
    let path = "test/**/*.hprof\nengine/**/*.hprof";

    let mut step = upload_artifact("Upload Heap Dumps")
        .with_custom_argument("name", artifact_name.into())
        .with_custom_argument("path", path)
        .with_custom_argument("retention-days", 3)
        .with_custom_argument("if-no-files-found", "ignore");
    // This step should be run every time, but not on forks.
    step.r#if = Some(format!("(success() || failure()) && {}", not_a_fork()));
    step
}

pub fn upload_engine_distribution(
    target: Target,
    engine_launcher: engine::EngineLauncher,
    graal_edition: graalvm::Edition,
) -> Step {
    upload_artifact("Upload Engine Distribution")
        .with_custom_argument(
            "name",
            format!(
                "Engine Distribution ({}) ({}) ({}, {})",
                graal_edition, engine_launcher, target.0, target.1
            ),
        )
        .with_custom_argument("path", "built-distribution.tar")
}

pub fn download_engine_distribution(
    target: Target,
    engine_launcher: engine::EngineLauncher,
    graal_edition: graalvm::Edition,
) -> Step {
    download_artifact("Download Engine Distribution").with_custom_argument(
        "name",
        format!(
            "Engine Distribution ({}) ({}) ({}, {})",
            graal_edition, engine_launcher, target.0, target.1
        ),
    )
}

pub fn check_engine_distribution() -> Step {
    Step {
        run: Some("ls -l built-distribution.tar".into()),
        shell: Some(Shell::Bash),
        ..Default::default()
    }
}

pub fn unpack_engine_distribution() -> Step {
    Step {
        name: Some("Unpack Engine Distribution".into()),
        run: Some(
            "tar -xvf built-distribution.tar -C .
rm built-distribution.tar"
                .into(),
        ),
        ..Default::default()
    }
}

pub fn archive_engine_distribution(engine_launcher: engine::EngineLauncher) -> Step {
    let command = format!(
        "tar -cvf built-distribution.tar {}",
        built_distribution_directories(engine_launcher)
    );
    Step {
        name: Some("Archive Engine Distribution".into()),
        run: Some(command),
        ..Default::default()
    }
}

pub fn cleanup_engine_distribution(engine_launcher: engine::EngineLauncher) -> Step {
    Step {
        run: Some(format!("rm -rf {}", built_distribution_directories(engine_launcher))),
        shell: Some(Shell::Bash),
        ..Default::default()
    }
}

fn built_distribution_directories(engine_launcher: engine::EngineLauncher) -> String {
    format!("built-distribution{}", match engine_launcher {
        engine::EngineLauncher::TestNative => " test",
        engine::EngineLauncher::TestDebugNative => " test",
        _ => "",
    })
}

pub fn upload_artifact(step_name: impl Into<String>) -> Step {
    Step {
        name: Some(step_name.into()),
        uses: Some("actions/upload-artifact@v4".into()),
        ..default()
    }
}

pub fn download_artifact(step_name: impl Into<String>) -> Step {
    Step {
        name: Some(step_name.into()),
        uses: Some("actions/download-artifact@v4".into()),
        ..default()
    }
}
