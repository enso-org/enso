use crate::prelude::*;

use crate::ci_gen::not_a_fork;
use crate::paths;

use ide_ci::actions::workflow::definition::env_expression;
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
