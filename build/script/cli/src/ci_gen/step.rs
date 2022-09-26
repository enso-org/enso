use crate::prelude::*;

use enso_build::paths;
use ide_ci::actions::workflow::definition::env_expression;
use ide_ci::actions::workflow::definition::Step;

pub fn test_reporter(os: OS) -> Step {
    Step {
        name: Some("Stdlib test report".into()),
        uses: Some("dorny/test-reporter@v1".into()),
        r#if: Some("success() || failure()".into()),
        ..default()
    }
    .with_custom_argument("reporter", "java-junit")
    .with_custom_argument(
        "path",
        format!("{}/**/*.xml", env_expression(&paths::ENSO_TEST_JUNIT_DIR)),
    )
    .with_custom_argument("path-replace-backslashes", "true")
    .with_custom_argument("name", format!("Enso Standard Library Tests ({os})"))
}
