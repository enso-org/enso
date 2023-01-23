//! Logic that generates CI workflow definition that is used to build the shaderc packages.

use crate::prelude::*;
use crate::ENSO_RELEASE_ID;
use ide_ci::actions::workflow::definition::checkout_repo_step;
use ide_ci::actions::workflow::definition::Job;
use ide_ci::actions::workflow::definition::RunnerLabel;
use ide_ci::actions::workflow::definition::Step;
use ide_ci::actions::workflow::definition::Workflow;
use ide_ci::github::GITHUB_TOKEN;

#[derive(Clone, Copy, Debug)]
pub enum Binary {
    Create,
    Package,
    Publish,
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Create => "create",
            Self::Package => "package",
            Self::Publish => "publish",
        };
        write!(f, "{name}")
    }
}

/// Generate a step definition that runs given binary from this crate.
pub fn run_bin(binary: Binary) -> Step {
    let command = format!("cargo run --package enso-shader-tools --bin {binary}");
    ide_ci::actions::workflow::definition::shell(command)
}

pub fn job_that_runs(binary: Binary, runs_on: RunnerLabel, output: Option<&str>) -> Job {
    let checkout_steps = checkout_repo_step();

    let mut job = Job::new(format!("Run {binary} ({runs_on:?})"), [runs_on]);
    job.steps.extend(checkout_steps);
    let main_step = run_bin(binary).with_secret_exposed_as("CI_PRIVATE_TOKEN", GITHUB_TOKEN);
    if let Some(output) = output {
        job.add_step_with_output(main_step, output);
    } else {
        job.steps.push(main_step);
    }
    job
}

pub async fn ci_gen() -> Result {
    // TODO? [mwu] Once CMake is added, we might want to switch to self-hosted runners.
    let linux = RunnerLabel::LinuxLatest;
    let windows = RunnerLabel::WindowsLatest;
    let macos = RunnerLabel::MacOSLatest;

    let mut workflow = Workflow::new("Package Tools");
    workflow.on.workflow_dispatch(default());
    workflow.on.push(default());
    let create_release_job = job_that_runs(Binary::Create, linux, Some(ENSO_RELEASE_ID.as_ref()));
    let create_job_id = workflow.add_job(create_release_job);

    let package_job_ids = [linux, windows, macos]
        .into_iter()
        .map(|target| {
            let mut job = job_that_runs(Binary::Package, target, None);
            workflow.expose_outputs(&create_job_id, &mut job);
            workflow.add_job(job)
        })
        .collect_vec();

    let mut publish_job = job_that_runs(Binary::Publish, linux, None);
    workflow.expose_outputs(&create_job_id, &mut publish_job);
    for package_job_id in package_job_ids {
        publish_job.needs(package_job_id);
    }
    workflow.add_job(publish_job);


    let yaml = serde_yaml::to_string(&workflow)?;
    println!("{}", yaml);
    ide_ci::fs::tokio::write("../../.github/workflows/shader-tools.yml", yaml).await?;


    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore]
    async fn ci_gen() -> Result {
        super::ci_gen().await
    }
}
