use crate::prelude::*;
use crate::ENSO_RELEASE_ID;
use ide_ci::actions::workflow::definition::checkout_repo_step;
use ide_ci::actions::workflow::definition::Job;
use ide_ci::actions::workflow::definition::RunnerLabel;
use ide_ci::actions::workflow::definition::Step;
use ide_ci::actions::workflow::definition::Workflow;
use ide_ci::github::GITHUB_TOKEN;

pub fn run_bin(binary: &str) -> Step {
    let command = format!("cargo run --package enso-shader-tools --bin {binary}");
    ide_ci::actions::workflow::definition::shell(command)
}

pub fn job_that_runs(bin: &str, runs_on: RunnerLabel, output: Option<&str>) -> Job {
    let checkout_steps = checkout_repo_step();

    let mut job = Job::new(format!("Run {bin} ({runs_on:?})"), [runs_on]);
    job.steps.extend(checkout_steps);
    let main_step = run_bin(bin).with_secret_exposed_as("CI_PRIVATE_TOKEN", GITHUB_TOKEN);
    if let Some(output) = output {
        job.add_step_with_output(main_step, output);
    } else {
        job.steps.push(main_step);
    }
    job
}

pub async fn ci_gen() -> Result {
    // FIXME switch to our runners once CMake is added
    let linux = RunnerLabel::LinuxLatest;
    let windows = RunnerLabel::WindowsLatest;
    let macos = RunnerLabel::MacOSLatest;

    let mut workflow = Workflow::new("Package Tools");
    workflow.on.workflow_dispatch(default());
    workflow.on.push(default());
    let create_release_job = job_that_runs("create", linux, Some(ENSO_RELEASE_ID.as_ref()));
    let create_job_id = workflow.add_job(create_release_job);

    let package_job_ids = [linux, windows, macos]
        .into_iter()
        .map(|target| {
            let mut job = job_that_runs("package", target, None);
            workflow.expose_outputs(&create_job_id, &mut job);
            workflow.add_job(job)
        })
        .collect_vec();

    let mut publish_job = job_that_runs("publish", linux, None);
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
