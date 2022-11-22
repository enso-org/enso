use crate::prelude::*;
use sysinfo::Pid;
use sysinfo::Process;
use sysinfo::ProcessRefreshKind;
use sysinfo::System;

use tokio::process::Child;



pub mod env {
    use super::*;

    ide_ci::define_env_var! {
        /// Environment variable that stores URL under which spawned httpbin server is available.
        ENSO_HTTP_TEST_HTTPBIN_URL, Url;
    }
}

#[derive(Debug)]
pub struct Spawned {
    pub process: Child,
    pub url:     Url,
}

pub async fn get_and_spawn_httpbin(
    sbt: &crate::engine::sbt::Context,
    port: u16,
) -> Result<Spawned> {
    let process = sbt
        .command()?
        .arg(format!("simple-httpbin/run localhost {port}"))
        .kill_on_drop(true)
        .spawn()?;

    let url_string = format!("http://localhost:{port}");
    let url = Url::parse(&url_string)?;
    env::ENSO_HTTP_TEST_HTTPBIN_URL.set(&url)?;
    Ok(Spawned { url, process })
}

impl Drop for Spawned {
    fn drop(&mut self) {
        debug!("Dropping the httpbin wrapper.");
        env::ENSO_HTTP_TEST_HTTPBIN_URL.remove();
        let Some(pid) = self.process.id().map(Pid::from_u32) else {
            error!("Failed to get PID of the httpbin process.");
            return;
        };
        let mut system = sysinfo::System::new();
        ProcessHierarchy::new(&mut system).kill_process_subtree(pid);
    }
}

pub async fn get_and_spawn_httpbin_on_free_port(
    sbt: &crate::engine::sbt::Context,
) -> Result<Spawned> {
    get_and_spawn_httpbin(sbt, ide_ci::get_free_port()?).await
}

#[derive(Debug, Clone)]
pub struct ProcessHierarchy<'a> {
    pub processes: &'a HashMap<Pid, Process>,
    pub children:  HashMap<Pid, HashSet<Pid>>,
}

impl<'a> ProcessHierarchy<'a> {
    pub fn new(system: &'a mut System) -> Self {
        trace!("Refreshing system information.");
        system.refresh_processes_specifics(ProcessRefreshKind::default());
        let processes = system.processes();
        let mut children = HashMap::<_, HashSet<Pid>>::new();
        for (pid, process) in processes.iter() {
            let parent_pid = process.parent();
            if let Some(parent_pid) = parent_pid {
                children.entry(parent_pid).or_default().insert(*pid);
            }
        }
        Self { processes, children }
    }

    pub fn kill_process_subtree(&self, pid: Pid) {
        if let Some(children) = self.children.get(&pid) {
            for child in children {
                self.kill_process_subtree(*child);
            }
        }
        if let Some(process) = self.processes.get(&pid) {
            let name = process.name();
            let command = process.cmd();
            trace!(%pid, %name, ?command, "Killing process.");
            process.kill();
        } else {
            warn!(%pid, "Failed to kill process. It does not exist.");
        }
    }
}


#[cfg(test)]
mod tests {
    use ide_ci::cache;
    use ide_ci::env::current_dir;
    use std::env::set_current_dir;

    use super::*;


    #[tokio::test]
    #[ignore]
    async fn spawn() -> Result {
        setup_logging()?;
        set_current_dir(r"H:\NBO\enso5")?;
        let cache = cache::Cache::new_default().await?;
        cache::goodie::sbt::Sbt.install_if_missing(&cache).await?;

        let sbt = crate::engine::sbt::Context {
            repo_root:         current_dir()?,
            system_properties: vec![],
        };

        let spawned = get_and_spawn_httpbin_on_free_port(&sbt).await?;
        std::thread::sleep(std::time::Duration::from_secs(20));
        dbg!(&spawned);


        Ok(())
    }
}
