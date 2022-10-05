use crate::prelude::*;

use crate::env::new::TypedVariable;
use crate::extensions::child::ChildExt;

use shrinkwraprs::Shrinkwrap;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::process::Stdio;
use std::str::FromStr;



#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Hash)]
pub enum NetworkDriver {
    // Linux
    Bridge,
    Host,
    Overlay,
    Ipvlan,
    Macvlan,
    None,
    // Windows
    Ics,
    Nat,
    Transparent,
    L2bridge,
    Null,
    //
    Other(String),
}

impl AsRef<str> for NetworkDriver {
    fn as_ref(&self) -> &str {
        match self {
            NetworkDriver::Bridge => "bridge",
            NetworkDriver::Host => "host",
            NetworkDriver::Overlay => "overlay",
            NetworkDriver::Ipvlan => "ipvlan",
            NetworkDriver::Macvlan => "macvlan",
            NetworkDriver::None => "none",
            NetworkDriver::Ics => "ics",
            NetworkDriver::Nat => "nat",
            NetworkDriver::Transparent => "transparent",
            NetworkDriver::L2bridge => "l2bridge",
            NetworkDriver::Null => "null",
            NetworkDriver::Other(name) => name.as_str(),
        }
    }
}

impl Default for NetworkDriver {
    fn default() -> Self {
        if TARGET_OS == OS::Windows {
            NetworkDriver::Nat
        } else {
            NetworkDriver::Bridge
        }
    }
}


#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Hash)]
pub struct NetworkInfo {
    pub id:     String,
    pub name:   String,
    pub driver: NetworkDriver,
    pub scope:  String,
}

#[derive(Clone, Debug)]
pub struct Credentials {
    pub username: String,
    pub password: String,
    pub server:   String,
}

impl Credentials {
    pub fn new(
        username: impl Into<String>,
        password: impl Into<String>,
        server: impl Into<String>,
    ) -> Self {
        Self { username: username.into(), password: password.into(), server: server.into() }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Docker;

impl Program for Docker {
    fn executable_name(&self) -> &'static str {
        "docker"
    }
}

impl Docker {
    pub async fn build(&self, options: BuildOptions) -> Result<ImageId> {
        let mut command = self.cmd()?;
        command.arg("build").args(options.args());
        debug!("{:?}", command);
        let output = command.output_ok().await?;
        trace!("Output: {:?}", output);
        let built_image_id = std::str::from_utf8(&output.stdout)?
            .lines()
            .inspect(|line| debug!("{}", line))
            .filter(|line| !line.is_empty())
            .last()
            .ok_or_else(|| anyhow!("Docker provided no output"))?
            .split(' ')
            .last()
            .ok_or_else(|| anyhow!("The last line has no space!"))?;
        debug!("Image {} successfully built!", built_image_id);
        Ok(ImageId(built_image_id.into()))
    }

    pub fn run_cmd(&self, options: &RunOptions) -> Result<Command> {
        let mut cmd = self.cmd()?;
        cmd.arg("run").args(options.args());
        Ok(cmd)
    }

    pub async fn run(&self, options: &RunOptions) -> Result {
        self.run_cmd(options)?.run_ok().await
    }

    pub async fn create(&self, options: &RunOptions) -> Result<ContainerId> {
        let output = self.cmd()?.arg("create").args(options.args()).output_ok().await?;
        Ok(ContainerId(output.single_line_stdout()?))
    }

    pub async fn remove_container(&self, name: &ContainerId, force: bool) -> Result {
        let force_arg = if force { ["-f"].as_slice() } else { [].as_slice() };
        self.cmd()?.arg("rm").args(force_arg).arg(name.as_ref()).run_ok().await
    }

    pub async fn run_detached(&self, options: &RunOptions) -> Result<ContainerId> {
        let output =
            dbg!(self.cmd()?.arg("run").arg("-d").args(options.args())).output_ok().await?;
        // dbg!(&output);
        Ok(ContainerId(output.single_line_stdout()?))
        // output.status.exit_ok()?;
    }

    pub async fn kill(&self, target: impl AsRef<str>) -> Result {
        Docker.cmd()?.args(["kill", target.as_ref()]).run_ok().await
    }

    pub async fn upload(
        &self,
        from: impl AsRef<Path>,
        container: &ContainerId,
        to: impl AsRef<Path>,
    ) -> Result {
        self.cmd()?
            .arg("cp")
            .arg("--archive")
            .arg(from.as_ref())
            .arg(format!("{}:{}", container.as_str(), to.as_ref().display()))
            .run_ok()
            .await
    }

    pub async fn start(&self, container: &ContainerId) -> Result {
        self.cmd()?.arg("start").arg(container.as_str()).run_ok().await
    }

    /// Returns network ID.
    pub async fn create_network(
        &self,
        driver: &NetworkDriver,
        name: impl AsRef<str>,
    ) -> Result<String> {
        Docker
            .cmd()?
            .args(["network", "create", "--driver", driver.as_ref(), name.as_ref()])
            .output_ok()
            .await?
            .single_line_stdout()
    }

    /// Returns network ID.
    pub async fn remove_network(&self, name_or_id: impl AsRef<str>) -> Result<String> {
        Docker
            .cmd()?
            .args(["network", "rm", name_or_id.as_ref()])
            .output_ok()
            .await?
            .single_line_stdout()
    }

    pub async fn list_networks(&self) -> Result<Vec<NetworkInfo>> {
        let mut cmd = Docker.cmd()?;
        cmd.args(["network", "ls", "--no-trunc"]);
        cmd.stdout(Stdio::piped());
        let stdout = cmd.output_ok().await?.stdout;
        let stdout = String::from_utf8(stdout)?;

        let mut ret = Vec::new();
        for line in stdout.lines().skip(1) {
            // Network name can contain spaces, e.g. "Default Switch".
            // It seems that columns are separated by at least 3 spaces.
            match line.split("   ").filter(|word| !word.is_empty()).collect_vec().as_slice() {
                [id, name, driver, scope] => ret.push(NetworkInfo {
                    id:     id.to_string(),
                    driver: match *driver {
                        "bridge" => NetworkDriver::Bridge,
                        "host" => NetworkDriver::Host,
                        "overlay" => NetworkDriver::Overlay,
                        "ipvlan" => NetworkDriver::Ipvlan,
                        "macvlan" => NetworkDriver::Macvlan,
                        "none" => NetworkDriver::None,
                        "ics" => NetworkDriver::Ics,
                        "nat" => NetworkDriver::Nat,
                        "transparent" => NetworkDriver::Transparent,
                        "l2bridge" => NetworkDriver::L2bridge,
                        "null" => NetworkDriver::Null,
                        name => NetworkDriver::Other(name.to_string()),
                    },
                    name:   name.to_string(),
                    scope:  scope.to_string(),
                }),
                _ => bail!("Failed to parse line: {}", line),
            }
        }
        Ok(ret)
    }

    pub async fn while_logged_in<F: Future<Output = Result<T>>, T>(
        &self,
        credentials: Credentials,
        f: impl FnOnce() -> F,
    ) -> F::Output {
        self.login(&credentials).await?;
        let ret = f().await;
        // Logout before returning result.
        self.logout(&credentials.server).await?;
        ret
    }

    pub async fn login(&self, credentials: &Credentials) -> Result {
        let Credentials { username, password, server } = credentials;
        let mut cmd = self.cmd()?;
        cmd.args(["login", "--username", username, "--password-stdin", server]);
        cmd.stdin(Stdio::piped());
        let mut process = cmd.spawn()?;
        let stdin = process.stdin.as_mut().context("Failed to open stdin")?;
        stdin.write_all(password.as_bytes()).await?;
        process.wait_ok().await
    }

    pub async fn logout(&self, registry: &str) -> Result {
        let mut cmd = self.cmd()?;
        cmd.args(["logout", registry]);
        cmd.run_ok().await
    }

    pub async fn push(&self, image: &str) -> Result {
        let mut cmd = self.cmd()?;
        cmd.args(["push", image]);
        cmd.run_ok().await
    }
}

#[derive(Clone, Debug)]
pub struct BuildOptions {
    pub context:    PathBuf,
    pub target:     Option<OsString>,
    pub tags:       Vec<String>,
    pub build_args: HashMap<String, Option<String>>,
    pub file:       Option<PathBuf>,
}

impl BuildOptions {
    pub fn new(context_path: impl Into<PathBuf>) -> Self {
        Self {
            context:    context_path.into(),
            target:     default(),
            tags:       default(),
            build_args: default(),
            file:       default(),
        }
    }

    pub fn add_build_arg_from_env_or<R>(
        &mut self,
        name: impl AsRef<str>,
        f: impl FnOnce() -> Result<R>,
    ) -> Result
    where
        R: ToString,
    {
        let value = match std::env::var(name.as_ref()) {
            Ok(env_value) => env_value,
            Err(_) => f()?.to_string(),
        };
        self.build_args.insert(name.as_ref().into(), Some(value));
        Ok(())
    }

    pub fn args(&self) -> Vec<OsString> {
        let mut ret = Vec::new();
        ret.push(self.context.clone().into());
        if let Some(target) = self.target.as_ref() {
            ret.push("--target".into());
            ret.push(target.clone());
        }
        for tag in &self.tags {
            ret.push("--tag".into());
            ret.push(tag.into());
        }
        for (name, value) in &self.build_args {
            ret.push("--build-arg".into());
            if let Some(value) = value {
                ret.push(format!("{name}={value}").into());
            } else {
                ret.push(name.into());
            }
        }
        if let Some(file) = self.file.as_ref() {
            ret.push("--file".into());
            // Docker can't handle verbatim Dockerfile path. It would fail like:
            // `unable to prepare context: unable to get relative Dockerfile path: Rel: can't make
            // \\?\C:\Users\mwu\ci\image\windows\Dockerfile relative to
            // C:\Users\mwu\AppData\Local\Temp\2\.tmpOykTop`
            ret.push(file.without_verbatim_prefix().into());
        }
        ret
    }
}

/// Using the --restart flag on Docker run you can specify a restart policy for how a container
/// should or should not be restarted on exit.
#[derive(Clone, Copy, Debug)]
pub enum RestartPolicy {
    /// Do not automatically restart the container when it exits. This is the default.
    No,
    /// Restart only if the container exits with a non-zero exit status.
    OnFailure {
        /// Optionally, limit the number of restart retries the Docker daemon attempts.
        max_retries: Option<u32>,
    },
    /// Always restart the container regardless of the exit status. When you specify always, the
    /// Docker daemon will try to restart the container indefinitely. The container will also
    /// always start on daemon startup, regardless of the current state of the container.
    Always,
    /// Always restart the container regardless of the exit status, including on daemon startup,
    /// except if the container was put into a stopped state before the Docker daemon was stopped.
    UnlessStopped,
}

impl RestartPolicy {
    pub fn print_args(self) -> [OsString; 2] {
        let value = match self {
            RestartPolicy::No => "no".into(),
            RestartPolicy::OnFailure { max_retries: Some(max_retries) } =>
                format!("on-failure:{}", max_retries).into(),
            RestartPolicy::OnFailure { max_retries: None } => "on-failure:{}".into(),
            RestartPolicy::Always => "always".into(),
            RestartPolicy::UnlessStopped => "unless-stopped".into(),
        };
        ["--restart".into(), value]
    }
}

#[derive(Clone, Debug)]
pub enum Network {
    Bridge,
    Host,
    User(String),
    Container(ContainerId),
}

impl Default for Network {
    fn default() -> Self {
        Network::Bridge
    }
}

impl Display for Network {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Network::Bridge => write!(f, "bridge"),
            Network::Host => write!(f, "host"),
            Network::User(name) => write!(f, "{}", name),
            Network::Container(name_or_id) => write!(f, "container:{}", name_or_id),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RunOptions {
    pub image:             ImageId,
    pub working_directory: Option<PathBuf>,
    pub volume:            Vec<(PathBuf, PathBuf)>,
    pub command:           Vec<OsString>,
    pub name:              Option<String>,
    pub restart:           Option<RestartPolicy>,
    pub env:               HashMap<OsString, OsString>,
    /// Mapping host port => guest port.
    pub ports:             HashMap<u16, u16>,
    pub network:           Option<Network>,
    pub storage_size_gb:   Option<usize>,
    /// Proxy all received signals to the process (non-TTY mode only).
    pub sig_proxy:         Option<bool>,
}

impl RunOptions {
    pub fn new(image: ImageId) -> Self {
        Self {
            image,
            working_directory: default(),
            volume: default(),
            command: default(),
            name: default(),
            restart: default(),
            env: default(),
            ports: default(),
            network: default(),
            storage_size_gb: default(),
            sig_proxy: default(),
        }
    }

    pub fn env_raw(&mut self, name: impl Into<OsString>, value: impl Into<OsString>) -> &mut Self {
        self.env.insert(name.into(), value.into());
        self
    }

    pub fn volume(&mut self, host: impl Into<PathBuf>, guest: impl Into<PathBuf>) -> &mut Self {
        self.volume.push((host.into(), guest.into()));
        self
    }

    pub fn env<T: TypedVariable>(
        &mut self,
        variable: &T,
        value: impl Borrow<T::Borrowed>,
    ) -> Result<&mut Self> {
        Ok(self.env_raw(variable.name(), variable.generate(value.borrow())?))
    }

    pub fn bind_docker_daemon(&mut self) {
        let path = match TARGET_OS {
            OS::Windows => r"\\.\pipe\docker_engine",
            OS::Linux => r"/var/run/docker.sock",
            _ => unimplemented!("OS {} is not supported!", TARGET_OS),
        };
        self.volume.push((PathBuf::from(path), PathBuf::from(path)));
    }

    pub fn storage_size_gb(&mut self, storage_size_in_gb: usize) -> &mut Self {
        self.storage_size_gb = Some(storage_size_in_gb);
        self
    }

    pub fn publish_port(&mut self, host_port: u16, container_port: u16) -> &mut Self {
        self.ports.insert(host_port, container_port);
        self
    }

    pub fn args(&self) -> Vec<OsString> {
        let mut ret = Vec::new();
        if let Some(working_directory) = self.working_directory.as_ref() {
            ret.push("--workdir".into());
            ret.push(working_directory.clone().into());
        }
        for (volume_src, volume_dst) in &self.volume {
            ret.push("--volume".into());

            let mut mapping = volume_src.clone().into_os_string();
            mapping.push(":");
            mapping.push(volume_dst);
            ret.push(mapping);
        }
        if let Some(name) = self.name.as_ref() {
            ret.push("--name".into());
            ret.push(name.into());
        }
        if let Some(restart) = self.restart.as_ref() {
            ret.extend(restart.print_args());
        }

        for (name, value) in &self.env {
            ret.push("--env".into());
            let mut mapping = name.clone();
            mapping.push("=");
            mapping.push(value);
            ret.push(mapping);
        }

        for (host, guest) in &self.ports {
            ret.push("-p".into());
            ret.push(format!("{host}:{guest}").into());
        }

        if let Some(network) = self.network.as_ref() {
            let arg = format!(r#"--network={network}"#);
            ret.push(arg.into());
        }

        if let Some(storage_size_gb) = self.storage_size_gb {
            // e.g. --storage-opt size=120G
            ret.push("--storage-opt".into());
            ret.push(format!("size={}G", storage_size_gb).into());
        }

        if let Some(sig_proxy) = self.sig_proxy {
            let arg = format!(r#"--sig-proxy={sig_proxy}"#);
            ret.push(arg.into());
        }

        ret.push(OsString::from(&self.image.0));

        ret.extend(self.command.clone());
        ret
    }
}

#[derive(Clone, Display, Debug)]
pub struct ImageId(pub String);

#[derive(Clone, Debug, Display, Shrinkwrap)]
pub struct ContainerId(pub String);

impl FromStr for ContainerId {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(ContainerId(s.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore]
    async fn network() -> Result {
        dbg!(Docker.list_networks().await?);
        dbg!(Docker.remove_network("fd").await?);
        dbg!(Docker.create_network(&default(), "fd").await?);
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn build() -> Result {
        let opts = BuildOptions::new(r"C:\Users\mwu\ci\image\windows\");
        dbg!(Docker.build(opts).await?);
        Ok(())
    }
}
