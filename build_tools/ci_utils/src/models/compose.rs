//! Simple and incomplete model for docker-compose.yml file contents.
//!
//! See: <https://docs.docker.com/compose/compose-file/compose-file-v3/>

use crate::prelude::*;



/// A build section of the service.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Build {
    pub context: PathBuf,
    pub target:  String,
    pub args:    Vec<String>,
}

/// A service entry.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Service {
    pub build:   Build,
    pub command: String,
    pub volumes: Vec<String>,
}

/// A top-level volume entry.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct Volume {
    pub external: bool,
}

/// File root.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Compose {
    pub services: BTreeMap<String, Service>,
    pub volumes:  BTreeMap<String, Volume>,
}
