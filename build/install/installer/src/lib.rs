pub mod win;

pub use enso_install::prelude;

use enso_install::prelude::*;

#[derive(Debug)]
pub enum InstallerUpdate {
    Progress(f32),
    Stage(String),
    Finished(Result),
}
