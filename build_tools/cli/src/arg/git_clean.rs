use crate::prelude::*;



#[derive(Clone, Copy, Debug, Default, clap::Args)]
pub struct Options {
    /// Do not perform the action, just print what would be deleted.
    #[clap(long)]
    pub dry_run:      bool,
    /// Clean also the build script's cache (located in the user's local application data subtree).
    #[clap(long)]
    pub cache:        bool,
    /// Clean also the build script's build artifacts.
    #[clap(long)]
    pub build_script: bool,
}
