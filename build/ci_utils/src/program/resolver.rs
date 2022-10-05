use crate::prelude::*;



#[derive(Debug)]
pub struct Resolver<P> {
    pub cwd:          PathBuf,
    pub names:        Vec<OsString>,
    pub lookup_dirs:  OsString,
    pub phantom_data: PhantomData<P>,
}

impl<P> Resolver<P> {
    pub fn new(names: Vec<&str>, fallback_dirs: Vec<PathBuf>) -> Result<Self> {
        let path = std::env::var_os("PATH").unwrap_or_default();
        let env_path_dirs = std::env::split_paths(&path);
        let lookup_dirs = std::env::join_paths(env_path_dirs.chain(fallback_dirs))?;
        let names = names.into_iter().map(OsString::from).collect();
        let cwd = std::env::current_dir()?;
        let phantom_data = default();
        Ok(Resolver { cwd, names, lookup_dirs, phantom_data })
    }
    pub fn lookup_all(self) -> impl Iterator<Item = PathBuf> {
        let Self { names, lookup_dirs, cwd, phantom_data: _phantom_data } = self;
        names
            .into_iter()
            .filter_map(move |name| {
                // We discard this error, as "error finding program" is like "no program available".
                which::which_in_all(name, Some(lookup_dirs.clone()), cwd.clone()).ok()
            })
            .flatten()
    }

    pub fn lookup(self) -> Result<PathBuf> {
        let empty = Cow::from("<MISSING NAME>");
        let names = self.names.iter().map(|name| name.to_string_lossy()).collect_vec();
        let name = names.first().unwrap_or(&empty).to_string();
        let names = names.join(", ");
        let locations = self.lookup_dirs.clone();
        self.lookup_all().next().ok_or_else(|| {
            anyhow!("Failed to find a program `{}`. Recognized executable names: {}. Tested locations: {}", name, names, locations.to_string_lossy())
        })
    }
}
