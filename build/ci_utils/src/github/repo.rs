use crate::prelude::*;

/// Data denoting a specific GitHub repository.
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize, derive_more::Display)]
#[display(fmt = "{}/{}", owner, name)]
pub struct Repo {
    /// Owner - an organization's or user's name.
    pub owner: String,
    pub name:  String,
}

impl IsRepo for Repo {
    fn owner(&self) -> &str {
        &self.owner
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Parse from strings in format "owner/name". Opposite of `Display`.
impl std::str::FromStr for Repo {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        RepoRef::from_str(s).map(|repo| repo.into())
    }
}

impl<'a> From<RepoRef<'a>> for Repo {
    fn from(repo: RepoRef<'a>) -> Self {
        Repo { owner: repo.owner.to_owned(), name: repo.name.to_owned() }
    }
}

impl Repo {
    pub fn new(owner: impl Into<String>, name: impl Into<String>) -> Self {
        Self { owner: owner.into(), name: name.into() }
    }
}


/// Non-owning reference to a GitHub repository.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize, derive_more::Display)]
#[display(fmt = "{}/{}", owner, name)]
pub struct RepoRef<'a> {
    /// Owner - an organization's or user's name.
    pub owner: &'a str,
    pub name:  &'a str,
}

impl<'a> IsRepo for RepoRef<'a> {
    fn owner(&self) -> &str {
        self.owner
    }

    fn name(&self) -> &str {
        self.name
    }
}

impl<'a> RepoRef<'a> {
    pub const fn new<T1, T2>(owner: &'a T1, name: &'a T2) -> Self
    where
        T1: ~const AsRef<str> + ?Sized,
        T2: ~const AsRef<str> + ?Sized, {
        Self { owner: owner.as_ref(), name: name.as_ref() }
    }

    pub fn from_str(s: &'a (impl AsRef<str> + ?Sized)) -> Result<Self> {
        let s = s.as_ref();
        match s.split('/').collect_vec().as_slice() {
            [owner, name] => Ok(Self { owner, name }),
            slice => bail!("Failed to parse string '{}': Splitting by '/' should yield exactly 2 pieces, found: {}", s, slice.len()),
        }
    }
}
