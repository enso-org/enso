//! Code for representing git references.
//!
//! See [Git Reference Documentation](https://git-scm.com/book/en/v2/Git-Internals-Git-References).

use crate::prelude::*;



/// A git reference.
///
/// It is a name that points to an object in the repository or another reference.
///
/// # Examples
/// ```
/// use enso_build_base::prelude::FromString;
/// use ide_ci::programs::git::Ref;
/// let reference = Ref::from_str("refs/heads/master").unwrap();
/// assert_eq!(reference, Ref::Branch { name: "master".into() });
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ref {
    /// The current branch.
    ///
    /// HEAD is a reference to one of the heads in your repository, except when using a detached
    /// HEAD, in which case it directly references an arbitrary commit.
    Head,
    /// Head of a local branch.
    Branch {
        /// Branch name, stripped of the `refs/heads/` prefix.
        name: String,
    },
    /// Head of a remote branch.
    RemoteBranch {
        /// Name of the remote.
        remote: String,
        /// Branch name, stripped of the `refs/remotes/<remote>/` prefix.
        branch: String,
    },
    /// A ref under refs/tags/ namespace that points to an object of an arbitrary type (typically a
    /// tag points to either a tag or a commit object).
    Tag {
        /// Name of the tag. Any trailing `^{}` suffix is stripped.
        name:   String,
        /// Whether the tag was peeled, i.e. had `^{}` suffix.
        peeled: bool,
    },
    /// All other refs, e.g. `refs/notes/commits` or `MERGE_HEAD`.
    Other {
        /// Full reference name.
        name: String,
    },
}

impl std::str::FromStr for Ref {
    type Err = anyhow::Error;

    /// Parse the reference from the full decorated name, like `refs/heads/main`.
    /// See: <https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefdecorateadecorate>
    ///
    /// # Examples
    /// ```
    /// use enso_build_base::prelude::*;
    /// use ide_ci::programs::git::r#ref::Ref;
    /// fn main() -> Result {
    ///     assert_eq!(Ref::from_str("HEAD")?, Ref::Head);
    ///     assert_eq!(Ref::from_str("refs/heads/main")?, Ref::Branch { name: "main".into() });
    ///     assert_eq!(Ref::from_str("refs/remotes/origin/main")?, Ref::RemoteBranch {
    ///         remote: "origin".into(),
    ///         branch: "main".into(),
    ///     });
    ///     assert_eq!(Ref::from_str("refs/tags/2022.1.1-nightly.2022-11-09")?, Ref::Tag {
    ///         name:   "2022.1.1-nightly.2022-11-09".into(),
    ///         peeled: false,
    ///     });
    ///     assert_eq!(Ref::from_str("refs/tags/2022.1.1-nightly.2022-11-09^{}")?, Ref::Tag {
    ///         name:   "2022.1.1-nightly.2022-11-09".into(),
    ///         peeled: true,
    ///     });
    ///     Ok(())
    /// }
    /// ```
    fn from_str(name: &str) -> Result<Self> {
        let name = name.trim();
        Ok(if name == "HEAD" {
            Ref::Head
        } else if let Some(name) = name.strip_prefix("refs/heads/") {
            Ref::Branch { name: name.to_string() }
        } else if let Some(name) = name.strip_prefix("refs/remotes/") {
            let mut parts = name.splitn(2, '/');
            let remote = parts.next().context("Missing remote name.")?.to_string();
            let branch = parts.next().context("Missing branch name.")?.to_string();
            Ref::RemoteBranch { remote, branch }
        } else if let Some(name) = name.strip_prefix("refs/tags/") {
            if let Some(peeled_name) = name.strip_suffix("^{}") {
                Ref::Tag { name: peeled_name.to_string(), peeled: true }
            } else {
                Ref::Tag { name: name.to_string(), peeled: false }
            }
        } else {
            bail!("Unknown ref name: `{}`.", name);
        })
    }
}

impl Display for Ref {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Ref::Head => write!(f, "HEAD"),
            Ref::Branch { name } => write!(f, "refs/heads/{name}"),
            Ref::RemoteBranch { remote, branch } => write!(f, "refs/remotes/{remote}/{branch}"),
            Ref::Tag { name, peeled } =>
                write!(f, "refs/tags/{name}{}", if *peeled { "^{}" } else { "" }),
            Ref::Other { name } => write!(f, "{name}"),
        }
    }
}
