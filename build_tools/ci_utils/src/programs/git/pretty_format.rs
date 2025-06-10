//! Customized formatting utilities.
//!
//! See:
//! <https://git-scm.com/docs/pretty-formats#Documentation/pretty-formats.txt>

use crate::prelude::*;



/// Placeholders that can be used with `format:<...>` in git commands.
/// See: <https://git-scm.com/docs/pretty-formats#Documentation/pretty-formats.txt>
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Placeholder {
    // === Placeholders that expand to a single literal ===
    /// A newline character.
    Newline,
    /// A raw percent sign.
    Percent,
    /// Hex-encoded byte character.
    HexCode(u8),

    // === Placeholders that control formatting ===
    // TODO: Add, as needed.

    // === Placeholders for commit information ===
    /// The commit hash.
    Hash,
    /// Abbreviated commit hash.
    AbbreviatedHash,
    /// Tree hash.
    TreeHash,
    /// Abbreviated tree hash.
    AbbreviatedTreeHash,
    /// Parent hashes.
    ParentHashes,
    /// Abbreviated parent hashes.
    AbbreviatedParentHashes,
    /// Author name.
    AuthorName,
    /// Author name respecting `.mailmap`.
    AuthorNameMailmap,
    /// Author email.
    AuthorEmail,
    /// Author email respecting `.mailmap`.
    AuthorEmailMailmap,
    /// Author email local part (the part before the @).
    AuthorEmailLocal,
    /// Author email local part respecting `.mailmap`.
    AuthorEmailLocalMailmap,
    /// Author date, respecting the --date=<format> option.
    AuthorDate,
    /// Author date in [RFC 2822 format](https://www.rfc-editor.org/rfc/rfc2822).
    AuthorDateRfc2822,
    /// Author date, relative to current time (e.g. "2 weeks ago").
    AuthorDateRelative,
    /// Author date in UNIX timestamp format.
    AuthorDateUnixTimestamp,
    /// Author date, ISO 8601-like format. The differences to the strict ISO 8601 format are:
    /// * a space instead of the T date/time delimiter;
    /// * a space between time and time zone;
    /// * no colon between hours and minutes of the time zone.
    AuthorDateIso8601Like,
    /// Author date, strict ISO 8601 format.
    AuthorDateIso8601Strict,
    /// Author date, short format (YYYY-MM-DD).
    AuthorDateShort,
    /// Author date, human-friendly format.
    ///
    /// This shows the timezone if the timezone does not match the current time-zone, and doesn't
    /// print the whole date if that matches (ie skip printing year for dates that are "this year",
    /// but also skip the whole date itself if it’s in the last few days and we can just say what
    /// weekday it was). For older dates the hour and minute is also omitted.
    AuthorDateHuman,
    /// Committer name.
    CommitterName,
    /// Committer name respecting `.mailmap`.
    CommitterNameMailmap,
    /// Committer email.
    CommitterEmail,
    /// Committer email respecting `.mailmap`.
    CommitterEmailMailmap,
    /// Committer email local part (the part before the @).
    CommitterEmailLocal,
    /// Committer email local part respecting `.mailmap`.
    CommitterEmailLocalMailmap,
    /// Committer date, respecting the `--date=<format>` option.
    CommitterDate,
    /// Committer date in [RFC 2822 format](https://www.rfc-editor.org/rfc/rfc2822).
    CommitterDateRfc2822,
    /// Committer date, relative to current time (e.g. "2 weeks ago").
    CommitterDateRelative,
    /// Committer date in UNIX timestamp format.
    CommitterDateUnixTimestamp,
    /// Committer date, ISO 8601-like format. The differences to the strict ISO 8601 format are:
    /// * a space instead of the T date/time delimiter;
    /// * a space between time and time zone;
    /// * no colon between hours and minutes of the time zone.
    CommitterDateIso8601Like,
    /// Committer date, strict ISO 8601 format.
    CommitterDateIso8601Strict,
    /// Committer date, short format (YYYY-MM-DD).
    CommitterDateShort,
    /// Committer date, human-friendly format.
    ///
    /// This shows the timezone if the timezone does not match the current time-zone, and doesn’t
    /// print the whole date if that matches (ie skip printing year for dates that are "this year",
    /// but also skip the whole date itself if it’s in the last few days and we can just say what
    /// weekday it was). For older dates the hour and minute is also omitted.
    CommitterDateHuman,
    /// Ref names, like the --decorate option of [git-log](https://git-scm.com/docs/git-log).
    RefNamesParenthesized,
    /// Ref names without the parentheses wrapping.
    RefNames,
    /// Ref name that was used to find the commit. Works only with `git-log`.
    RefNameUsed,
    /// Encoding name.
    Encoding,
    /// Subject. This is the commit message without the body.
    Subject,
    /// Sanitized subject line, suitable for a filename.
    SubjectSanitized,
    /// Body. This is the commit message without the subject line.
    Body,
    /// Raw body.
    RawBody,
    /// Commit notes.
    Notes,
    /// Raw verification message from GPG for a signed commit.
    GpgVerificationMessage,
    /// GPG verification status for a signed commit.
    ///
    /// Show "G" for a good (valid) signature, "B" for a bad signature, "U" for a good signature
    /// with unknown validity, "X" for a good signature that has expired, "Y" for a good signature
    /// made by an expired key, "R" for a good signature made by a revoked key, "E" if the
    /// signature cannot be checked (e.g. missing key) and "N" for no signature.
    GpgVerificationStatus,
    /// The name of the signer of a signed commit.
    GpgSignerName,
    /// The key used to sign a signed commit.
    GpgSignerKey,
    /// The fingerprint of the key used to sign a signed commit.
    GpgSignerFingerprint,
    /// The fingerprint of the primary key whose subkey was used to sign a signed commit.
    GpgSignerPrimaryFingerprint,
    /// Trust level of the key used to sign a signed commit.
    GpgSignerTrustLevel,
    /// Reflog selector.
    ReflogSelector,
    /// Shortened reflog selector.
    ReflogSelectorShort,
    /// Reflog identity name.
    ReflogIdentity,
    /// Reflog identity mail respecting `.mailmap`.
    ReflogIdentityMailmap,
    /// Reflog identity email.
    ReflogIdentityEmail,
    /// Reflog identity email respecting `.mailmap`.
    ReflogIdentityEmailMailmap,
    /// Reflog subject.
    ReflogSubject,
    // TODO: trailers of the body
    // https://git-scm.com/docs/pretty-formats#Documentation/pretty-formats.txt-emtrailersoptionsem
}

impl Display for Placeholder {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Placeholder::Newline => write!(f, "%n"),
            Placeholder::Percent => write!(f, "%%"),
            Placeholder::HexCode(code) => write!(f, "%{code:02x}"),
            Placeholder::Hash => write!(f, "%H"),
            Placeholder::AbbreviatedHash => write!(f, "%h"),
            Placeholder::TreeHash => write!(f, "%T"),
            Placeholder::AbbreviatedTreeHash => write!(f, "%t"),
            Placeholder::ParentHashes => write!(f, "%P"),
            Placeholder::AbbreviatedParentHashes => write!(f, "%p"),
            Placeholder::AuthorName => write!(f, "%an"),
            Placeholder::AuthorNameMailmap => write!(f, "%aN"),
            Placeholder::AuthorEmail => write!(f, "%ae"),
            Placeholder::AuthorEmailMailmap => write!(f, "%aE"),
            Placeholder::AuthorEmailLocal => write!(f, "%al"),
            Placeholder::AuthorEmailLocalMailmap => write!(f, "%aL"),
            Placeholder::AuthorDate => write!(f, "%ad"),
            Placeholder::AuthorDateRfc2822 => write!(f, "%aD"),
            Placeholder::AuthorDateRelative => write!(f, "%ar"),
            Placeholder::AuthorDateUnixTimestamp => write!(f, "%at"),
            Placeholder::AuthorDateIso8601Like => write!(f, "%ai"),
            Placeholder::AuthorDateIso8601Strict => write!(f, "%aI"),
            Placeholder::AuthorDateShort => write!(f, "%as"),
            Placeholder::AuthorDateHuman => write!(f, "%ah"),
            Placeholder::CommitterName => write!(f, "%cn"),
            Placeholder::CommitterNameMailmap => write!(f, "%cN"),
            Placeholder::CommitterEmail => write!(f, "%ce"),
            Placeholder::CommitterEmailMailmap => write!(f, "%cE"),
            Placeholder::CommitterEmailLocal => write!(f, "%cl"),
            Placeholder::CommitterEmailLocalMailmap => write!(f, "%cL"),
            Placeholder::CommitterDate => write!(f, "%cd"),
            Placeholder::CommitterDateRfc2822 => write!(f, "%cD"),
            Placeholder::CommitterDateRelative => write!(f, "%cr"),
            Placeholder::CommitterDateUnixTimestamp => write!(f, "%ct"),
            Placeholder::CommitterDateIso8601Like => write!(f, "%ci"),
            Placeholder::CommitterDateIso8601Strict => write!(f, "%cI"),
            Placeholder::CommitterDateShort => write!(f, "%cs"),
            Placeholder::CommitterDateHuman => write!(f, "%ch"),
            Placeholder::RefNamesParenthesized => write!(f, "%d"),
            Placeholder::RefNames => write!(f, "%D"),
            Placeholder::RefNameUsed => write!(f, "%S"),
            Placeholder::Encoding => write!(f, "%e"),
            Placeholder::Subject => write!(f, "%s"),
            Placeholder::SubjectSanitized => write!(f, "%f"),
            Placeholder::Body => write!(f, "%b"),
            Placeholder::RawBody => write!(f, "%B"),
            Placeholder::Notes => write!(f, "%N"),
            Placeholder::GpgVerificationMessage => write!(f, "%GG"),
            Placeholder::GpgVerificationStatus => write!(f, "%G?"),
            Placeholder::GpgSignerName => write!(f, "%GS"),
            Placeholder::GpgSignerKey => write!(f, "%GK"),
            Placeholder::GpgSignerFingerprint => write!(f, "%GF"),
            Placeholder::GpgSignerPrimaryFingerprint => write!(f, "%GP"),
            Placeholder::GpgSignerTrustLevel => write!(f, "%GT"),
            Placeholder::ReflogSelector => write!(f, "%gD"),
            Placeholder::ReflogSelectorShort => write!(f, "%gd"),
            Placeholder::ReflogIdentity => write!(f, "%gn"),
            Placeholder::ReflogIdentityMailmap => write!(f, "%gN"),
            Placeholder::ReflogIdentityEmail => write!(f, "%ge"),
            Placeholder::ReflogIdentityEmailMailmap => write!(f, "%gE"),
            Placeholder::ReflogSubject => write!(f, "%gs"),
        }
    }
}


/// Get reference names from the decoration printed from `git log` with `--decorate=full` and
/// `%D` format ([Placeholder::RefNames]).
///
/// Such names can be further parsed, e.g. into [`Ref`].
///
/// ```
/// use ide_ci::programs::git::pretty_format::refs_from_decoration;
/// let text = "HEAD -> refs/heads/wip/mwu/stable-release, tag: refs/tags/2022.1.1-nightly.2022-11-09, refs/heads/develop";
/// let refs = refs_from_decoration(text);
/// assert_eq!(refs, vec![
///     "HEAD",
///     "refs/heads/wip/mwu/stable-release",
///     "refs/tags/2022.1.1-nightly.2022-11-09",
///     "refs/heads/develop"
/// ]);
/// ```
pub fn refs_from_decoration(text: &str) -> Vec<&str> {
    text.split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .flat_map(|s| {
            if let Some(stripped) = s.strip_prefix("HEAD -> ") {
                vec!["HEAD", stripped]
            } else {
                vec![s]
            }
            .into_iter()
        })
        .map(|s| s.trim_start_matches("tag: "))
        .collect_vec()
}
