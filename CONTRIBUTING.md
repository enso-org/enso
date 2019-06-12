# Contributing to Enso
Thank you for your interest in contributing to Enso! We believe that only
through community involvement can Enso be the best it can be! There are a whole
host of ways to contribute, and every single one is appreciated. The major
sections of this document are linked below:

<!-- MarkdownTOC levels="2" autolink="true" -->

- [Issues](#issues)
- [Feature Enhancements](#feature-enhancements)
- [Bug Reports](#bug-reports)
- [Hacking on Enso](#hacking-on-enso)
- [Pull Requests](#pull-requests)
- [Documentation](#documentation)
- [Issue Triage](#issue-triage)
- [Out-of-Tree Contributions](#out-of-tree-contributions)
- [Helpful Documentation and Links](#helpful-documentation-and-links)

<!-- /MarkdownTOC -->

All contributions to Luna should be in keeping with our
[Code of Conduct](https://github.com/luna/luna/blob/CODE_OF_CONDUCT.md).

## Issues
If you are looking for somewhere to start, check out the `Help Wanted` tag in
the following repositories:
- [Enso](https://github.com/luna/luna/labels/Status%3A%20Help%20Wanted)
- [Luna Studio](https://github.com/luna/luna-studio/labels/Status%3A%20Help%20Wanted)
- [Luna Manager](https://github.com/luna/luna-manager/labels/Status%3A%20Help%20Wanted)
- [Luna Dataframes](https://github.com/luna/dataframes/labels/Status%3A%20Help%20Wanted)

## Feature Enhancements
If you feel like you have a suggestion for a change to the way that Enso works
as a language, please open an issue in our
[RFCs Repository](https://github.com/luna/luna-rfcs), rather than in this one!
New features and other significant language changes must go through the RFC
process so they can be properly discussed.

## Bug Reports
While it's never great to find a bug, they are a reality of software and
software development! We can't fix or improve on the things that we don't know
about, so report as many bugs as you can! If you're not sure whether something
is a bug, file it anyway!

**If you are concerned that your bug publicly presents a security risk to the
users of Enso, please contact
[security@luna-lang.org](mailto:security@luna-lang.org).**

Even though GitHub search can be a bit hard to use sometimes, we'd appreciate if
you could
[search](https://github.com/luna/enso/search?q=&type=Issues&utf8=%E2%9C%93) for
your issue before filing a bug as it's possible that someone else has already
reported the issue. We know the search isn't the best, and it can be hard to
know what to search for, so we really don't mind if you do submit a duplicate!

Opening an issue is as easy as following [this link](https://github.com/luna/enso/issues/new?template=bug-report.md)
and filling out the fields. The template is intended to collect all the
information we need to best diagnose the issue, so please take the time to fill
it out accurately.

The reproduction steps are particularly important, as the more easily we can
reproduce it, the faster we can fix the bug! It's also helpful to have the
output of `enso --version`, as that will let us know if the bug is Operating
System or Architecture specific.

## Hacking on Enso
This will get you up and running for Enso development, with only a minimal
amount of setup required. Enso's build system is fairly simple, allowing you to
bootstrap the compiler as long as you have...

### Design Documentation
If you're going to start contributing to Enso, it is often a good idea to take a
look at the design documentation for the language. These files explain provide
both a rigorous specification of Enso's design, but also insight into the _why_
behind the decisions that have been made.

These can be found in [`doc/design/`](doc/design/), and are organised by the
part of the compiler that they relate to.

### System Requirements
TBC

### Getting the Sources
Given you've probably been reading this document on GitHub, you might have an
inkling where to look!. You can clone Luna using two methods:

- **Via HTTPS:** We recommend you only use HTTPS if checking out the sources as
  read-only.

```
git clone https://github.com/luna/enso.git
```

- **Via SSH:** For those who plan on regularly making direct commits, cloning
  over SSH may provide a better user experience (but requires setting up your
  SSH Keys with GitHub).

```
git clone git@github.com:luna/enso.git
```

### Building Enso
TBC

#### Building Enso Components
TBC

#### Developing Enso's Libraries
TBC

#### Building Enso for Release
TBC

#### Packaging Enso
TBC

### Running Luna
TBC

#### Projects

#### Standalone Files

#### Language Server Mode

## Pull Requests
Pull Requests are the primary method for making changes to Enso. GitHub has
[fantastic documentation](https://help.github.com/articles/about-pull-requests/)
on using the pull request feature. Luna uses the 'fork-and-pull' model of
development. It is as described
[here](https://help.github.com/articles/about-collaborative-development-models/)
and involves people pushing changes to their own fork and creating pull requests
to bring those changes into the main Luna repository.

Please make all pull requests against the `master` branch.

Before making your PR, please make sure that the commit passes the Enso test
suite. You can run all the tests by ... TBC. In addition, please ensure that
your code conforms to the Enso [Scala Style Guide](./doc/scala-style-guide.md)
and [Haskell Style Guide](./doc/haskell-style-guide.md) as relevant.

Make sure you perform these checks before _every_ pull request. You can even add
[git hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) before
every push to make sure that you can't forget.

Every pull request for Enso is reviewed by another person! You'll get a
reviewer from the core team assigned at random, but feel free to ask for a
specific person if you've dealt with them in a certain area before!

Once the reviewer approves your pull request it will be tested by our continuous
integration provider before being merged!

## Documentation
Documentation improvements are very welcome! The source for the Enso, Book can be
found in [`luna/luna-book`](https://github.com/luna/luna-book), but most of the
API documentation is generated directly from the code!

Documentation pull requests are reviewed in exactly the same way as normal pull
requests.

To find documentation-related issues, sort by the
[Category: Documentation](hhttps://github.com/luna/enso/labels/Category%3A%20Documentation)
label.

## Issue Triage
Sometimes issues can be left open long after the bug has been fixed. Other
times, a bug might go stale because something has changed in the meantime.

It can be helpful to go through older bug reports and make sure that they are
still valid. Load up an older issue, double check that it's still true, and
leave a comment letting us know if it is or is not. The
[least recently updated](https://github.com/luna/enso/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-asc)
sort is good for finding issues like this.

Contributors with sufficient permissions can help by adding labels to help with
issue triage.

If you're looking for somewhere to start, take a look at the
[Difficulty: Beginner](https://github.com/luna/enso/labels/Difficulty%3A%20Beginner)
issue label, as well as the
[Status: Help Wanted](https://github.com/luna/enso/labels/Status%3A%20Help%20Wanted)
label.

## Out-of-Tree Contributions
As helpful as contributing to Enso directly is, it can also be just as helpful
to contribute in other ways outside this repository:

- Answer questions in the [Discord](https://discordapp.com/invite/YFEZz3y) or
  on [StackOverflow](https://stackoverflow.com/questions/tagged/luna).
- Participate in the [RFC Process](https://github.com/luna/luna-rfcs).

## Helpful Documentation and Links
For people new to Luna, and just starting to contribute, or even for more
seasoned developers, some useful places to look for information are:

- [The Enso Book](https://luna-lang.gitbooks.io/docs/)
- The community! Don't be afraid to ask questions.
