# Contributing to Luna
Thank you for your interest in contributing to Luna! We believe that only
through community involvement can Luna be the best it can be! There are a whole
host of ways to contribute, and every single one is appreciated. The major
sections of this document are linked below:

- [Issues](#issues)
- [Feature Enhancements](#feature-enhancements)
- [Bug Reports](#bug-reports)
- [Hacking on Luna](#hacking-on-luna)
- [Pull Requests](#pull-requests)
- [Documentation](#documentation)
- [Issue Triage](#issue-triage)
- [Out-of-Tree Contributions](#out-of-tree-contributions)

All contributions to Luna should be in keeping with our
[Code of Conduct](https://github.com/luna/luna/blob/CODE_OF_CONDUCT.md).

## Issues

If you are looking for somewhere to start, check out the `Help wanted` tags in
repositories:
- [Luna](https://github.com/luna/luna/labels/Status%3A%20Help%20Wanted)
- [Luna Studio](https://github.com/luna/luna-studio/labels/Status%3A%20Help%20Wanted)
- [Luna Manager](https://github.com/luna/luna-manager/labels/Status%3A%20Help%20Wanted)
- [Luna Dataframes](https://github.com/luna/dataframes/labels/Status%3A%20Help%20Wanted)

## Feature Enhancements
If you feel like you have a suggestion for a change to the way that Luna works
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
users of Luna, please contact
[security@luna-lang.org](mailto:security@luna-lang.org).**

Even though GitHub search can be a bit hard to use sometimes, we'd appreciate if
you could
[search](https://github.com/luna/luna/search?q=&type=Issues&utf8=%E2%9C%93) for
your issue before filing a bug as it's possible that someone else has already
reported the issue. We know the search isn't the best, and it can be hard to
know what to search for, so we really don't mind if you do submit a duplicate!

Opening an issue is as easy as following [this link](https://github.com/luna/luna/issues/new?template=bug-report.md)
and filling out the fields. The template is intended to collect all the
information we need to best diagnose the issue, so please take the time to fill
it out accurately.

The reproduction steps are particularly important, as the more easily we can
reproduce it, the faster we can fix the bug! It's also helpful to have the
output of `luna --version`, as that will let us know if the bug is Operating
System or Architecture specific.

## Hacking on Luna
This will get you up and running for Luna development, with only a minimal
amount of setup required. Luna's build system is nice and simple, allowing you
to bootstrap the compiler as long as you have an installation of
[The Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

### Design Documentation
If you're going to start contributing to Luna, it is often a good idea to take a
look at the design documentation for the language. These files explain provide
both a rigorous specification of Luna's design, but also insight into the _why_
behind the decisions that have been made.

These can be found in [`doc/design/`](doc/design/), and are organised by the
part of the compiler that they relate to.

### System Requirements
Luna runs on all reasonably new Linuxes, MacOS, and Windows. Luna was mostly
tested on Ubuntu >= 14.04, Fedora >= 23, MacOS >= 10.11 (El Capitan) and Windows
10, although it should run fine on all Linux distros like Mint, Debian or Arch.
Please report any issues here on GitHub or shoot an email to
[contact@luna-lang.org](mailto:contact@luna-lang.org).

### Getting the Sources
Given you've probably been reading this document on GitHub, you might have an
inkling where to look!. You can clone Luna using two methods:

- **Via HTTPS:** We recommend you only use HTTPS if checking out the sources as
  read-only.

```
git clone https://github.com/luna/luna.git
```

- **Via SSH:** For those who plan on regularly making direct commits, cloning
  over SSH may provide a better user experience (but requires setting up your
  SSH Keys with GitHub).

```
git clone git@github.com:luna/luna.git
```

### Building Luna
To build the command-line compiler interface along with all its sub-components,
you will need to build the `shell` project. The instructions below assume that
your `luna` repo is already cloned and we will refer to its location as
`$LUNA_REPO_PATH`.

```
cd $LUNA_REPO_PATH
stack build
```

The resultant binary will be deep in the `stack` working tree, and to run it,
you will need to use `stack exec luna`. If you have arguements to pass to luna,
you should pass them after a `--` as follows: `stack exec luna -- <args>`.

If, instead, you intend to simply _use_ the Luna compiler (as opposed to
tinkering) with it, which requires frequent rebuilds, you can improve its
performance by using either of the options in the [Building Luna for Release](#building-luna-for-release)
section below.

#### Building Luna Components
It is also possible to build and test each component of Luna (e.g. core, parser)
separately. To do this, pick the component you want from the following list, and
then execute the following command:

```
stack build <component>
stack test <component>
```

Where `<component>` is one of the following:

- `luna-core` (found in `$LUNA_REPO_PATH/core`)
- `lune-passes` (found in `$LUNA_REPO_PATH/passes`)
- `luna-package` (found in `$LUNA_REPO_PATH/package`)
- `luna-runtime` (found in `$LUNA_REPO_PATH/runtime`)
- `luna-shell` (found in `$LUNA_REPO_PATH/shell`)
- `luna-stdlib` (found in `$LUNA_REPO_PATH/stdlib`)
- `luna-syntax-text-lexer` (found in `$LUNA_REPO_PATH/syntax/text/lexer`)
- `luna-syntax-text-parser` (found in `$LUNA_REPO_PATH/syntax/text/parser`)

#### Developing Luna's Libraries
Currently, all of the libraries owned by us that Luna depends on are in-tree,
and can be found in the `lib` folder. This is motivated by how often we were
changing them purely to support Luna's development, and then having to go
through the hackage release song and dance.

Instead, while these libraries are so actively developed. we've decided that
they should live in Luna's tree, where they can be depended on by Luna and
Luna Studio directly. Working with these libraries is a breeze. If you want to
make some changes, it's as simple as making them, and rebuilding to test them.

Similarly to the key components of the compiler listed above, all of these
libraries can be built individually. As there are so many, their build names
can't be listed here, but most of them will match their containing folders. If
one doesn't it will usually be `luna-<foldername>` instead.

#### Building Luna for Release
In order to keep compile times down for development, we compile Luna with
`-fomit-interface-pragmas`. However, this disables cross-module inlining, which
is an important optimisation for ensuring that Luna is performant.

In order to build Luna for maximum performance, you need to override this flag
when giving the build command. This will significantly increase compile times
and can be done as follows:

```
stack build --ghc-options="-fno-omit-interface-pragmas" <...>
```

It is recommended to always use this when building the benchmarks. You can use
this additional argument with any of the commands listed above for development
of individual components.

#### Packaging Luna
While we currently do not distribute packaged binary releases of Luna, this repo
contains a rudimentary packaging script that builds a Luna package that can be
freely relocated on the system that builds it. To build this rudimentary package
you can use the following invocation.

```
stack build_package.hs
```

It has a variety of options that you can discover by passing it the `--help`
command-line flag, but the main options are:

- `--release`: Build the Luna package in release mode. As discussed above, this
  will result in the compilation of Luna taking significantly longer.
- `--verbose`: Full logging of the output of the build process. Useful if you
  are trying to debug a build error, but it is usually recommended to go back to
  a plain `stack build` in those cases.
- `--package-dir DIR`: This lets you specify the directory in which you want the
  Luna package to be built. It defaults to `./dist/`, but if you have a specific
  location on your `PATH`, you can use this option to change the default.

### Running Luna
First, you need to create a project. This is as simple as executing
`luna init <project-path>`, which will create a project in the directory
specified with the correct structure. It will create a defaulted `Main.luna`
file for you as well, allowing you to immediately execute this.

To interpret and run the project, simply type `luna run` in the main project
directory. Alternatively, you can pass the project directory explicitly as
follows:

```
luna run --target <project-path>
```

The above instructions assume that you have the relocatable package described in
[Packaging Luna](#packaging-luna) in your `PATH`. If you don't and would just
like to run these commands from the Luna repository, please ensure that you
instead call `stack exec luna -- <args>`, where `<args>` are the arguments that
you would like to pass to Luna.

#### Executing Luna on Standalone Files
The Luna interpreter is also capable of executing standalone Luna files. This
can be done by passing a luna source file to the `--target` flag, similarly to
the above. This source file must contain a `main` function to be used as the
entry point.

#### Failed Execution
If you get an error when executing Luna that states `Repository head not found`,
this means that you are attempting to run Luna in an unsupported way. Either you
need to call `luna`, where the `luna` on your `PATH` is part of a relocatable
package (as described in [Packging Luna](#packaging-luna)), or using it via a
call to `stack exec luna`.

If you are using either of these methods and are still seeing the error, please
[report a bug](https://github.com/luna/luna/issues/new?template=bug-report.md).

#### Overriding the Standard Library Location
By default, a development build of Luna will look for the standard library in
`$LUNA_REPO_PATH/stdlib/`, but it is possible to override this. If you would
like to do so, you need to define the `LUNA_STDLIB_OVERRIDE` environment
variable and set its value to the absolute path to the Luna standard library
that you want to use.

As long as the variable is set, the compiler will use the location you provide,
so don't forget to unset it if you would like to go back to using the included
copy.

This environment variable override will also work for distributed binaries of
Luna, and so provides a useful way to work on the standard library without
needing to download and build Luna in its entirety.

## Pull Requests
Pull Requests are the primary method for making changes to Luna. GitHub has
[fantastic documentation](https://help.github.com/articles/about-pull-requests/)
on using the pull request feature. Luna uses the 'fork-and-pull' model of
development. It is as described
[here](https://help.github.com/articles/about-collaborative-development-models/)
and involves people pushing changes to their own fork and creating pull requests
to bring those changes into the main Luna repository.

Please make all pull requests against the `master` branch.

Before making your PR, please make sure that the commit passes the Luna test
suite. You can run all the tests by executing `stack test` in the
`$LUNA_REPO_PATH` directory. Additionally, please make sure your code is
in compliance with the
[Luna Style Guidelines](https://github.com/luna/luna/wiki/Haskell-code-style).

Make sure you perform these checks before _every_ pull request. You can even add
[git hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) before
every push to make sure that you can't forget.

Every pull request for Luna is reviewed by another person! You'll get a
reviewer from the core team assigned at random, but feel free to ask for a
specific person if you've dealt with them in a certain area before!

Once the reviewer approves your pull request it will be tested by our continuous
integration provider before being merged!

## Documentation
Documentation improvements are very welcome! The source for the Luna Book can be
found in [`luna/luna-book`](https://github.com/luna/luna-book), but most of the
API documentation is generated directly from the code!

Documentation pull requests are reviewed in exactly the same way as normal pull
requests.

To find documentation-related issues, sort by the
[Category: Documentation](hhttps://github.com/luna/luna/labels/Category%3A%20Documentation)
label.

## Issue Triage
Sometimes issues can be left open long after the bug has been fixed. Other
times, a bug might go stale because something has changed in the meantime.

It can be helpful to go through older bug reports and make sure that they are
still valid. Load up an older issue, double check that it's still true, and
leave a comment letting us know if it is or is not. The
[least recently updated](https://github.com/luna/luna/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-asc)
sort is good for finding issues like this.

Contributors with sufficient permissions can help by adding labels to help with
issue triage.

If you're looking for somewhere to start, take a look at the
[Difficulty: Beginner](https://github.com/luna/luna/labels/Difficulty%3A%20Beginner)
issue label, as well as the
[Status: Help Wanted](https://github.com/luna/luna/labels/Status%3A%20Help%20Wanted)
label.

## Out-of-Tree Contributions
As helpful as contributing to Luna directly is, it can also be just as helpful
to contribute in other ways outside this repository:

- Answer questions in the [Discord](https://discordapp.com/invite/YFEZz3y) or
  on [StackOverflow](https://stackoverflow.com/questions/tagged/luna).
- Participate in the [RFC Process](https://github.com/luna/luna-rfcs).

## Helpful Documentation and Links
For people new to Luna, and just starting to contribute, or even for more
seasoned developers, some useful places to look for information are:

- [The Luna Book](https://luna-lang.gitbooks.io/docs/)
- The community! Don't be afraid to ask questions.

