# Contributing to Luna
Thank you for your interest in contributing to Luna! We believe that only 
through community involvement can Luna be the best it can be! There are a whole
host of ways to contribute, and every single one is appreciated. The major 
sections of this document are linked below: 

- [Feature Enhancements](#feature-enhancements)
- [Bug Reports](#bug-reports)
- [Hacking on Luna](#hacking-on-luna)
- [Pull Requests](#pull-requests)
- [Documentation](#documentation)
- [Issue Triage](#issue-triage)
- [Out-of-Tree Contributions](#out-of-tree-contributions)

All contributions to Luna should be in keeping with our 
[Code of Conduct](https://github.com/luna/luna/blob/CODE_OF_CONDUCT.md).

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

Opening an issue is as easy as following 
[this link](https://github.com/luna/luna/issues/new) and filling out the fields.
Below is a template you can use to file the bug, but it doesn't matter if you
don't follow it exactly! Just get the important info in there!

```md
## Summary

## Reproduction
A set of steps and a code sample that produces the issue.

**Observed Result:** What you see happen.
**Expected Result:** What you _think_ should happen.

## Metadata
Include your operating system, Luna version and any other relevant data.
```

All three bits of this are important, especially what you did to cause the 
issue. The more detail you provide, the more easily we can reproduce it and fix
the bug! It's also very helpful to have some information about your system, in
case the bug is Operating System or Architecture specific.

## Hacking on Luna
Luna's build system is nice and simple, allowing you to bootstrap the compiler
as long as you have an installation of 

[The Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and the
Haskell parser generator `happy`. 

You can install the latter just by running `stack install happy`, which should
build the tool for your system and put it in your `stack` binary folder. 

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
stack install
```

Note that the executable for the compiler will be located in 
`$LUNA_REPO_PATH/dist/bin/public/luna` folder. You may wish to add it to your 
`$PATH`.

Additionally, if you intend to simply use the Luna compiler (as opposed to 
tinkering with it, which requires frequent rebuilds), you may consider adding 
`--ghc-options="-O2 -j4"` to the stack install command. This should make the 
Luna compiler run considerably faster, at the cost of longer build times for 
building it.

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
- `luna-project` (found in `$LUNA_REPO_PATH/project`)
- `luna-shell` (found in `$LUNA_REPO_PATH/shell`)
- `luna-stdlib` (found in `$LUNA_REPO_PATH/stdlib`)
- `luna-lexer` (found in `$LUNA_REPO_PATH/syntax/text/lexer`)
- `luna-parser` (found in `$LUNA_REPO_PATH/syntax/text/parser`)

#### Developing with Local Libraries
If you are hacking on the libraries that Luna uses, you'll want to be building
Luna using your local copies of these libraries. These libraries should have
their standard names and be located in the following path:

```
$LUNA_REPO_PATH/../libs/
```

If your libraries are located in this path, you can use the local development 
stack `.yaml` file located in `build/stack-local.yaml` to build and test your
version of Luna as follows:

```
stack build --stack-yaml build/stack-local.yaml
```

### Running Luna
As a prerequisite, you need to set a `LUNA_HOME` variable to point to the 
location of the Luna standard library. Assuming your repo is at 
`$LUNA_REPO_PATH`, you will need to set `LUNA_HOME` to `$LUNA_REPO_PATH/stdlib`.

Next, you need to create the project: create a directory structure like this:

```
your_project/
 ┖─ src/
    ┠─ Main.luna
    ┖─ any other *.luna files
```
A sample `Main.luna` file may look like this:

```python
import Std.Base

def main:
    print "Hello world"
    print (2 + 2)
```

To compile and run the project, simply type `luna` in the main project 
directory.

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
`$LUNA_REPO_PATH/shell` directory. Additionally, please make sure your code is
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
[C - Documentation](https://github.com/luna/luna/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3A%22C+-+Documentation%22+)
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
[D - Beginner](https://github.com/luna/luna/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3A%22D+-+Beginner%22+)
issue label.

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
