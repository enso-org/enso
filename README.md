# Luna programming language
### Visual and textual functional programming language with a focus on productivity, collaboration and development ergonomics.

Luna is a developer’s whiteboard on steroids. Design, prototype, develop and refactor any application simply by connecting visual elements together. Collaborate with co-workers, interactively fine tune parameters, inspect the results and visually profile the performance in real-time.

Visit www.luna-lang.org to learn more!

This repository contains the Luna compiler core and its command line version. For the full (visual) Luna Studio, please take a look at the https://github.com/luna/luna-studio repository. For installation and management tools, there is https://github.com/luna/luna-manager.

## Getting Started

This will get you up and running for Luna development, with only a minimal amount of setup required.

### System Requirements

Luna runs on all reasonably new Linuxes, Mac OSes and Windows. Luna was mostly tested on Ubuntu >= 14.04, Fedora >= 23, MacOS >= 10.11 (ElCapitan) and Windows 10, although it should run fine on all Linux distros like Mint, Debian or Arch. Some users were experiencing some issues on Gentoo, but that proved to be largely installation-dependent and hard to reproduce. Please report any issues on GitHub or shoot an email to contact@luna-lang.org.

The only two dependencies necessary to build Luna are [The Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and the program `happy`. To install stack, simply follow the instructions at https://docs.haskellstack.org/en/stable/README/. Remember that in order to run stack-installed executables, you need to add `${HOME}/.local/bin` to your `${PATH}`. To install happy, run:
```
stack install happy
```
(in your home directory, not inside any of the stack-managed projects you may have on your machine).

### Getting Sources for Luna compiler and its ecosystem tools

**Via HTTPS**  For those checking out sources as read-only, HTTPS works best:
```
git clone https://github.com/luna/luna.git
```

**Via SSH**  For those who plan on regularly making direct commits,
cloning over SSH may provide a better experience (which requires
uploading SSH keys to GitHub):
```
git clone git@github.com:luna/luna.git
```

### Building Luna

To build the command-line compiler interface along with all its sub-components, you will need to build the `shell` project. The instructions below assume that your `luna` repo is already cloned and we will refer to its location as `${LUNA_REPO_PATH}`.
```
cd ${LUNA_REPO_PATH/shell
stack install
```
Note that the executable for the compiler will be located in `${LUNA_REPO_PATH}/dist/bin/public/luna` folder. You may wish to add it to your `${PATH}`.

Additionally, if you intend to simply use the compiler (as opposed to tinkering on it, which requires frequent rebuilds), you may consider adding `--ghc-options="-O2 -j4"` to the `stack install` command. This should make the compiler run considerably faster, at the cost of longer build times.
