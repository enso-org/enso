<p align="center">
<img src="https://github.com/luna/luna-studio/raw/master/resources/logo.ico" style="margin: 0 auto;">
</p>
<h1 align="center">Luna programming language</h1>
<h3 align="center">
Visual and textual functional programming language with a focus on productivity, collaboration and development ergonomics.
</h3>

Luna is a developer’s whiteboard on steroids. Design, prototype, develop and refactor any application simply by connecting visual elements together. Collaborate with co-workers, interactively fine tune parameters, inspect the results and visually profile the performance in real-time.

Visit www.luna-lang.org to learn more!

This repository contains the Luna compiler core and its command line version. For the full (visual) Luna Studio, please take a look at the https://github.com/luna/luna-studio repository. For installation and management tools, there is https://github.com/luna/luna-manager.

## Getting Started

This will get you up and running for Luna development, with only a minimal amount of setup required.

### System Requirements

Luna runs on all reasonably new Linuxes, MacOS, and Windows. Luna was mostly tested on Ubuntu >= 14.04, Fedora >= 23, MacOS >= 10.11 (El Capitan) and Windows 10, although it should run fine on all Linux distros like Mint, Debian or Arch. Please report any issues on GitHub or shoot an email to contact@luna-lang.org.

The only two dependencies necessary to build Luna are [The Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and the program `happy`. To install stack, simply follow the instructions at https://docs.haskellstack.org/en/stable/README/. Remember that in order to run stack-installed executables, you need to add `$HOME/.local/bin` to your `$PATH`. To install happy, run in your home directory:
```
$ cd $HOME
$ stack install happy
```

### Getting Sources for Luna compiler and its ecosystem tools

**Via HTTPS**  For those checking out sources as read-only, HTTPS works best:
```
$ git clone https://github.com/luna/luna.git
```

**Via SSH**  For those who plan on regularly making direct commits,
cloning over SSH may provide a better experience (which requires
uploading SSH keys to GitHub):
```
$ git clone git@github.com:luna/luna.git
```

### Building Luna

To build the command-line compiler interface along with all its sub-components, you will need to build the `shell` project. The instructions below assume that your `luna` repo is already cloned and we will refer to its location as `$LUNA_REPO_PATH`.
```
$ cd $LUNA_REPO_PATH/shell
$ stack install
```
Note that the executable for the compiler will be located in `$LUNA_REPO_PATH/dist/bin/public/luna` folder. You may wish to add it to your `$PATH`.

Additionally, if you intend to simply use the compiler (as opposed to tinkering with it, which requires frequent rebuilds), you may consider adding `--ghc-options="-O2 -j4"` to the `stack install` command. This should make the compiler run considerably faster, at the cost of longer build times.

### Running Luna

As a prerequisite, you need to set a `LUNA_HOME` variable to point to the location of the Luna standard library. Assuming your repo is at `$LUNA_REPO_PATH`, you will need to set `LUNA_HOME` to `$LUNA_REPO_PATH/stdlib`.

Next, you need to create the project: create a directory structure like this:
```
- your_project/
   - src/
      - Main.luna
      - any other *.luna files
```
A sample `Main.luna` file may look like this:
```python
import Std.Base

def main:
    print "Hello world"
    print (2 + 2)
```

Tu compile and run the project, simply type `luna` in the main project directory.
