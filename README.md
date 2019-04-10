[![Build Status](https://dev.azure.com/luna-lang/luna/_apis/build/status/luna.luna?branchName=master)](https://dev.azure.com/luna-lang/luna/_build/latest?definitionId=1&branchName=master)

<p align="center">
<img src="https://github.com/luna/luna-studio/raw/master/resources/logo.ico" style="margin: 0 auto;">
</p>
<h1 align="center">Luna Programming Language</h1>
<h3 align="center">
Visual and textual functional programming language with a focus on productivity, collaboration and development ergonomics.
</h3>

Luna is a developer’s whiteboard on steroids. Design, prototype, develop and
refactor any application simply by connecting visual elements together.
Collaborate with co-workers, interactively fine tune parameters, inspect the
results and visually profile the performance in real-time.

Visit [The Luna Website](http://www.luna-lang.org) to learn more!

This repository contains the Luna compiler core and its command line version.
For the full (visual) Luna Studio, please take a look at the
[Luna Studio](https://github.com/luna/luna-studio) repository. For installation
and management tools, there is [Luna Manager](https://github.com/luna/luna-manager).

## Getting Started
This will get you up and running for Luna development, with only a minimal
amount of setup required. Luna's build system is nice and simple, allowing you
to bootstrap the compiler as long as you have an installation of
[The Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

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

## Contributing to Luna
If you are interested in contributing to the development of Luna, please read
the
[`CONTRIBUTING.md`](https://github.com/luna/luna/blob/master/CONTRIBUTING.md)
file.

## License
This repository is licensed under the
[Apache 2.0](https://opensource.org/licenses/apache-2.0), as specified in the
[LICENSE](https://github.com/luna/luna/blob/master/LICENSE) file.

Please be aware that, as the commercial backing for Luna,
**New Byte Order Sp. z o. o.** reserves the right under the CLA to use
contributions made to this repository as part of commercially available Luna
products.

If these terms are unacceptable to you, please do not contribute to the
repository.

### The Contributor License Agreement
As part of your first contribution to this repository, you need to accept the
Contributor License Agreement. You will automatically be asked to sign the CLA
when you make your first pull request.

Any work intentionally submitted for inclusion in Luna shall be licensed under
this CLA.

The CLA you sign applies to all repositories associated with the Luna project
([Luna](https://github.com/luna/luna-rfcs),
[Luna Studio](https://github.com/luna/luna-studio), etc), so you will only have
to sign it once at the start of your contributions.

