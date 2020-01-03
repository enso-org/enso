[![License](https://img.shields.io/static/v1?label=License&message=MIT&color=2ec352&labelColor=2c3239)](https://github.com/luna/basegl/blob/master/LICENSE) 
[![Actions Status](https://github.com/luna/basegl/workflows/Build%20%28MacOS%2C%20Linux%2C%20Windows%29/badge.svg)](https://github.com/luna/basegl/actions)
[![Coverage](https://img.shields.io/codecov/c/github/luna/basegl?label=Coverage&labelColor=2c3239)](https://codecov.io/gh/luna/basegl/branch/master) 
![Stability](https://img.shields.io/static/v1?label=Stability&message=Unstable&color=d52229&labelColor=2c3239)

# BaseGL

BaseGL is a blazing fast 2D vector rendering engine with a rich set of
primitives and a GUI component library. It is able to display millions of shapes
60 frames per second in a web browser on a modern laptop hardware. 

This repository is a work in progress of BaseGL 2.0. Please refer to BaseGL 1.0
repository for more information: https://github.com/luna/basegl-old.


## Development

### The Rust toolchain 
This project uses several features available only in the nightly Rust toolchain.
To setup the toolchain, please use the [the Rust toolchain
installer](https://rustup.rs/):

```bash
rustup toolchain install nightly-2019-11-04 # Install the nightly channel.
rustup default nightly                      # Set it as the default one.
rustup component add clippy                 # Install the linter.
```

### Building the sources
Please use the `script/build.sh` script to build the project or the
`script/watch.sh` script to run a file watch utility which will build the
project when on every source change. The scripts are thin wrappers over
[wasm-pack](https://github.com/rustwasm/wasm-pack) and accept the same [command
line arguments](https://rustwasm.github.io/wasm-pack/book/commands/build.html).
In particular, you can provide them with `--release`, `--dev`, or `--profile`
flags to switch the compilation profile. If not option is provided, the scripts
default to the `--release` profile.

### Running examples
Please note that in order to run the examples you have to first build the
project. For best experience, it is recommended to use the `scripts/watch.sh`
in a second shell. In order to build the demo scenes, follow the steps below:

```bash
cd examples
npm install
npm run start 
```

You can now navigate to http://localhost:8080 and play with the demo scenes!

Please note that `npm run start` runs the Webpack Dev-Server in the production
mode. You can use the `npm run start-dev` in order to enable the development
mode, however, as all sources are provided to Webpack in form of WASM binaries,
we haven't observed any differences between them in this project. 

While Webpack provides handy utilities for development, like live-reloading on
sources change, it also adds some runtime overhead. In order to run the compiled
examples using a lightweight http-server (without live-reloading functionality),
please use the `npm run prod-server` command.

**Please remember to disable the cache in your browser during development!**

### Running tests
The sources use both unit tests and web test, which are run in a browser and
produce visual results. To run them, use the `scripts/test.sh` script and follow
the output in the terminal.


### Working with the source code

#### Formatting
Please note that this codebase does not use `rustfmt`. Please read the following
documents to learn more about reasons behind this decision and the recommended
code style guide. Be sure to carefully read the documents before contributing to
this repository:
- [Rust style guide 1](https://github.com/luna/basegl/blob/master/docs/style-guide.md)
- [Rust style
  guide 2](https://github.com/luna/enso/blob/master/doc/rust-style-guide.md) 


#### Linting 
Please be sure to fix all errors reported by `scripts/lint.sh` before creating a
pull request to this repository.
