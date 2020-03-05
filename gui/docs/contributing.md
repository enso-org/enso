# Contributing Guide

## Development Environment
The project builds on MacOS, Windows, and Linux. Cross-platform targets work well on all of these 
platforms, however, MacOS package will miss the right application icon if built on Linux or Window
due to non-trival icon generation on these platforms. In order to develop the source code you will 
need the following setup:

- **The Rust Toolchain (nightly-2019-11-04)**  
  This project uses several features available only in the nightly Rust toolchain.
  Please use the [the Rust toolchain installer](https://rustup.rs) to install it:

  ```bash
  rustup toolchain install nightly-2019-11-04 # Install the nightly channel.
  rustup default nightly                      # Set it as the default one.
  rustup component add clippy                 # Install the linter.
  ```

- **Node and Node Package Manager LTS**  
  In order to build the web and desktop applications you will need 
  [the latest LTS version of node and npm](https://nodejs.org/en/download). Even minor release 
  changes are known to cause serious issues, thus **we provide support for the latest LTS version only.
  Please do not report build issues if you use other versions.** In case you run run MacOS or Linux 
  the easiest way to setup the proper version is by installing the 
  [Node Version Manager](https://github.com/nvm-sh/nvm) and running `nvm use --lts`.

<br/>
<br/>
<br/>

## Working with sources
Please note that you should not use a code auto-formatter in this codebase. Please read the following
documents to learn more about reasons behind this decision and the recommended code style guide. 
Be sure to carefully read the [Rust style guide 1](https://github.com/luna/basegl/blob/master/docs/style-guide.md)
and the [Rust style guide 2](https://github.com/luna/enso/blob/master/doc/rust-style-guide.md) before
contributing to the codebase.

<br/>

### Development
As this is a multi-part project with many complex dependencies, it was equipped with a build script
which both validates your working environment as well as takes care of providing most suitable 
compilation flags for a particular development stage. In order to run the build script simply run 
`node ./run` in the root of the codebase. On MacOS and Linux you can use a simpler form of `./run`, 
however, this doc will use the former form in order to stay cross-platform compatible. Run 
`node ./run help` to learn about available commands and options. All arguments provided after the 
`--` symbol will be passed to sub-commands. For example `node ./run build -- --dev` will pass the 
`--dev` flag to `cargo` (Rust build tool). The most common options are presented below:

- **Interactive mode**  
  Run `node ./run watch` to start a local web-server and a source-file watch utility which will build 
  the project on every change. Open `http://localhost:8080` (the port may vary and will be reported in
  the terminal if `8080` was already in use) to run the application, or `http://localhost:8080/debug`
  to open example demo scenes. Please remember to disable the cache in your browser during the 
  development! By default, the script disables heavyweight optimizations to provide interactive 
  development experience. The scripts are thin wrappers for 
  [wasm-pack](https://github.com/rustwasm/wasm-pack) and accept the same 
  [command line arguments](https://rustwasm.github.io/wasm-pack/book/commands/build.html).

- **Production mode**  
  In order to compile in a production mode (enable all optimizations, strip WASM debug symbols, 
  minimize the output binaries, etc.), run `node ./run build`. To create platform-specific packages and
  installers use `node ./run dist` instead. The final packages will be located at `app/dist/native`.

<br/>

### Testing, Linting, and Validation
After changing the code it's always a good idea to lint and test the code. We have prepared several 
scripts which maximally automate the process:

- **Size Validation**
  Use `node ./run check-size` to check if the size of the final binary did not grew too much in comparison
  to the previous release. Watching the resulting binary size is one of the most important 
  responsibility of each contributor in order to keep the project small and suitable for web-based
  usage.
  
- **Testing**
  Use `node ./run test` run both unit and web-based visual test.
  
- **Linting**  
  Please be sure to fix all errors reported by `node ./run line` before creating a pull request to 
  this repository.
  
