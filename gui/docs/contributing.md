# Development & Contributing Guide
Thank you for your interest in contributing to the Enso IDE! We believe that only through community
involvement can Enso be the best it can be! There are a whole host of ways to contribute, and every
single one is appreciated.

<br/>

## Reporting Issues

**If you are concerned that your bug publicly presents a security risk to the users of Enso, please
contact [security@enso.org](mailto:security@enso.org).**

While it's never great to find a bug, they are a reality of software and software development! We
can't fix or improve on the things that we don't know about, so report as many bugs as you can! If
you're not sure whether something is a bug, file it anyway!

Even though GitHub search can be a bit hard to use sometimes, we'd appreciate if you could
[search](https://github.com/luna/enso/search?q=&type=Issues&utf8=%E2%9C%93) for your issue before
filing a bug as it's possible that someone else has already reported the issue. We know the search
isn't the best, and it can be hard to know what to search for, so we really don't mind if you _do_
submit a duplicate!

Opening an issue is as easy as following [this link](https://github.com/luna/ide/issues/new?template=bug-report.md)
and filling out the fields. The template is intended to collect all the information we need to best
diagnose the issue, so please take the time to fill it out accurately.

The reproduction steps are particularly important, as the more easily we can reproduce it, the
faster we can fix the bug! It's also helpful to have the version of the IDE, as that will let us
know if the bug is Operating System or Architecture specific.

<br/>

## Development Environment
The project builds on MacOS, Windows, and Linux. Cross-platform targets work well on all of these
platforms, however, MacOS package will miss the right application icon if built on Linux or Windows
due to non-trival icon generation on these platforms. In order to develop the source code you will
need the following setup:

- **The Rust Toolchain (nightly-2019-11-04)**
  This project uses several features available only in the nightly Rust toolchain.  Please use the
  [the Rust toolchain installer](https://rustup.rs) to install it:

  ```bash
  rustup toolchain install nightly-2019-11-04 # Install the nightly channel.
  rustup override set nightly-2019-11-04      # Set it as the default toolchain for this folder.
  rustup component add clippy                 # Install the linter.
  cargo install wasm-pack --version 0.8.1     # Install the wasm-pack toolkit.
  cargo install cargo-watch                   # To enable ./run watch utility
  ```

- **Node and Node Package Manager LTS**
  In order to build the web and desktop applications you will need
  [the latest LTS version of node and npm](https://nodejs.org/en/download). Even minor release
  changes are known to cause serious issues, thus **we provide support for the latest LTS version
  only. Please do not report build issues if you use other versions.** In case you run run MacOS or
  Linux the easiest way to set up the proper version is by installing the
  [Node Version Manager](https://github.com/nvm-sh/nvm) and running
  `nvm install --lts && nvm use --lts`.

<br/>

## Working with sources
Please note that you should not use a code auto-formatter in this codebase. Please read the
following documents to learn more about reasons behind this decision and the recommended code style
guide. Be sure to carefully read the
[Rust style guide 1](https://github.com/luna/ide/blob/master/docs/style-guide.md)
and the [Rust style guide 2](https://github.com/luna/enso/blob/master/doc/rust-style-guide.md)
before contributing to the codebase.


### Development
As this is a multi-part project with many complex dependencies, it is equipped with a build script
which both validates your working environment as well as takes care of providing the most suitable
compilation flags for a particular development stage. In order to run the build script simply run
`node ./run` in the root of the codebase. On MacOS and Linux you can use a simpler form of `./run`,
however, this doc will use the former form in order to stay cross-platform compatible. Run
`node ./run help` to learn about available commands and options. All arguments provided after the
`--` symbol will be passed to sub-commands. For example `node ./run build -- --dev` will pass the
`--dev` flag to `cargo` (Rust build tool). The most common options are presented below:

- **Interactive mode**
  Run `node ./run watch` to start a local web-server and a source-file watch utility which will
  build the project on every change. Open `http://localhost:8080` (the port may vary and will be
  reported in the terminal if `8080` was already in use) to run the application, or
  `http://localhost:8080/debug` to open example demo scenes. Please remember to disable the cache in
  your browser during the development! By default, the script disables heavyweight optimizations to
  provide interactive development experience. The scripts are thin wrappers for
  [wasm-pack](https://github.com/rustwasm/wasm-pack) and accept the same
  [command line arguments](https://rustwasm.github.io/wasm-pack/book/commands/build.html).

- **Production mode**
  In order to compile in a production mode (enable all optimizations, strip WASM debug symbols,
  minimize the output binaries, etc.), run `node ./run build`. To create platform-specific packages
  and installers use `node ./run dist` instead. The final packages will be located at
  `app/dist/native`.

### Testing, Linting, and Validation
After changing the code it's always a good idea to lint and test the code. We have prepared several
scripts which maximally automate the process:

- **Size Validation**
  Use `node ./run check-size` to check if the size of the final binary did not grew too much in
  comparison to the previous release. Watching the resulting binary size is one of the most
  important responsibility of each contributor in order to keep the project small and suitable for
  web-based usage.

- **Testing**
  Use `node ./run test` run both unit and web-based visual test.

- **Linting**
  Please be sure to fix all errors reported by `node ./run line` before creating a pull request to
  this repository.

