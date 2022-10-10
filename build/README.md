<p align="center">
  <br/>
  <a href="http://enso.org">
      <img
          src="https://user-images.githubusercontent.com/1623053/79905826-22bac080-8417-11ea-82b0-ee015904a485.png"
          alt="Enso Language"
          width="136"
      />
  </a>
</p>

# [WIP] Enso CI Build Scripts

The code under this directory is under ongoing intensive development. As such it
has not been reviewed or cleaned up yet.

## General principles

- Written in Rust.
- Portable. Works on any platform that Enso targets.
- Do not duplicate functionality that is already available in tools being part
  of our tech stack.
- Supports both developers and CI use-cases (and environments). Developers can
  call it locally to do anything that CI does.
- Does not require much setup work. Where feasible sets things up for the user.

### Concepts

#### Target

- Can be built locally from sources. Building yields artifacts.
- Artifacts are self-contained to a single filesystem directory.
- Artifacts can be downloaded from a remote location rather than built with the
  same effect.
- Can contain other targets.
- Artifacts can be platform-specific.
- Artifacts must be releasable as CI run artifacts or release assets.

# Usage

While the script is invoked using `cargo run`, the convenience `run` script is
provided in the repository root.

The general usage form is:

```
./run <command> [options]
```

The command itself is usually a combination of target name and subcommand, like
`ide build` or `backend test`.

At every layer, the `--help` command can be used to get more information. Note
that the information depends on the command, so running `./run --help` will not
give you the same information as `./run ide --help` nor
`./run ide build --help`.

## Targets

### IDE

IDE is the top level target for our project. It consists of `GUI` and `Backend`
targets.

Sources consist mostly of TS code for the Electron client and packaging.

Artifacts are platform specific and consist of the single image file.

### GUI

GUI is the front-end part of the project. It consists of `WASM` target.

Sources consist mostly of TS code for the web page that embeds the `WASM`
binaries.

Artifacts are portable across the platforms and consist of the web page that can
be served either using Electron client (as IDE does) or using a web server (like
the Cloud version of Enso).

### WASM

This is the core of GUI, written in Rust. It has no external dependencies.

Artifacts are portable across the platforms and consist of the single WASM
binary accompanied by the JS snippets and glue.

### Backend

Backend is the back-end part of the project, as seen from the IDE perspective.
It contains a Project Manager bundle that includes:

- Project Manager native image;
- Enso Engine distribution (with the Standard Library);
- GraalVM distribution.

These components are not represented as "Targets" (in terms of build script) but
could be and likely will be in the future.
