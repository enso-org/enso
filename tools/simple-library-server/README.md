# Simple Enso Library Server

A simple server for hosting custom Enso library repositories.

## Usage

You need Node.JS to use this version of the server.

To install the dependencies, run `npm install` in the root directory of the
application. Then you can run the `main.js` file to start the server. See
`./main.js --help` for available commandline options.

## Repository structure

When launching the server, you need to provide it with a directory that is the
root of the server. This directory should contain a `libraries` directory or
`editions` directory (or both). Each of them should have the directory structure
as described in [the Enso documentation](../../docs/libraries/repositories.md).

For example, the root directory may look like this:

```
root
├── libraries
│   ├── Foo
│   │   └── Bar
│   │       ├── 1.2.3
│   │       │   ├── meta
│   │       │   │   ├── preview.png
│   │       │   │   └── icon.png
│   │       │   ├── main.tgz
│   │       │   ├── tests.tgz
│   │       │   ├── LICENSE.md
│   │       │   ├── package.yaml
│   │       │   └── manifest.yaml
│   │       └── 1.2.4-SNAPSHOT.2021-06-24
│   │           └── ... # Truncated for clarity
│   └── Standard
│       ├── Base
│       │   └── 1.0.0
│       │       └── ...
│       └── Table
│           └── 1.0.0
│               └── ...
└── editions
    ├── manifest.yaml
    ├── 2021.1.yaml
    ├── foo.yaml
    └── bar.yaml
```

Then to add this repository as an edition provider you can append
`http://hostname:port/editions` to the `edition-providers` field in
`global-config.yaml`.

To use libraries from this repository, the editions should define the repository
with URL `http://hostname:port/libraries`.
