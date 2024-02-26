# Desktop app

This folder contains projects related to the desktop app.

## Folder structure

Refer to the `README.md` in each individual module (if it exists) for the
internal folder structure of the module.

- `assets/`: Icons and images used by other modules. Currently these are only
  used by `dashboard/`.
- `client/`: The code for the Electron desktop app.
- `common/`: Utility functions required by multiple other modules.
- `content/`: The entry point for the GUI1 web app. This is the main page for
  the desktop app.
- `content-config/`: The statically-typed configuration object for `content/`.
- [`dashboard/`](./lib/dashboard/README.md): The dashboard, used to manage
  projects. It launches the GUI (located in `content/` for GUI1, or `/app/gui2/`
  for GUI2) when a project is opened.
- `esbuild-plugin-copy-directories/`: An ESBuild plugin for continuously copying
  directories from the a given location to a given subdirectory of the build
  output directory.
- `icons/`: Generates the logo for the app.
- `ts-plugin-namespace-auto-import/`: (WIP) A TypeScript plugin to change
  auto-import to use `import * as moduleName` rather than `import {}`.
- `types/`: Miscellaneous types used by multiple modules.
