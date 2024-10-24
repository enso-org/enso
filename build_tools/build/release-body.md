{{!-- This is a template of the markdown text that is used as a release body.

Please see [this](src/release.rs) for the code that uses this template and
available placeholders.

The template itself is written in [Handlebars](https://handlebarsjs.com/).

--}}

# Download

## Enso IDE

Enso IDE is the main product of the Enso project. The packages are stand-alone, they contain both GUI and the backend.

Download links:
{{#each assets.ide}}
- [{{target_pretty}}]({{url}})
{{/each}}

This is the recommended download for most users.

## Enso Engine

If you are interested in using Enso Engine command line tools only, download the Enso Engine bundle.

Download links:
{{#each assets.engine}}
- [{{target_pretty}}]({{url}})
{{/each}}

These are archives containing the [Enso portable distribution](https://enso.org/docs/developer/enso/distribution/distribution.html#portable-enso-distribution-layout). User is responsible for setting up the environment variables and adding the `bin` directory to the `PATH`.

Note that these distributions do not allow you to use the Enso IDE.

It is recommended only for advanced users, who want to just try the compiler CLI.

# Anonymous Data Collection

Please note that this release collects anonymous usage data which will be used to improve Enso and prepare it for a stable release. We will switch to opt-in data collection in stable version releases. The usage data will not contain your code (expressions above nodes), however, reported errors may contain brief snippets of out of context code that specifically leads to the error, like "the method 'foo' does not exist on Number". The following data will be collected:

- Session length.
- Graph editing events (node creation, deletion, position change, connect, disconnect, collapse, edit start, edit end). This will not include any information about node expressions used.
- Navigation events (camera movement, scope change).
- Visualization events (visualization open, close, switch). This will not include any information about the displayed data nor the rendered visualization itself.
- Project management events (project open, close, rename).
- Errors (IDE crashes, WASM panics, Project Manager errors, Language Server errors, Compiler errors).
- Performance statistics (minimum, maximum, average GUI refresh rate).

# Changelog

{{changelog}}
