---
layout: developer-doc
title: Distribution Bundles
category: distribution
tags: [distribution, layout, bundles]
order: 9
---

# Bundles

This document describes how the distributions are bundled to provide releases
that work out-of-the box, allowing to use the latest engine without downloading
any additional dependencies.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Project Manager Bundle](#project-manager-bundle)
- [Launcher Bundles](#launcher-bundles)

<!-- /MarkdownTOC -->

## Project Manager Bundle

The Project Manager is distributed with latest engine version and its
corresponding Graal runtime to avoid having to download them at first startup.

The bundled components are placed in their respective subdirectories (not as
packages, but extracted and ready to use) and a bundle marker file called
`.enso.bundle` must be placed next to these directories so that the Project
Manager can detect the bundle.

The `project-manager` executable looks for the `.enso.bundle` marker in the
parent directory of the directory that it is, itself, located in. So overall,
the bundle should have the following structure (the actual engine and Graal
versions may of course differ):

```
enso
├── bin
│   └── project-manager
├── dist
│   └── 0.2.1-SNAPSHOT
├── other-project-manager-files
└── runtime
    └── graalvm-ce-java11-20.2.0
```

If the bundle is detected, the additional `dist` and `runtime` directories are
added as secondary search paths for components. Thus, the `project-manager` can
use both components present in the default
[installed location](distribution.md#installed-enso-distribution-layout) or
those from the bundle. In a situation that the same component were to be
available both in the installed location and the bundle, the installed location
is preferred. New components are installed in the installed location, never next
to the bundles.

In fact, it is possible for the bundle directory to be read-only (which may be
the case for example if the Project Manager bundle is packaged as part of IDE's
AppImage package). In such situation, it will be impossible to uninstall the
bundled components and a relevant error message will be returned.

## Launcher Bundles

Bundles are also distributed for the launcher, but these are implemented using a
different mechanism.

Since the launcher can run in
[portable mode](distribution.md#portable-enso-distribution-layout), the bundled
engine and runtime are simply included within its portable package. They can
then be used from within this portable package or
[installed](distribution.md#installing-from-a-portable-distribution).
