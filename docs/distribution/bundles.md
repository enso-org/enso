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
- [`ensoup` Bundles](#ensoup-bundles)

<!-- /MarkdownTOC -->

## Backend Assets

The IDE bundles backend assets required to run the language server: engine
distribution (in `dist/`), Graal runtime (in `runtime/`), and licenses
(`THIRD-PARTY`).

These components are placed in their respective subdirectories (not as packages,
but extracted and ready to use).

In fact, it is possible for the assets directory to be read-only (which may be
the case for example if the Project Manager bundle is packaged as part of IDE's
AppImage package). In such situation, it will be impossible to uninstall the
bundled components and a relevant error message will be returned.

## `ensoup` Bundles

Bundles are also distributed for the `ensoup` updater, but these are implemented
using a different mechanism.

Since the `ensoup` can run in
[portable mode](distribution.md#portable-enso-distribution-layout), the bundled
engine and runtime are simply included within its portable package. They can
then be used from within this portable package or
[installed](distribution.md#installing-from-a-portable-distribution).
