---
layout: developer-doc
title: Editions
category: libraries
tags: [libraries, editions]
order: 1
---

# Editions

This document describes the concept of Editions.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [What Is An Edition](#what-is-an-edition)
- [The Edition File](#the-edition-file)

<!-- /MarkdownTOC -->

## What Is An Edition

An Edition is in principle a list of library versions that should be compatible
with each other. An Edition specifies the engine version and a set of library
versions that can be used together.

If a library included in an Edition depends on another library, that other
library must also be included in that Edition and the version at which it is
included must be compatible with the library that depends on it. Each Edition
may only include a single version of each library.

Thus, when a library is to be installed, its version is uniquely determined by
the selected Edition. A curated Edition file will guarantee that all libraries
are compatible which simplifies version resolution - the exact version that is
specified in the Edition is always used.

## The Edition File

The Edition file is a YAML file that can contain the following fields:

- `engine-version` which should be a semantic versioning string specifying the
  engine version that should be associated with that edition,
- `repositories` which can contain a list of repositories which are sources of
  library packages, its format is described below,
- `extends` which can contain a name of another Edition that this Edition
  extends,
- `packages` which can contain a list of packages that this Edition should
  include, its format is described below.

Every field is optional, but an Edition file to be valid must specify at least
the engine version to be used (either by specifying it directly or extending
another edition that specifies it).

### Repositories

TODO

### Packages

TODO

### Extending Editions

TODO
