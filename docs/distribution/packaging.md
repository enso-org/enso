---
layout: developer-doc
title: Enso Libraries Packaging
category: distribution
tags: [packaging, distribution, layout]
order: 2
---

# Enso Libraries Packaging
Given the nature of Enso as an open-source programming language and platform,
it is crucial that we provide users with an extensible package management
system. This document describes the current state of our packaging efforts, as
well as future directions and enhancements to it.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Enso Package Structure](#enso-package-structure)
  - [The `src` Directory](#the-src-directory)
  - [The `polyglot` Directory](#the-polyglot-directory)
  - [The `package.yaml` File](#the-packageyaml-file)
- [Build Reproducibility](#build-reproducibility)

<!-- /MarkdownTOC -->

## Enso Package Structure
The general directory structure of an Enso package is as follows:

```
My_Package
├── package.yaml
├── polyglot
│   ├── java
│   │   └── jar.jar
│   └── js
│       └── library.js
└── src
    ├── Main.enso
    └── Sub_Module
        ├── Helper.enso
        └── Util.enso
```

### The `src` Directory
The `src` directory contains all Enso sources, organized in a hierarchical
structure. The structure of this directory dictates how particular modules
are imported in all of Enso code.

Note that all files and directories in this subtree must be named according
to the Enso style for referent names (i.e. `Upper_Snake_Case`, see
[the syntax specification](../syntax/naming.md#naming-constructs)).

A file located at the path `My_Package/src/Sub_Module/Helper.enso` will be
imported like so:

```ruby
import My_Package.Sub_Module.Helper
```

Please note the following:

- The name of the package appears as the first segment of the name.
- The package name is not specified by the containing directory's name,
  but rather it is described in the `package.yaml` file.

The exact transformation is as follows:

1. The name of the package becomes the first segment of the qualified module
   name.
2. Any subdirectories on the path from the `src` directory to the source file
   are appended as consecutive segments.
3. The name of the source file, with the `.enso` extension stripped, becomes
   the last segment.

### The `polyglot` Directory
The `polyglot` directory contains per-language subdirectories containing files
used by the supported polyglot languages. The contents of each subdirectory is
specified on a per-language basis, in the
[polyglot documentation](../polyglot/README.md).

### The `package.yaml` File
`package.yaml` describes certain package metadata, such as its name, authors
and version. It also includes the list of dependencies of the package.
The following is an example of this manifest file.

```yaml
license: MIT
name: My_Package
version: 1.0.1
author: "John Doe <john.doe@example.com>"
maintainer: "Jane Doe <jane.doe@example.com>"
enso_version: 1.2.0
dependencies:
  - name: Base
    version: "1.2.0"
  - name: Http
    version: "4.5.3"
```

The following is the specification of the manifest fields.

#### license
**Optional** *String*: The short license name of this package. Defaults to
`None`, meaning the package is not safe for use by third parties.

#### version
**Required** *String*: The [semantic versioning](https://semver.org/) string,
in the `major.minor.patch` format.

#### author
**Optional** *String* or *List of Strings*: The name(s) and contact info(s) of
the author(s) of this library, in the `Name <contact>` or `Name` format.

#### maintainer
**Optional** *String* or *List of Strings*: The name(s) and contact info(s)
of the current maintainer(s) of this library, in the `Name <contact>` or `Name`
format.

#### enso_version
**Required** *String*: The version of the Enso compiler this library is created
for.

> The actionables for this section are:
>
> - Extend the compiler version to handle version bounds.

#### dependencies
**Optional** *List of Library objects*: The list of all libraries this package
requires to function properly. Defaults to an empty list.
A library object is of the form:

```yaml
name: <name of the library>
version: <semver string of the required library version>
```

> The actionables for this section are:
>
> - Extend the library version field to handle version bounds.

## Build Reproducibility
It is crucial for any good development environment to provide reproducible
builds, such that it is impossible for it to go wrong by mismatching library
versions.

> The actionables for this section are:
> 
> - Decide on the strategies of ensuring consistent library resolution. This
>   may include hashing the downloaded versions of libraries and publishing
>   stack-style resolvers for sets of libraries that are proven to work well
>   together.
