---
layout: developer-doc
title: Enso Libraries Packaging
category: distribution
tags: [packaging, distribution, layout]
order: 2
---

# Enso Libraries Packaging

Given the nature of Enso as an open-source programming language and platform, it
is crucial that we provide users with an extensible package management system.
This document describes the current state of our packaging efforts, as well as
future directions and enhancements to it.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Enso Package Structure](#enso-package-structure)
  - [The `src` Directory](#the-src-directory)
  - [The `polyglot` Directory](#the-polyglot-directory)
  - [The `package.yaml` File](#the-packageyaml-file)
  - [The `visualization` Directory](#the-visualization-directory)
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
├── src
│   ├── Main.enso
│   └── Sub_Module
│       ├── Helper.enso
│       └── Util.enso
└── visualization (optional)
    └── ...
```

### The `src` Directory

The `src` directory contains all Enso sources, organized in a hierarchical
structure. The structure of this directory dictates how particular modules are
imported in all of Enso code.

Note that all files and directories in this subtree must be named according to
the Enso style for referent names (i.e. `Upper_Snake_Case`, see
[the syntax specification](../syntax/naming.md#naming-constructs)).

A file located at the path `My_Package/src/Sub_Module/Helper.enso` will be
imported like so:

```ruby
import My_Package.Sub_Module.Helper
```

Please note the following:

- The name of the package appears as the first segment of the name.
- The package name is not specified by the containing directory's name, but
  rather it is described in the `package.yaml` file.

The exact transformation is as follows:

1. The name of the package becomes the first segment of the qualified module
   name.
2. Any subdirectories on the path from the `src` directory to the source file
   are appended as consecutive segments.
3. The name of the source file, with the `.enso` extension stripped, becomes the
   last segment.

### The `polyglot` Directory

The `polyglot` directory contains per-language subdirectories containing files
used by the supported polyglot languages. The contents of each subdirectory is
specified on a per-language basis, in the
[polyglot documentation](../polyglot/README.md).

### The `package.yaml` File

`package.yaml` describes certain package metadata, such as its name, authors and
version. It also includes the list of extra dependencies of the package
(dependencies that are not present in the resolver or need a version override).
The following is an example of this manifest file.

```yaml
license: MIT
name: My_Package
version: 1.0.1
enso-version: 0.1.0
authors:
  - name: John Doe
    email: john.doe@example.com
maintainers:
  - name: Jane Doe
    email: jane.doe@example.com
resolver: lts-1.2.0
extra-dependencies:
  - name: Base
    version: "1.2.0"
  - name: Http
    version: "4.5.3"
```

The following is the specification of the manifest fields. Fields marked as
**Optional (required for publishing)** are completely optional during
development - if not specified, their default values will be used. However, they
must be specified before publishing the package. A package missing any of these
fields cannot be published.

#### license

**Optional (required for publishing)** _String_: The short license name of this
package. Defaults to `None`, meaning the package is not safe for use by third
parties.

#### enso-version

**Optional (required for publishing)** _String_: Specifies the Enso version that
should be used for this project. If not set or set to `default`, the default
locally installed Enso version will be used. The version should not be `default`
if the package is to be published.

#### version

**Optional (required for publishing)** _String_: The
[semantic versioning](https://semver.org/) string, in the `major.minor.patch`
format. If not set, it defaults to `dev` (which can be used for development, but
is not a valid version for publishing).

#### authors

**Optional** _List of contacts_: The name(s) and contact info(s) of the
author(s) of this library.

A contact is of the form:

```yaml
name: Contact Name
email: email@example.com
```

Both `name` and `email` fields are optional, but at least one of them has to be
present.

#### maintainers

**Optional** _List of contacts_: The name(s) and contact info(s) of the current
maintainer(s) of this library, in the same format as `authors` above.

#### resolver

**Note** This field is not currently implemented. **Optional (required for
publishing)** _String_: The resolver name, used to choose compiler version and
basic libraries set. If not set, the system-default resolver will be used.

> The actionables for this section are:
>
> - Extend the compiler version to handle version bounds.

#### extra-dependencies

**Note** This field is not currently implemented. **Optional** _List of Library
objects_: The list of libraries this package requires to function properly and
that are not included in the resolver. Defaults to an empty list.

A library object is of the form:

```yaml
name: <name of the library>
version: <semver string of the required library version>
```

> The actionables for this section are:
>
> - Extend the library version field to handle version bounds.

### The `visualization` Directory

As Enso is a visual language, a package may contain a specification of how data
can be displayed in various tools, for example
[Enso IDE](https://github.com/enso-org/ide). The Enso package structure may
optionally contain a `visualization` directory which may contain visualization
definitions.

For more information on how visualization definitions should work with the Enso
IDE, see
[this example](https://dev.enso.org/docs/ide/product/visualizations.html#custom-visualization-example).

## Build Reproducibility

It is crucial for any good development environment to provide reproducible
builds, such that it is impossible for it to go wrong by mismatching library
versions.

> The actionables for this section are:
>
> - Decide on the strategies of ensuring consistent library resolution. This may
>   include hashing the downloaded versions of libraries and publishing
>   stack-style resolvers for sets of libraries that are proven to work well
>   together.
