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
├── visualization (optional)
│   └── ...
└── data (optional)
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

The way to populate the `polyglot` directory is _language dependent_. It is
different for JavaScript or Python, etc. A tutorial describing how to
incorporate a
[Java library](../polyglot/java.md#download-a-java-library-from-maven-central)
is available.

### The `data` Directory

The `data` directory contains any data files and resources that the user needs
quick access to. Allows referring to resource files in a location-independent
way, by using the `enso_project.data` method.

### The `package.yaml` File

`package.yaml` describes certain package metadata, such as its name, authors and
version. It also includes the list of extra dependencies of the package
(dependencies that are not present in the resolver or need a version override).
The following is an example of this manifest file.

```yaml
license: MIT
name: My_Package
version: 1.0.1
edition:
  extends: 2021.3
  enso-version: 0.2.12
  libraries:
    - name: Foo.Bar
      version: 1.2.3
      repository: main
prefer-local-libraries: false
authors:
  - name: John Doe
    email: john.doe@example.com
maintainers:
  - name: Jane Doe
    email: jane.doe@example.com
```

The following is the specification of the manifest fields. Fields marked as
**Optional (required for publishing)** are completely optional during
development - if not specified, their default values will be used. However, they
must be specified before publishing the package. A package missing any of these
fields cannot be published.

#### normalized-name

**Optional** _String_: The name that will be used as a prefix to the module
names of the project. If not set, it will be derived from the project `name`.

#### license

**Optional (required for publishing)** _String_: The short license name of this
package. Defaults to `None`, meaning the package is not safe for use by third
parties.

#### edition

**Optional (required for publishing)** _Edition_: Defines the Edition settings
of the package that determine the engine version and library resolution
settings. It is a sub-setting that can consist of multiple fields, see
[the Edition documentation](../libraries/editions.md#the-edition-file) for the
format description.

The field was added in version 0.2.12 as a replacement for `enso-version` as it
supersedes its functionality.

If the `edition` field is not specified, a default edition is used.

#### enso-version

**Deprecated** _String_: Specifies the Enso version that should be used for this
project. If not set or set to `default`, the default locally installed Enso
version will be used.

The field was deprecated in version 0.2.12. Currently it is still supported, but
the newer tools will migrate it to the `edition` format when the config is
modified.

If old tools see a config file that includes an `edition` setting but does not
include the `engine-version` (for example after the migration), they will fall
back to using the default engine version - that is because old tools were not
aware of the `edition` field, so they will simply ignore it.

If a config defines the `edition` field it should not define the
`engine-version` field anymore, as that could lead to inconsistent engine
version settings.

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

#### prefer-local-libraries

**Optional** _Boolean_: A flag that tells the library resolver to prefer local
library versions over the ones specified by the edition configuration. This is
useful to make all local libraries easily accessible, but in more sophisticated
scenarios individual `local` repository overrides should be used instead of
that. See [Library Resolution](../libraries/editions.md#library-resolution) for
more details.

If the flag is not specified, it defaults to `false`, delegating all library
resolution to the edition configuration. However, newly created projects will
have it set to `true`.

### The `visualization` Directory

As Enso is a visual language, a package may contain a specification of how data
can be displayed in various tools, for example
[Enso IDE](https://github.com/enso-org/ide). The Enso package structure may
optionally contain a `visualization` directory which may contain visualization
definitions.

For more information on how visualization definitions should work with the Enso
IDE, see
[this example](https://enso.org/docs/developer/docs/ide/product/visualizations.html#custom-visualization-example).

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
