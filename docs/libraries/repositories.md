---
layout: developer-doc
title: Repositories
category: libraries
tags: [repositories, libraries, editions]
order: 2
---

# Editions

This document describes the format of repositories that are providing Enso
libraries and Editions.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

<!-- /MarkdownTOC -->

## General Repository Format

Library and Edition providers are based on the HTTP(S) protocol. They are
designed in such a way that they can be backed by a simple file storage exposed
over the HTTP protocol, but of course it is also possible to implement the
backend differently as long as it conforms to the specification.

It is recommended that the server should support to send text files with
`Content-Encoding: gzip` to more effectively transmit the larger manifest files,
but this is optional and sending them without compression is also acceptable.
Nonetheless, Enso tools will send `Accept-Encoding: gzip` to indicate that they
support compressed transmission.

## Libraries Repository

The library repository should contain separate 'directories' for each prefix and
inside of them each library has its own directory named after the library name.

Inside that directory are the following files:

- `manifest.yaml` - the helper file that tells the tool what it should download,
  it is explained in more detail below;
- `package.yaml` - the package file of the library;
- `meta` - an optional metadata directory, that may be used by the marketplace;
- `LICENSE.md` - a license associated with the library; in our official
  repository the license is required, but internal company repositories may skip
  this, however if the file is not present a warning will be emitted during
  installation;
- `.tgz` packages containing sources and other data files of the library, split
  into components as explained below.

The directory structure is as below:

```
root
└── Prefix                  # The author's username.
    └── Library_Name        # The name of the library.
        ├── meta            # (Optional) Library metadata for display in the marketplace.
        │   ├── preview.png
        │   └── icon.png
        ├── main.tgz        # The compressed package containing sources of the library.
        ├── tests.tgz       # A package containing the test sources.
        ├── LICENSE.md
        ├── package.yaml
        └── manifest.yaml
```

### The Manifest File

### The Sub-Archives

The published library consists of sub-archives that allow to selectively
download only parts of the library.

Each downloaded archive will be extracted to the libraries' root directory in
such a way that common directories from multiple archives are merged on
extraction. However, different packages should not contain overlapping files as
there would be no way which of the files should be kept when the packages are
extracted.

> It is not an error if multiple downloaded packages contain conflicting files,
> but there are no guarantees as to which of the conflicting files is kept.

The package called `tests` is treated specially - it will not be downloaded by
default, as tests (which may contain heavy data files) are not necessary to use
the library.

> In the future, we will introduce platform specific sub-archives. The initial
> idea is that if an archive name has format `<prefix>-<os>-<arch>.tgz` where
> `os` is one of `windows`, `macos`, `linux` and `arch` is `amd64` (or in the
> future other values may be available here), the package is only downloaded if
> the current machine is running the same OS and architecture as indicated by
> its name. This is however a draft and the particular logic may be modified
> before it is implemented. Since the current behaviour is to download all
> packages (except for `test`), adding this feature will be backwards
> compatible, because the older versions will just download packages for every
> system (which will be unnecessary, but not incorrect).

All other packages are always downloaded by default. This may however change in
the future with additional reserved names with special behaviour being added.

There is no special name for a default package that should always be downloaded,
the only requirement is that the library should consist of at least one package
that is downloaded on every supported operating system (as otherwise it would be
empty). A safe name to choose is `main.tgz` as this name is guaranteed to never
become reserved, and so it will always be downloaded.

### Example

For example a library may have the following manifest:

```
archives:
- main.tgz
- tests.tgz
```

With the following directory structure (nodes under archives represent what the
archive contains):

```
root/Foo/Bar
├── main.tgz
│   ├── src
│   │   ├── Main.enso
│   │   └── Foo.enso
│   ├── polyglot
│   │   └── java
│   │       └── native-helper.jar
│   ├── THIRD-PARTY
│   │   ├── native-component-license.txt
│   │   └── native-component-distribution-notice.txt
│   └── data
│       └── required-constants.csv
├── tests.tgz
│   ├── tests
│   │   └── MainSpec.enso
│   └── data
│       └── tests
│           └── big-test-data.csv
├── package.yaml
├── LICENSE.md
└── manifest.yaml
```

Then if both `maing.tgz` and `tests.tgz` packages are downloaded (normally we
don't download the tests, but there may be special settings that do download
them), it will result in the following merged directory structure:

```
<downloaded-libraries-cache>/Foo/Bar
├── src
│   ├── Main.enso
│   └── Foo.enso
├── polyglot
│   └── java
│       └── native-helper.jar
├── THIRD-PARTY
│   ├── native-component-license.txt
│   └── native-component-distribution-notice.txt
├── tests
│   └── MainSpec.enso
├── data
│   ├── required-constants.csv
│   └── tests
│       └── big-test-data.csv
├── package.yaml
└── LICENSE.md
```

## Editions Repository

The Editions repository has a very simple structure.

Firstly, it must contain a `manifest.yaml` file at its root. The manifest
contains a single field `editions` which is a list of strings specifying the
editions that this provider provides.

For each entry in the manifest, there should be a file `<edition-name>.yaml` at
the root which corresponds to that entry.

For example for a manifest file with the following contents, we will have a
directory structure as shown below.

```yaml
editions:
  - "2021.1"
  - "foo"
  - "bar"
```

```
root
├── manifest.yaml
├── 2021.1.yaml
├── foo.yaml
└── bar.yaml
```
