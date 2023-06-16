---
layout: developer-doc
title: Sharing Libraries
category: libraries
tags: [libraries, editions, sharing]
order: 3
---

# Sharing Libraries

This document explains how users can share Enso libraries.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Sharing Privately](#sharing-privately)
- [Publishing](#publishing)

<!-- /MarkdownTOC -->

## Sharing Privately

To prepare the project for sharing, make sure that it has a proper `namespace`
field set in `package.yaml`. It should be set to something unique, like your
username.

> **NOTE**: The field `namespace` is a temporary workaround and in the near
> future it will be deprecated and handled mostly automatically. For now however
> you need to set it properly.

To share an Enso library, all you need to do is to package the project into an
archive (for example ZIP) and share it (through e-mail, cloud drive services
etc.) with your peers. Now to be able to use the library that was shared with
you, you need to extract it to the directory
`~/enso/libraries/<namespace>/<Project_Name>` (where on Windows `~` should be
interpreted as your user home directory). To make sure that the library is
extracted correctly, make sure that under the path
`~/enso/libraries/<namespace>/<Project_Name>/package.yaml` and that its
`namespace` field has the same value as the name of the `<namespace>` directory.

Now you need to set up your project properly to be able to use this unpublished
library. The simplest way to do that is to set `prefer-local-libraries` in your
project's `package.yaml` to `true`. This will make all libraries from
`~/enso/libraries` take precedence over published libraries set-up in the
edition. Alternatively, if you do not want to override all libraries, but only
some of them, you can add a local library override, by adding a proper entry in
the `libraries` section of the `edition` in your project's `package.yaml`, like
shown below:

```yaml
edition:
   (...)
   libraries:
     - name: <namespace>.<Project_Name>
       repository: local
```

Now, you can use your library by adding a proper import to your project:

```
import <namespace>.<Project_Name>
```

## Publishing

To publish a library, first you must obtain the upload URL of the repository, if
you are hosting the repository locally it will be `http://localhost:8080/upload`
(or possibly with a different port if that was overridden).

If the repository requires authentication, it is best to set it up by setting
the `ENSO_AUTH_TOKEN` environment variable to the value of your secret token.

Then you can use the Enso CLI to upload the project:

```bash
enso publish-library --upload-url <URL> <path to project root>
```

The `--upload-url` is optional, if not provided, the library will be uploaded to
the main Enso library repository. See `enso publish-library --help` for more
information.

## Crafting Library API

Exporting your project as a library is as easy publishing its ZIP archive.
However, if you want to increase comfort of users of your library, you may want
to polish the API of your library. Following sections describe how to do it.

### Ease of Exporting

To make creation of new libraries as smooth as possible, one doesn’t have to
define anything special than library identification in `package.yaml` (read
above). Every module your library contains is then exposed and ready for use.

### Exposing Logical Structure

However sometimes exposing everything can create confusion and you may want to
craft your library and expose just parts of your library. To do so define
`Main.enso` file in the root of your library. Once such file exists, only
elements re-exported from that file are going to be available to your library
users. Typical `Main.enso` file:

```haskell
import project.Api.Useful_Tool
export project.Api.Useful_Tool

import project.Helpers.Less_Useful_Tool
export project.Helpers.Less_Useful_Tool
```

One way to envision these re-export is to treat them like _symlinks on a
filesystem_. The structure of `.enso` files in your `project` defines a physical
structure of modules. Rather than exposing such physical structure (as is the
case when `Main.enso` file is missing), we expose _logical structure_ defined by
`Main.enso` file. In the example above users can use your library as:

```haskell
from Namespace.Library_Name import Useful_Tool, Less_Useful_Tool
```

By using re-exports one can re-export modules, types, constructors present in
the library in a way most suitable for users of the library.

### Encapsulation

Defining `Main.enso` library entry point not only allows re-exports of important
API elements, but it also provides encapsulation. Only the re-exported modules,
types, etc. are accessible to users of your library. The rest is hidden.

Should there be a file `Internal/Implementation/Utilities.enso` in a library
with a `Main.enso` entry point without being re-exported in `Main.enso`, then
trying to import such module from another project:

```haskell
import Namespace.Library_Name.Internal.Implementation.Utilities
```

Would generate a compiler error claiming such module isn't exposed for use.

### Ease of Imports

Enso language offers two basic ways to import a library. One can import it
**all**:

```haskell
from Namespace.Library_Name import all
```

That's the simplest import suitable for most users. One gets all re-exported
elements of a library as crafted by the library API designer.

Another way to import is to selectively enumerate elements to import:

```haskell
from Namespace.Library_Name import Useful_Tool, Less_Useful_Tool
```

This way one gets more direct control over the imported API elements. However
this style requires deeper knowledge of library design intentions. As such it is
suitable for more experienced users of the library.

In both cases one may access the imported API elements directly and may also get
access to other (sub)elements indirectly. One could for example use:

```haskell
direct = Useful_Tool
indirect = Useful_Tool.Submodule.Utility
```

e.g. access `Useful_Tool` directly and its API subelements with a `.` notation.
As noted in the _encapsulation_ section, only API elements re-exported in
`Main.enso` and their subelements are accessible from outside of other projects.
One cannot get access to more of a library than defined by **all**.
