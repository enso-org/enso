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

To share an Enso library, all you need to do is to package the project into an
archive (for example ZIP) and share it (through e-mail, cloud drive services
etc.) with your peers. Now to be able to use the library that was shared with
you, you need to extract it to the directory `~/enso/libraries/<Project_Name>`
(where on Windows `~` should be interpreted as your user home directory).

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
