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

- [Publishing](#publishing)
- [Publishing to library server](#publishing-to-library-server)
- [Sharing Privately](#sharing-privately)

<!-- /MarkdownTOC -->

## Publishing

A library can be automatically published during the (nightly) release as a
GitHub release asset.

Such _published_ libraries are:

- Not part of the standard Enso release (`.dmg`, `.AppImage`, and `.exe` bits).
  - That is, if user downloads the Enso binary from a release, it will not
    contain the published libraries.
- Downloaded on demand when imported by an Enso workflow.
- Uploaded as assets on corresponding GitHub release.

To mark a library for _publishing_, apply the following changes in
[edition.template.yaml](https://github.com/enso-org/enso/blob/19f3348d09cf5002e6b67705d4a1d1498190bae0/distribution/edition.template.yaml):

- Create new repository with `jar` URL scheme and a name with `.zip` suffix.
- Change the library's repository to the newly created one.

You only have to modify the `edition.template.yaml` file, the rest is handled by
the release process. See the following section for details.

### How it works

Library uploading is handled by the
[Upload Backend job](https://github.com/enso-org/enso/blob/dbc74c509687e313eb1c3a10b2d2a76299f058bb/.github/workflows/release.yml#L170)
which is part of the
[(Nightly) Release workflow](https://github.com/enso-org/enso/actions/workflows/nightly.yml).

In this job:

- The engine distribution is build, with `sbt buildEngineDistribution`.
- Which generates an edition file out of the template.
- The release script reads the generated edition file and uploads all the
  libraries marked for publishing to the GitHub release as assets.

See:

- [build_tools/build/src/engine/edition.rs](https://github.com/enso-org/enso/blob/607b33414f94e77746c3880c417b5ebd523ad9f6/build_tools/build/src/engine/edition.rs).
  Which contains the functionality explained by this section.
- [Release policy](https://github.com/enso-org/enso/blob/develop/docs/distribution/release-policy.md)
  for the generic release process documentation.

## Publishing to library server

<!-- prettier-ignore -->
> [!WARNING]
> Works only on local host.

To publish a library, first you must obtain the upload URL of the repository, if
you are hosting the repository locally it will be `http://localhost:8080/upload`
(or possibly with a different port if that was overridden).

If the repository requires authentication, it is best to set it up by setting
the `ENSO_AUTH_TOKEN` environment variable to the value of your secret token.

Then you can use the `ensoup` launcher to upload the project:

```bash
ensoup publish-library --upload-url <URL> <path to project root>
```

The `--upload-url` is optional, if not provided, the library will be uploaded to
the main Enso library repository. See `ensoup publish-library --help` for more
information.

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
