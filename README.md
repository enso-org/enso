<p align="center">
  <br/>
  <a href="http://enso.org">
      <img
          src="https://user-images.githubusercontent.com/1623053/79905826-22bac080-8417-11ea-82b0-ee015904a485.png"
          alt="Enso Language"
          width="136"
      />
  </a>
  <br/>
  <br/>
  <a href="http://enso.org">
      <img
          src="https://user-images.githubusercontent.com/1623053/75661125-05664300-5c6d-11ea-9bd3-8a5355db9609.png"
          alt="Enso Language"
          width="240"
      />
  </a>
  <br/>
  <br/>
  <br/>
</p>

### Fluidly Combining Worlds
<p>
  <a href="https://github.com/enso-org/enso/actions">
    <img src="https://github.com/enso-org/enso/workflows/Engine%20CI/badge.svg"
         alt="Actions Status">
  </a>
  <a href="https://github.com/enso-org/enso/blob/main/LICENSE">
    <img src="https://img.shields.io/static/v1?label=Compiler%20License&message=Apache%20v2&color=2ec352&labelColor=2c3239"
         alt="License">
  </a>
  <a href="https://github.com/enso-org/ide/blob/main/LICENSE">
    <img src="https://img.shields.io/static/v1?label=GUI%20License&message=AGPL%20v3&color=2ec352&labelColor=2c3239"
         alt="License">
  </a>
  <a href="http://chat.luna-lang.org">
    <img src="https://img.shields.io/discord/401396655599124480?label=Chat&color=2ec352&labelColor=2c3239"
         alt="Chat">
  </a>
</p>

Enso is an open-source, visual language for data science that lets you design,
prototype, develop and refactor any application by connecting visual elements
together. Enso lets you collaborate with your co-workers, interactively
fine-tune parameters, inspect results and visually profile and debug your
programs in real-time, creating a moment where the mind is free to let the body
create.

Enso consists of several sub projects, including the
[Enso Language Compiler](https://github.com/enso-org/enso) and the
[Enso Integrated Development Environment (IDE)](https://github.com/enso-org/ide).
You can also check out the [Enso Website](https://enso.org) for more
information.

This repository contains [Enso Engine](./engine/), which consists of the
compiler, type-checker, runtime and language server. These components implement
Enso the language in its entirety, and are usable in isolation.

### Getting Started
Enso is distributed as
[pre-built packages](https://github.com/enso-org/enso/releases)
for MacOS, Linux and Windows, as well as universal `.jar` packages that can run
anywhere that [GraalVM](https://graalvm.org) can. See the
[documentation](http://enso.org) for more.

Nightly builds can be obtained from the CI artifacts. Please refer to the
[documentation](./docs/getting-enso.md).

If you want to read more about the internals of Enso, please take a look at the
developer documentation rendered [here](https://dev.enso.org), or in the
repository [here](./docs).

### Building
The project builds on any platform where [GraalVM](https://graalvm.org) can run.
You will need the source code, and [`sbt`](https://www.scala-sbt.org/). For more
information, please read the detailed instructions in
[CONTRIBUTING.md](./docs/CONTRIBUTING.md).

### Enso's Design
If you would like to gain a better understanding of the principles on which Enso
is based, or just delve into the why's and what's of Enso's design, please take
a look in the [`docs/` folder](./docs/). It is split up into subfolders for each
component of Enso. You can view this same documentation in a rendered form at
[the developer docs website](https://dev.enso.org).

This folder also contains a document on Enso's
[design philosophy](./docs/enso-philosophy.md), that details the thought process
that we use when contemplating changes or additions to the language.

This documentation will evolve as Enso does, both to help newcomers to the
project understand the reasoning behind the code, but also to act as a record of
the decisions that have been made through Enso's evolution.

### License
This repository is licensed under the
[Apache 2.0](https://opensource.org/licenses/apache-2.0), as specified in the
[LICENSE](https://github.com/enso-org/enso/blob/main/LICENSE) file.

This license set was choosen to both provide you with a complete freedom to use
Enso, create libraries, and release them under any license of your choice, while
also allowing us to release commercial products on top of the platform,
including Enso Cloud and Enso Enterprise server managers.

### Contributing to Enso
Enso is a community-driven open source project which is and will always be open
and free to use. We are committed to a fully transparent development process and
highly appreciate every contribution. If you love the vision behind Enso and you
want to redefine the data processing world, join us and help us track down bugs,
implement new features, improve the documentation or spread the word!

If you'd like to help us make this vision a reality, please feel free to join
our [chat](http://chat.luna-lang.org/), and take a look at our
[development and contribution guidelines](./docs/CONTRIBUTING.md). The latter
describes all the ways in which you can help out with the project, as well as
provides detailed instructions for building and hacking on Enso.

If you believe that you have found a security vulnerability in Enso, or that
you have a bug report that poses a security risk to Enso's users, please take
a look at our [security guidelines](./docs/SECURITY.md) for a course of action.

<a href="https://github.com/enso-org/enso/graphs/contributors">
  <img src="https://opencollective.com/enso-language/contributors.svg?width=890&button=false">
</a>
