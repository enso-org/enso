<p align="center">
  <br/>
  <a href="http://luna-lang.org">
      <img
          src="https://user-images.githubusercontent.com/1623053/75657359-50c92300-5c66-11ea-9cb8-61da8ee34df1.png"
          alt="Enso Studio"
          width="128"
      />
  </a>
  <br/>
  <br/>
  <a href="http://luna-lang.org">
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

### Overview
<p>
  <a href="https://github.com/luna/ide/actions">
    <img src="https://github.com/luna/enso/workflows/Enso%20CI/badge.svg?branch=master"
         alt="Actions Status">
  </a>
  <a href="https://github.com/luna/enso/blob/master/LICENSE">
    <img src="https://img.shields.io/static/v1?label=Compiler%20License&message=Apache%20v2&color=2ec352&labelColor=2c3239"
         alt="License">
  </a>
  <a href="https://github.com/luna/ide/blob/master/LICENSE">
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
together. A developer's whiteboard on steroids, Enso lets you collaborate with
your co-workers, interactively fine-tune parameters, inspect results and
visually profile and debug your programs in real-time.

Enso consists of several sub projects, including the
[Enso Language Compiler](https://github.com/luna/enso) and the
[Enso Integrated Development Environment (IDE)](https://github.com/luna/ide).
You can also check out the [Enso Website](https://www.luna-lang.org) for more
information.

This repository contains [Enso Engine](engine/), which consists of the compiler,
type-checker, runtime and language server. These components implement Enso the
language in its entirety, and are usable in isolation.

### Getting Started
Enso is distributed as [pre-built packages](https://github.com/luna/enso/releases)
for MacOS, Linux and Windows, as well as universal `.jar` packages that can run
anywhere that [GraalVM](https://graalvm.org) can. See the
[documentation](http://luna-lang.org) for more.

### Building
The project builds on any platform where [GraalVM](https://graalvm.org) can run.
You will need the source code, and [`sbt`](https://www.scala-sbt.org/). For more
information, please read the detailed instructions in
[CONTRIBUTING.md](CONTRIBUTING.md).

### Enso's Design
If you would like to gain a better understanding of the principles on which Enso
is based, or just delve into the why's and what's of Enso's design, please take
a look in the [`doc/design/` folder](./doc/design).

This documentation will evolve as Enso does, both to help newcomers to the
project understand the reasoning behind the code, but also to act as a record of
the decisions that have been made through Enso's evolution.

### License
This repository is licensed under the
[Apache 2.0](https://opensource.org/licenses/apache-2.0), as specified in the
[LICENSE](https://github.com/luna/luna/blob/master/LICENSE) file.

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
[development and contribution guidelines](CONTRIBUTING.md). The latter describes
all the ways in which you can help out with the project, as well as provides
detailed instructions for building and hacking on Enso.

<a href="https://github.com/luna/enso/graphs/contributors">
  <img src="https://contributors-img.firebaseapp.com/image?repo=luna/enso" />
</a>
