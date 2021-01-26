<p align="center">
  <br/>
  <a href="http://enso.org">
      <img
          src="https://user-images.githubusercontent.com/1623053/79905826-22bac080-8417-11ea-82b0-ee015904a485.png"
          alt="Enso Visual Environmment"
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


### Get insights you can act on, fast.

<p>
  <a href="http://chat.enso.org">
    <img src="https://img.shields.io/discord/401396655599124480?label=Chat&color=2ec352&labelColor=2c3239"
         alt="Chat">
  </a>
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
</p>

Enso is an interactive data processing environment with both a visual and a textual programming language interface.

* It is extensible and easy to connect to all the tools you’re already using.
* It uses the latest innovations in data processing to allow you to work fast and trust the results you get.
* Available on MacOS, Windows, and Linux.

<a href="https://www.youtube.com/watch?v=XReCQMZUmuE">See it in action.<br>
<img width="600" alt="An example Enso graph" src="https://user-images.githubusercontent.com/1790822/105632239-7056d480-5e52-11eb-98fd-476254b1df8b.png">
</a>

<br/>

### Getting Started

* Download Enso from the [GitHub Releases](https://github.com/enso-org/ide/releases).
* Follow [the Enso 101 tutorial](https://github.com/enso-org/tutorial_101) to take your first steps with Enso.
* Watch [the Enso YouTube tutorials](https://www.youtube.com/playlist?list=PLk8NuufOVK01GhaObYr1_gqeASlkj2um0) to learn more and improve your skills.
* [Keep up with the latest updates](https://medium.com/@enso_org) with our developer blog, or subscribe to the [mailing list](http://eepurl.com/bRru9j).
* Join us in the [Enso Discord](https://discord.gg/enso) to get help, share use cases, meet the team behind Enso and other Enso users. 

<br/>

### Project components

Enso consists of several sub projects:

  * **Enso Engine**  
    The [Enso Engine](./engine/) is the , which consists of the
    compiler, type-checker, runtime and language server. These components implement
    Enso the language in its entirety, and are usable in isolation. 

    You can download the Enso Engine from the [GitHub Releases](https://github.com/enso-org/enso/releases) of this repository. For more information on how to run the Enso Engine, please refer to the documentation on [getting Enso](https://github.com/enso-org/enso/blob/main/docs/getting-enso.md).

  * **Enso IDE**. 
    The [Enso IDE](https://github.com/enso-org/ide) is the desktop application that allows working with visual Enso. It consists of an Electron application, a high     performance WebGL UI framework, and the Searcher which provides search, hints, and documentation for Enso functions.

    You can download the Enso IDE from the [GitHub Releases of the IDE repository](https://github.com/enso-org/ide/releases). For more information on usage of the IDE, please refer to the [101 tutorial](https://github.com/enso-org/tutorial_101) and the [product documentation](https://dev.enso.org/docs/ide/product/).

<br/>

### License

This repository is licensed under the
[Apache 2.0](https://opensource.org/licenses/apache-2.0), as specified in the
[LICENSE](https://github.com/enso-org/enso/blob/main/LICENSE) file.

This license set was choosen to both provide you with a complete freedom to use
Enso, create libraries, and release them under any license of your choice, while
also allowing us to release commercial products on top of the platform,
including Enso Cloud and Enso Enterprise server managers.

<br/>

### Contributing to Enso

Enso is a community-driven open source project which is and will always be open
and free to use. We are committed to a fully transparent development process and
highly appreciate every contribution. If you love the vision behind Enso and you
want to redefine the data processing world, join us and help us track down bugs,
implement new features, improve the documentation or spread the word!

If you'd like to help us make this vision a reality, please feel free to join
our [chat](https://discord.gg/enso), and take a look at our
[development and contribution guidelines](./docs/CONTRIBUTING.md). The latter
describes all the ways in which you can help out with the project, as well as
provides detailed instructions for building and hacking on Enso.

If you believe that you have found a security vulnerability in Enso, or that you
have a bug report that poses a security risk to Enso's users, please take a look
at our [security guidelines](./docs/SECURITY.md) for a course of action.

<br/>

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
