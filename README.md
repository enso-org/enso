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

### Get insights you can act on, fast

<p>
  <a href="http://chat.enso.org">
    <img src="https://img.shields.io/discord/401396655599124480?label=Chat&color=2ec352&labelColor=2c3239"
         alt="Chat">
  </a>
  <a href="https://github.com/enso-org/enso/actions">
    <img src="https://github.com/enso-org/enso/workflows/Engine%20CI/badge.svg"
         alt="Actions Status">
  </a>
  <a href="https://github.com/enso-org/ide/actions">
    <img src="https://github.com/enso-org/ide/workflows/GUI%20CI/badge.svg"
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

Enso is a general-purpose programming language and environment for interactive
data processing. It is a tool that spans the entire stack, going from high-level
visualisation and communication to the nitty-gritty of backend services, all in
a single language.

- 🔗 **Connect to all the tools you're already using**  
  Enso ships with a robust set of libraries, allowing you to work with local
  files, databases, HTTP services and other applications in a seamless fashion.
  <br/><br/>
- 📊 **Cutting-edge visualization engine**  
  Enso is equipped with a highly-tailored WebGL visualization engine capable of
  displaying even millions of data points 60 frames per second in a web
  browser.<br/><br/>
- 🌐 **Polyglot**  
  Enso allows you to use any Java library in your code. Soon, it will also allow
  you to copy-paste code from Python, JavaScript, Ruby, and R with close-to-zero
  performance overhead at runtime.<br/><br/>
- ⚡ **High performance**  
  Enso graphs and code can run up to 100x faster than the analoguous Python
  code.<br/><br/>
- 🛡️ **Results you can trust**  
  Enso incorporates many recent innovations in data processing and programming
  language design to allow you to work quickly and trust the results that you
  get. It is a purely functional programming language with higher-order
  functions, user-defined algebraic datatypes, pattern-matching, and a rich set
  of primitive types.<br/><br/>
- 🌎 **Runs everywhere**  
  Enso is available on MacOS, Windows, and Linux, and the Enso IDE runs on
  web-native technologies. In time, you'll be able to run it in the web-browser,
  giving even your tablet of phone access to your data.<br/><br/>

<a href="https://www.youtube.com/watch?v=XReCQMZUmuE">See it in action.<br>

<img alt="An example Enso graph" src="https://user-images.githubusercontent.com/1623053/105841783-7c1ed400-5fd5-11eb-8493-7c6a629a84b7.png">
</a>

<br/>

### Getting Started

- Download Enso from our [GitHub
  Releases](https://github.com/enso-org/ide/releases).
- Follow [the Enso 101 tutorial](https://github.com/enso-org/tutorial_101) to
  take your first steps with Enso.
- Watch [the Enso YouTube
  tutorials](https://www.youtube.com/playlist?list=PLk8NuufOVK01GhaObYr1_gqeASlkj2um0)
  to learn more and improve your skills.
- [Keep up with the latest updates](https://medium.com/@enso_org) with our
  developer blog, or subscribe to the [mailing list](http://eepurl.com/bRru9j).
- Join us in the [Enso Discord](https://discord.gg/enso) to get help, share your
  use cases, meet the team behind Enso and other Enso users.

<br/>

### Project components

Enso consists of several sub projects:

- **Enso Engine:** The Enso Engine is the set of tools that implement the Enso
  language and its associated services. These include a just-in-time compiler,
  runtime, and language server. These components can be used on their own.

- **Enso IDE**. The [Enso IDE](https://github.com/enso-org/ide) is the desktop
  application that allows working with the visual form Enso. It consists of an
  Electron application, a high performance WebGL UI framework, and the Searcher
  which provides contextual search, hints, and documentation for Enso
  functionality.

<br/>

### License

This repository is licensed under the [Apache
2.0](https://opensource.org/licenses/apache-2.0), as specified in the
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
our [chat](https://discord.gg/enso), and take a look at our [development and
contribution guidelines](./docs/CONTRIBUTING.md). The latter describes all the
ways in which you can help out with the project, as well as provides detailed
instructions for building and hacking on Enso.

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

This folder also contains a document on Enso's [design
philosophy](./docs/enso-philosophy.md), that details the thought process that we
use when contemplating changes or additions to the language.

This documentation will evolve as Enso does, both to help newcomers to the
project understand the reasoning behind the code, but also to act as a record of
the decisions that have been made through Enso's evolution.
