# Enso Language Support for NetBeans & Ideal Graph Visualizer

[![Enso Language Support for IGV](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml/badge.svg)](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml)

[Enso language](http://enso.org) runtime engine is built on top of
[GraalVM](http://graalvm.org) and its _Truffle framework_. Enso,
as a good citizen of the GraalVM ecosystem, benefits from polyglot
capabilities of GraalVM as well as its rich tooling offering. One
of such tools is _IGV_ - the _Ideal Graph Visualizer_ - an excellent
tool to get insights into behavior of Graal compiler.

This document shows how to use _IGV_ with the _Enso language_.

## Installation

Visit [GraalVM.org](http://graalvm.org) download page and continue 
towards _enterprise edition_ option. There is an _Ideal Graph Visualizer_
option. After clicking thru the confirmation dialogs you should get a ZIP -
I've just got `idealgraphvisualizer-22.1.0.zip` and then:

```bash
$ unzip idealgraphvisualizer-22.1.0.zip
$ ./idealgraphvisualizer/bin/idealgraphvisualizer --userdir /tmp/emptyuserdir
```

launches the _IGV_ application. One doesn't have to use the `--userdir` option,
but doing so ensures the newly running _IGV_ process is isolated from any settings
left around by previous usage of _IGV_.

Now download [Enso Language Support module](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml).
Follow the [GitHub actions link](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml)
and select a build (usually the latest one). The build summary page provides various information
as well as list of artifacts at the bottom. Download the _Enso IGV Plugin_ ZIP file.
Make sure you are logged into GitHub -  artifacts are only available to those logged in.
Unzip it and get `enso*.nbm` file. This file can be installed into
_IGV_ (or any other [NetBeans](http://netbeans.apache.org) based application).
Go to _Tools_/_Plugins_/_Downloaded_ and install the NBM file.
