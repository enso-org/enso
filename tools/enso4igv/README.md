# Enso Language Support for NetBeans, Ideal Graph Visualizer & VSCode

[![Enso Language Support for IGV](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml/badge.svg)](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml)

[Enso language](http://enso.org) runtime engine is built on top of
[GraalVM](http://graalvm.org) and its _Truffle framework_. Enso, as a good
citizen of the GraalVM ecosystem, benefits from polyglot capabilities of GraalVM
as well as its rich tooling offering. One of such tools is _IGV_ - the _Ideal
Graph Visualizer_ - an excellent tool to get insights into behavior of Graal
compiler.

This document shows how to use _IGV_ with the _Enso language_. The command line
instructions format is Unix oriented. Use instructions appropriate for your
operating system to perform the same on different _OS_. _IGV_ itself as well as
the _Enso language_ support are platform neutral with launch scripts for all
major operating systems.

## Installation

Visit [GraalVM.org](http://graalvm.org) download page and continue towards
_enterprise edition_ option. There is an _Ideal Graph Visualizer_ option. After
clicking through the confirmation dialogs you should get a ZIP - I've just got
`idealgraphvisualizer-22.1.0.zip` and then:

```bash
$ unzip idealgraphvisualizer-22.1.0.zip
$ ./idealgraphvisualizer/bin/idealgraphvisualizer --userdir /tmp/emptyuserdir
```

launches the _IGV_ application. One doesn't have to use the `--userdir` option,
but doing so ensures the newly running _IGV_ process is isolated from any
settings left around by previous usage of _IGV_.

IGV understands Enso when
[Enso Language Support module](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml)
is installed. Login to GitHub, follow the
[GitHub actions link](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml)
and select a build. Unless you have some special needs choose the latest one.
The build summary page provides various information as well as list of artifacts
at the bottom. Download the _Enso IGV Plugin_ ZIP file (make sure you are logged
into GitHub - artifacts are only available to those logged in). Unzip it and get
`enso*.nbm` file. This file can be installed into _IGV_ (or any other
[NetBeans](http://netbeans.apache.org) based application). Go to
_Tools_/_Plugins_/_Downloaded_/_Add Plugins_ and select the NBM file.

![Tools/Plugins/Downloaded](https://user-images.githubusercontent.com/26887752/174608153-9f0b54fa-b507-45be-83de-d7911186d121.png)

Proceed by clicking _Install_. You may be asked to download _TextMate Lexer_ - a
necessary dependency of the _Enso support_ module. Continue through the wizard
to _finish_ the installation.

![Tools/Plugins/Downloaded](https://user-images.githubusercontent.com/26887752/174608219-1faf2728-0045-478b-a297-e3c06f691b19.png)

## Using the IGV

Build an instance of the Enso runtime engine (see
[Running Enso](../../docs/CONTRIBUTING.md#running-enso)) using:

```bash
enso$ sbt buildEngineDistribution
```

and then launch it with special `--dump-graphs` option:

```bash
enso$ ./built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/bin/enso --dump-graphs --run yourprogram.enso
```

When executed on [GraalVM 22.3.0](http://graalvm.org) these options instruct the
_Graal/Truffle compiler_ to dump files into `graal_dumps/_sometimestamp_`
directory. Generating these files takes a while - make sure `yourprogram.enso`
runs long enough for the system to warmup, compile the code and run at _full
speed_.

#### Sieve of Eratosthenes Example

As an example you can download
[sieve.enso](https://github.com/jtulach/sieve/blob/5b32450da35415322e683bb9769aa45f0d71f1df/enso/sieve.enso)
which computes hundred thousand of prime numbers repeatedly and measures time of
each round. Download the file and launch Enso with `--dump-graphs` argument:

```bash
enso$ ./built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/bin/enso --dump-graphs --run sieve.enso
```

Bunch of files in `graal_dumps/*` subdirectory is going to be generated:

```bash
enso$ ls graal_dumps/*/Truffle* | tail -n5
graal_dumps/2022.06.20.06.18.21.733/TruffleHotSpotCompilation-9889[argument<2>].bgv
graal_dumps/2022.06.20.06.18.21.733/TruffleHotSpotCompilation-9896[IfThenElseMethodGen@3af870b9_<split-62b6b4f3>]_1.bgv
graal_dumps/2022.06.20.06.18.21.733/TruffleHotSpotCompilation-9896[IfThenElseMethodGen@3af870b9_<split-62b6b4f3>].bgv
graal_dumps/2022.06.20.06.18.21.733/TruffleHotSpotCompilation-9935[Primes.next_<split-717d5bdf>]_1.bgv
graal_dumps/2022.06.20.06.18.21.733/TruffleHotSpotCompilation-9935[Primes.next_<split-717d5bdf>].bgv
```

Let's launch IGV with Enso integration. Locate the `engine/runtime` directory
and open it as _"project"_ in IGV:

![Open Project in IGV](https://user-images.githubusercontent.com/26887752/201684275-b3ee7a37-7b55-4290-b426-75df0280ba32.png)

The project directories (not only `runtime`, but also other like
`runtime-language-epb`, etc.) are recognized only if you have built the Enso
engine sources with `sbt buildEngineDistribution`. Once the IGV opens the
`runtime` & co. projects, it allows smooth navigation among the sources

![IGV Projects view](https://user-images.githubusercontent.com/26887752/209615348-8911af4c-4680-4e61-ac87-19a19738e2ca.png)

With such setup let's open graph for one of the top-most functions:
`TruffleHotSpotCompilation*Primes*next*.bgv`. Choose compilation phase _"Before
lowering"_:

![Before Lowering Graph](https://user-images.githubusercontent.com/26887752/174608397-331a4438-1f12-40b0-9fcd-59eda5e53fb6.png)

Now you can inspect the _compiler graphs_ the regular _IGV_ way. Let's locate
for example `LoadField#FunctionSchema.isFullyApplied` node and let's check how
it got _inlined_(you can use search box in the top-right corner)

![Inlining Stacktrace](https://user-images.githubusercontent.com/26887752/174608478-e7002c43-d746-42c0-b61c-92ceb9d9f124.png)

The stack trace shows what methods of the Enso interpreter and Truffle runtime
are _"inlined on stack"_ when this node is being compiled. However thanks to
integration with `engine/runtime` sources one can directly jump to the sources
of the interpreter that represent certain graph nodes:

![Associated Engine Sources](https://user-images.githubusercontent.com/26887752/201688115-4afdb2ac-9a41-4469-8b7b-d7130f74883e.png)

Not only that, but one we can also switch to _Enso view_:

![Enso Source](https://user-images.githubusercontent.com/26887752/174608595-4ce80b00-949a-4b28-84a7-60d5988bfc70.png)

By choosing the _Enso language icon_ in front of the stack trace combo, the
source code of our `.enso` program is opened and we can analyze what _compiler
nodes_ refer to what lines in the our _Enso_ program. Click _Navigate to Source_
icon in the _Stack View_ to get from graph node to source. Select a drop down
widget in the editor toolbar to show you what compiler nodes as associated with
currently selected line.

## Building

The plugin can be rebuilt using [Apache Maven](http://maven.apache.org). The
build is platform independent. The following instructions are for Unix like
environment. Switch to this directory and invoke:

```bash
enso/tools/enso4igv$ mvn clean install
enso/tools/enso4igv$ ls target/*.nbm
target/enso4igv-*-SNAPSHOT.nbm
```

an NBM file is generated which can be installed into IGV, NetBeans or any other
NetBeans based application.

## Building VSCode Extension

One can package the same plugin into a VSCode extension and obtain _Enso_ syntax
coloring as well as support for editing `engine/runtime` sources in **VSCode**.
Just invoke:

```
enso/tools/enso4igv$ npm install
enso/tools/enso4igv$ npm run vsix
enso/tools/enso4igv$ ls *.vsix
enso4vscode-*.vsix
```

one needs to have `npm`, Java and `mvn` available to successfully build the
VSCode extension. Alternatively one can use Maven to built the VSIX extension
via `mvn clean install -Pvsix`.

![Install from VSIX...](https://user-images.githubusercontent.com/26887752/210131513-8c729f9b-5ddc-43aa-9ad5-420b7d87d81d.png)

Once the `.vsix` file is created, it can be installed into VSCode. Select
_Extension perspective_ and choose _Install from VSIX..._ menu item.
