# Enso Language Support for VSCode

[![Enso Language Support for VSCode](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml/badge.svg)](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml)

## Downloading

_Enso Tools for VSCode_ is available from
[VSCode marketplace](https://marketplace.visualstudio.com/items?itemName=Enso.enso4vscode).
Simply install it from there.

It is possible to download the latest development version of the _"VSCode
Extension" artifact_ from the
[latest actions run](https://github.com/enso-org/enso/actions/workflows/enso4igv.yml).
After downloading the ZIP file unzip a `.vsix` from it and install the `.vsix`
file into VSCode.

Your Enso files will get proper **syntax coloring**. You'll be able to **debug**
Java/Enso code interchangeably.

After installing the Enso `.vsix` file (and reloading window) we can find
following two extensions in the system:

![Installed VSCode extensions](https://user-images.githubusercontent.com/26887752/274904239-ae1ad4cc-e2ec-4c5b-bca0-c7d7189c6885.png)

## Using & Debugging

Once installation is over let's continue with choosing _File/Open Folder..._ and
opening root of [Enso Git Repository](http://github.com/enso-org/enso)
(presumably already built with
[sbt buildEngineDistribution](https://github.com/enso-org/enso/blob/develop/docs/CONTRIBUTING.md#running-enso)).
Following set of projects is opened and ready for use:

![Enso Projects](https://github.com/enso-org/enso/assets/26887752/7919d2ee-4bcd-4b7b-954a-e2dc61f7c01a)

With the workspace opened, you can open any Enso or Java file. Let's open for
example `Vector_Spec.enso` - a set of unit tests for `Vector` - a core class of
Enso standard library:

![Openning Vector](https://github.com/enso-org/enso/assets/26887752/0d182fc8-4ff9-48d7-af63-35cad5fb75cc)

It is now possible to place breakpoints into the `Vector_Spec.enso` file. Let's
place one on line 120:

![Breakpoint](https://github.com/enso-org/enso/assets/26887752/b6ae4725-49ef-439f-b900-3e08724e3748)

Let's do a bit of debugging. Select _"Listen to 5005"_ debug configuration:

![Listen to 5005](https://github.com/enso-org/enso/assets/26887752/1874bcb1-cf8b-4df4-92d8-e7fb57e1b17a)

And then just
[execute the engine distribution](https://github.com/enso-org/enso/blob/develop/docs/CONTRIBUTING.md#running-enso)
in debug mode:

```bash
sbt:enso> runEngineDistribution --debug --run test/Base_Tests/src/Data/Vector_Spec.enso
```

After a while the breakpoint is hit and one can inspect variables, step over the
statements and more...

![Breakpoint in Enso](https://github.com/enso-org/enso/assets/26887752/54ae4126-f77a-4463-9647-4dd3a5f83526)

...as one can seamlessly switch to debugging on the Enso interpreter itself! One
can place breakpoint into Java class like `PanicException.java` and continue
debugging with `F5`:

![Breakpoint in Java](https://github.com/enso-org/enso/assets/26887752/db3fbe4e-3bb3-4d4a-bb2a-b5039f716c85)

Should one ever want to jump back from Java to Enso one can use the _"Pause in
GraalVM Script"_ action. Select it and continue with `F5` - as soon as the code
reaches a statement in Enso, it stops:

![Pause in GraalVM](https://github.com/enso-org/enso/assets/26887752/98eb0bb7-48c2-4208-9d9a-5b8bacc99de2)

Read more on [Enso & Java Debugging](../../docs/debugger/runtime-debugging.md)

## Building VSCode Extension

To build this VSCode extension and obtain _Enso_ syntax coloring as well as
support for editing and debugging of `engine/runtime` sources in **VSCode**:

```
enso/tools/enso4igv$ mvn clean install -Pvsix
enso/tools/enso4igv$ ls *.vsix
enso4vscode-*.vsix
```

one needs to have `npm`, Java and `mvn` available to successfully build the
VSCode extension.

![Install from VSIX...](https://user-images.githubusercontent.com/26887752/269557870-9d7c35d6-44b2-4157-b451-bb27980425c7.png)

Once the `.vsix` file is created, it can be installed into VSCode. Select
_Extension perspective_ and choose _Install from VSIX..._ menu item.

## Reference

There are extensions for [NetBeans](http://netbeans.apache.org) and also for
**IGV**. Read more [here](IGV.md).
