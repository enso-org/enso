# High & Low Runtime Debugging

This document describes how to debug **everything** - e.g. how to debug the
_Java code_ that powers the Enso _engine & interpreter_ as well as Enso standard
_libraries Java code_ (used to interact with the operating system via Java
APIs). At the same thetime the document describes how to _map that information_
onto the execution of the actual _Enso source code_ being interpreted.

## Debugging Single Source File

To analyze what is happening in the interpreter, we often want to debug
excecution of a _single Enso file_.

Get started by building the _Engine distribution_ built with
`sbt buildEngineDistribution` command. Then configure your IDE to understand the
_engine Java & Scala sources_ which are mostly located in the
[engine subdirectory](https://github.com/enso-org/enso/tree/develop/engine). The
sources are known to be understandable by:

- VSCode - with
  [Enso Extension described herein](../../tools/enso4igv/README.md)
- IntelliJ
- IGV - with [extension described herein](../../tools/enso4igv/IGV.md)
- [NetBeans](http://netbeans.apache.org) - with the same
  [extension as IGV](../../tools/enso4igv/IGV.md) is using

There are two ways to start JVM in a debug mode. The first one is fully
integrated into the `sbt` build system. It builds the engine (in case there were
any changes) and then starts the JVM in debug mode trying to attach to port
`5005`:

```sh
sbt:enso> runEngineDistribution --debug --run ./test/Base_Tests/src/Data/Numbers_Spec.enso
```

The second options gives one a complete control as it launches everything from a
command line. By specifying `JAVA_OPTS` environment variable one influences the
special JVM arguments when launching the `bin/enso` from the engine
distribution:

```bash
enso$ JAVA_OPTS=-agentlib:jdwp=transport=dt_socket,server=n,address=5005 ./built-distribution/enso-engine-*/enso-*/bin/enso --run ./test/Base_Tests/src/Data/Numbers_Spec.enso
```

Both of the approaches launch the JVM in a _debug mode_. Once the JVM is
started, simply attach the debugger to the JVM process at the specified port
(usually `5005` is used).

### Attaching from VSCode

First of all make sure your VSCode contains the
[Enso supporting extension](https://marketplace.visualstudio.com/items?itemName=Enso.enso4vscode).
Follow the [instructions provided herein](../../tools/enso4igv/README.md) to
install everything that's needed.

Once the
[Enso extension](https://marketplace.visualstudio.com/items?itemName=Enso.enso4vscode)
is installed, open the root of Enso repository as workspace and select _"Listen
to 5005"_ debug configuration:

![Listen to 5005](https://github.com/enso-org/enso/assets/26887752/1874bcb1-cf8b-4df4-92d8-e7fb57e1b17a)

Once the connection with the Enso JVM is made, debug the Java code as usual.

### Mapping Java and Enso Code

The _Enso interpreter_ maintains a mapping between its Java code and appropriate
_Enso Code_ internally. It shall be no problem to use that information in the
debugger. All the mentioned IDEs (alas except IntelliJ) support such _mixed
debugging_ of Java and Enso code.

Put a breakpoint into `.enso` file and after a while the breakpoint is hit and
one can inspect variables, step over the _Enso statements_ and more...

![Breakpoint in Enso](https://github.com/enso-org/enso/assets/26887752/54ae4126-f77a-4463-9647-4dd3a5f83526)

...as one can seamlessly switch to debugging on the Enso interpreter itself! One
can place breakpoint into Java class like `PanicException.java` and continue
debugging with `F5`:

![Breakpoint in Java](https://github.com/enso-org/enso/assets/26887752/db3fbe4e-3bb3-4d4a-bb2a-b5039f716c85)

Should one ever want to jump back from Java to Enso one can use the _"Pause in
GraalVM Script"_ action. Select it and continue with `F5` - as soon as the code
reaches a statement in Enso, it stops:

![Pause in GraalVM](https://github.com/enso-org/enso/assets/26887752/98eb0bb7-48c2-4208-9d9a-5b8bacc99de2)

Similar behavior to _VSCode_ is available in _IGV_ and _NetBeans_. Understanding
both the _engine Java code_ as well as _Enso Code_ shall be pretty simple with
these tools. For example _NetBeans_ offers _Toggle Pause in GraalVM Script_
button in the toolbar:

![NetBeans Debugger](https://user-images.githubusercontent.com/26887752/209614191-b0513635-819b-4c64-a6f9-9823b90a1513.png)

and your execution shall stop on the next `.enso` line of code.

These advanced developer tools allow one to debug both - the _Enso code_ as well
as _Java code_. The stack traces shows a _mixture of Java and Enso stack frames_
by default. Right-clicking on the thread allows one to switch to plain Java view
(with a way more stack frames) and back. Analyzing low level details as well as
Enso developer point of view shall be simple with such tool.

### Tips and Tricks (for poor IntelliJ users)

Finding the mapping of the Enso source code to the engine's Java code in
_IntelliJ_ isn't easy. Trying to find out how to debug a specific expression is
way more tricky than in case of _VSCode_, _IGV_ or _NetBeans_. However, if you
really want to stick with _IntelliJ_ as your only tool, following steps may help
you to skip all the irrelevant code and get to the code you are interested in:

- To get the mapping to Enso source code, evaluate the following expression in
  the Java debugger: `this.getRootNode().getSourceSection()`. Note that this,
  obviously, works only when the debugger is stopped in some Truffle node code.
- To debug a method called `foo`, put a breakpoint in
  `org.enso.interpreter.node.ClosureRootNode#execute` with a condition on
  `this.name.contains("foo")`
- To debug a specific expression, put some _unique_ expression, like
  `Debug.eval "1+1"`, in front of it and put a breakpoint in a Truffle node
  corresponding to that unique expression, in this case that is
  `org.enso.interpreter.node.expression.builtin.debug.DebugEvalNode`.

## Debugging annotation processors

The Engine uses annotation processors to generate some of the Java code, e.g.,
the builtin methods with `org.enso.interpreter.dsl.MethodProcessor`, or JMH
benchmark sources with `org.enso.benchmarks.processor.BenchProcessor`.
Annotation processors are invoked by the Java compiler (`javac`), therefore, we
need special instructions to attach the debugger to them.

Let's debug `org.enso.interpreter.dsl.MethodProcessor` as an example. The
following are the commands invoked in the `sbt` shell:

- `project runtime`
- `clean`
  - Delete all the compiled class files along with all the generated sources by
    the annotation processor. This ensures that the annotation processor will be
    invoked.
- `set javacOptions += FrgaalJavaCompiler.debugArg`
  - This sets a special flag that is passed to the frgaal Java compiler, which
    in turn waits for the debugger to attach. Note that this setting is not
    persisted and will be reset once the project is reloaded.
- `compile`
  - Launches the Java compiler, which will wait for the debugger to attach. Put
    a breakpoint in some class of `org.enso.interpreter.dsl` package. Wait for
    the message in the console instructing to attach the debugger.
- To reset the `javacOptions` setting, either run
  `set javacOptions -= FrgaalJavaCompiler.debugArg`, or reload the project with
  `reload`.
