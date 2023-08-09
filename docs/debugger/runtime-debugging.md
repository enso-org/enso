# Runtime (Engine) debugging

This section explains how to debug various parts of the Engine. By Engine, we
mean all the Java code located in the `runtime` SBT project, in `engine`
directory.

## Debugging source file evaluation

This subsection provides a guide how to debug a single Enso source file
evaluation. To evaluate a single source file, we use the _Engine distribution_
built with `buildEngineDistribution` command. Both of the following two options
starts the JVM in a debug mode. After the JVM is started in a debug mode, simply
attach the debugger to the JVM process at the specified port.

The first option is to invoke the engine distribution from SBT shell with:

```sh
sbt:enso> runEngineDistribution --debug --run ./test/Tests/src/Data/Numbers_Spec.enso
```

The second options is to pass in special JVM arguments when launching the
`bin/enso` from the engine distribution:

```bash
enso$ JAVA_OPTS=-agentlib:jdwp=transport=dt_socket,server=n,address=5005 ./built-distribution/enso-engine-*/enso-*/bin/enso --run ./test/Tests/src/Data/Numbers_Spec.enso
```

### Tips and tricks

There is no simple mapping of the Enso source code to the engine's Java code, so
if you try to debug a specific expression, it might be a bit tricky. However,
the following steps should help you to skip all the irrelevant code and get to
the code you are interested in:

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
