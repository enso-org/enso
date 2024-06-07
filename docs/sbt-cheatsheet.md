# sbt cheatsheet

This document contains various references and notes about sbt build definition.
One of the motivation for this document is that the
[official sbt documentation](https://www.scala-sbt.org/1.x/docs/index.html) is
not very informative.

### Resources

- [Official sbt documentation](https://www.scala-sbt.org/1.x/docs/index.html)
- [sbt.Keys](https://github.com/sbt/sbt/blob/1.10.x/main/src/main/scala/sbt/Keys.scala)
  defines all the tasks and settings keys, including their documentation.

### Logging

- Get reference to the logger via `streams.value.log`.
- Log and error via `streams.value.log.error("My error")`
  - Note that this will not cause the task to fail, this will only log an
    `[error]` message. IF you want to fail a task, refer to
    [Fail a task](#fail-a-task).

### Fail a task

- Simply throw a `java.lang.RuntimeException`.
  - This will always work - it will crash the whole sbt process with the
    RuntimeException. But it is not the official recommended way.

### Inspect tasks and settings

- `inspect tree project/assembly`
  - This prints all the tasks and settings that will be executed when
    `project / assembly` is run.
  - It may be helpful to increase the width of the ASCI tree with
    `set asciiGraphWidth := 150`.
- See `help inspect`.

## See all the transitive dependencies

- `print dependencyTree`
  - This shows all the transitive dependencies for the default `Compile` scope.
- `print Test/dependencyTree`
  - This show all the transitive dependencies for the `Test` scope.

### Debugging sbt tasks

- There is `--jvm-debug` cmd line option to sbt which allows to attach a
  debugger to the sbt process.
  - This option is not very handy, as you will still not be able to add a
    breakpoint to various task definitions inside `build.sbt`, only to some
    classes in the `project` directory. Moreover, once you run `reload`, the
    debugging will not work for sure.
    - This is because sbt internally compiles everything from `build.sbt` a
      various anonymous classes and the debugger does not see their sources.
- It is better to either use `println` directly, or to use
  `streams.value.log.info` to log messages.

### Exclude internal sbt project from a fat jar

- Fat jar for `fat-project` is assembled via the `fat-project / assembly` task.
- There is `fat-project / assembly / assemblyExcludedJars` setting which is of
  type `Def.ClassPath`.
- To exclude internal `project` one must:
  - Set `project / Compile / exportJars := true`. This ensures that
    `project / Compile / exportedProducts` is a path to the jar.
  - In `fat-project / assembly / assemblyExcludedJars` get the path to the
    `project` jar via `project / Compile / exportedProducts`.
  - Declare dependency on `project / Compile / packageBin` from
    `fat-project / assembly`.
- Note that this is complicated because `assemblyExcludedJars` setting only
  works if it points to an already existing jar file.
