package org.enso.build

import sbt._

/** Command allowing to run a task with additional JVM-level flags.
  * Supported tasks are run and benchOnly.
  * Supported flags are:
  * * `--dumpGraphs`: dumps IGV output of the program
  * * `--showCompilations`: prints Truffle compilation traces
  * * `--printAssembly`: prints the disassembler output
  * Any task arguments should be passed following `--` like so:
  * {{{
  *   withDebug run --dumpGraphs --printAssembly -- --run myFile.enso
  *   withDebug benchOnly --showCompilations -- myBenchmark
  * }}}
  */
object WithDebugCommand {

  val truffleNoBackgroundCompilationOptions = Seq(
    "-Dpolyglot.engine.BackgroundCompilation=false"
  )

  val truffleDumpGraphsOptions = Seq(
    "-Dgraal.PrintGraph=Network",
    "-Dgraal.Dump=Truffle:2"
  )

  val truffleShowCompilationsOptions = Seq(
    "-Dpolyglot.engine.TraceCompilation=true",
    "-Dpolyglot.engine.TraceCompilationAST=true",
    "-Dpolyglot.engine.TraceInlining=true",
    "-Dpolyglot.engine.TracePerformanceWarnings=all"
  )

  val trufflePrintAssemblyOptions = Seq(
    "-XX:+UnlockDiagnosticVMOptions",
    "-XX:+PrintAssembly"
  )

  val dumpGraphsOption = "--dumpGraphs"

  val showCompilationsOptions = "--showCompilations"

  val printAssemblyOption = "--printAssembly"

  val debuggerOption = "--debugger"

  val argSeparator = "--"

  val commandName = "withDebug"

  val benchOnlyCommandName = "benchOnly"
  val runCommandName       = "run"
  val testOnlyCommandName  = "testOnly"

  private def isDebugFlag(f : String): Boolean = f match {
    case `dumpGraphsOption` => true
    case `showCompilationsOptions` => true
    case `printAssemblyOption` => true
    case `debuggerOption` => true
    case `benchOnlyCommandName` => true
    case `runCommandName` => true
    case `testOnlyCommandName` => true
    case _ => false
  }

  /** The main logic for parsing and transforming the debug flags into JVM level flags */
  def withDebug: Command =
    Command.args(commandName, "<arguments>") { (state, args) =>
      var (debugFlags, prefixedRunArgs) = args.span(isDebugFlag(_))
      System.err.println(prefixedRunArgs)
      if (prefixedRunArgs(0) == argSeparator) {
        prefixedRunArgs = prefixedRunArgs.drop(1)
      }
      System.err.println(debugFlags)
      val runArgs                       = " " + prefixedRunArgs.mkString(" ")
      System.err.println(runArgs)

      val taskKey =
        if (debugFlags.contains(benchOnlyCommandName)) BenchTasks.benchOnly
        else if (debugFlags.contains(runCommandName)) Compile / Keys.run
        else if (debugFlags.contains(testOnlyCommandName)) Test / Keys.testOnly
        else throw new IllegalArgumentException("Invalid command name.")

      val dumpGraphsOpts =
        if (debugFlags.contains(dumpGraphsOption)) truffleDumpGraphsOptions
        else Seq()
      val showCompilationsOpts =
        if (debugFlags.contains(showCompilationsOptions))
          truffleShowCompilationsOptions
        else Seq()
      val printAssemblyOpts =
        if (debugFlags.contains(printAssemblyOption))
          trufflePrintAssemblyOptions
        else Seq()
      val debuggerOpts =
        if (debugFlags.contains(debuggerOption))
          Seq(
            "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"
          )
        else Seq()
      val javaOpts: Seq[String] = Seq(
        truffleNoBackgroundCompilationOptions,
        dumpGraphsOpts,
        showCompilationsOpts,
        printAssemblyOpts,
        debuggerOpts
      ).flatten

      val extracted = Project.extract(state)
      val withJavaOpts = extracted.appendWithoutSession(
        Seq(Compile / Keys.javaOptions ++= javaOpts),
        state
      )
      Project
        .extract(withJavaOpts)
        .runInputTask(taskKey, runArgs, withJavaOpts)
      state
    }
}
