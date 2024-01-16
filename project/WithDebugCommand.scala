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
  val DEBUG_OPTION =
    "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y";

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

  /** The main logic for parsing and transforming the debug flags into JVM level flags */
  def withDebug: Command =
    Command.args(commandName, "<arguments>") { (state, args) =>
      val (debugFlags, prefixedRunArgs) = args.span(_ != argSeparator)
      val runArgs                       = " " + prefixedRunArgs.drop(1).mkString(" ")

      val taskKeyOpt =
        if (debugFlags.contains(benchOnlyCommandName))
          Some(BenchTasks.benchOnly)
        else if (debugFlags.contains(runCommandName)) Some(Compile / Keys.run)
        else if (debugFlags.contains(testOnlyCommandName))
          Some(Test / Keys.testOnly)
        else None

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
          Seq(DEBUG_OPTION)
        else Seq()
      val javaOpts: Seq[String] = Seq(
        truffleNoBackgroundCompilationOptions,
        dumpGraphsOpts,
        showCompilationsOpts,
        printAssemblyOpts,
        debuggerOpts
      ).flatten

      taskKeyOpt match {
        case None =>
          state.log.error(
            s"Invalid command name. Expected one of $benchOnlyCommandName, $runCommandName, or $testOnlyCommandName"
          )
          state.fail
        case Some(taskKey) =>
          val extracted = Project.extract(state)
          // Append the java options also to the Benchmark configuration, in case we run `benchOnly`
          // task.
          val Benchmark = config("bench")
          val withJavaOpts = extracted
            .appendWithoutSession(
              Seq(Compile / Keys.javaOptions ++= javaOpts),
              state
            )
            .appendWithoutSession(
              Seq(Benchmark / Keys.javaOptions ++= javaOpts),
              state
            )
          Project
            .extract(withJavaOpts)
            .runInputTask(taskKey, runArgs, withJavaOpts)
          state
      }
    }
}
