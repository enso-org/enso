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
    "-Dgraal.TruffleBackgroundCompilation=false"
  )

  val truffleDumpGraphsOptions = Seq(
    "-Dgraal.PrintGraph=Network",
    "-Dgraal.Dump=Truffle:2"
  )

  val truffleShowCompilationsOptions = Seq(
    "-Dgraal.TraceTruffleCompilation=true",
    "-Dgraal.TraceTruffleCompilationCallTree=true",
    "-Dgraal.TraceTruffleInlining=true",
    "-Dgraal.TraceTrufflePerformanceWarnings=true"
  )

  val trufflePrintAssemblyOptions = Seq(
    "-XX:CompileCommand=print,*OptimizedCallTarget.callRoot",
    "-XX:CompileCommand=exclude,*OptimizedCallTarget.callRoot"
  )

  val dumpGraphsOption = "--dumpGraphs"

  val showCompilationsOptions = "--showCompilations"

  val printAssemblyOption = "--printAssembly"

  val argSeparator = "--"

  val commandName = "withDebug"

  val benchOnlyCommandName = "benchOnly"
  val runCommandName       = "run"

  /** The main logic for parsing and transforming the debug flags into JVM level flags */
  def withDebug: Command = Command.args(commandName, "<arguments>") {
    (state, args) =>
      val (debugFlags, prefixedRunArgs) = args.span(_ != argSeparator)
      val runArgs                       = " " + prefixedRunArgs.drop(1).mkString(" ")

      val taskKey =
        if (debugFlags.contains(benchOnlyCommandName)) BenchTasks.benchOnly
        else if (debugFlags.contains(runCommandName)) Compile / Keys.run
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
      val javaOpts: Seq[String] = Seq(
        truffleNoBackgroundCompilationOptions,
        dumpGraphsOpts,
        showCompilationsOpts,
        printAssemblyOpts
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
