package org.enso.cli.arguments

/**
  * Defines the behaviour of parsing top-level application options.
  *
  * @tparam Config type of configuration that is passed to commands
  */
sealed trait TopLevelBehavior[+Config]

object TopLevelBehavior {

  /**
    * If top-level options return a value of this class, the application should
    * continue execution. The provided value of `Config` should be passed to the
    * executed commands.
    *
    * @param withConfig the configuration that is passed to commands
    * @tparam Config type of configuration that is passed to commands
    */
  case class Continue[Config](withConfig: Config)
      extends TopLevelBehavior[Config]

  /**
    * If top-level options return a value of this class, it means that the
    * top-level options have handled the execution and commands should not be
    * parsed further. This can be useful to implement top-level options like
    * `--version`.
    *
    * @param exitCode exit code that should be returned when halting
    */
  case class Halt(exitCode: Int) extends TopLevelBehavior[Nothing]
}
