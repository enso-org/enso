package org.enso.projectmanager.infrastructure.languageserver

import java.io.PrintWriter

import scala.util.Using

/** A handle to the running Language Server child process. */
class LanguageServerProcessHandle(private val process: Process) {

  /** Requests the child process to terminate gracefully by sending the
    * termination request to its standard input stream.
    */
  def requestGracefulTermination(): Unit =
    Using(new PrintWriter(process.getOutputStream)) { writer =>
      writer.println()
    }

  /** Tries to forcibly kill the process. */
  def kill(): Unit = {
    process.destroyForcibly()
  }
}
