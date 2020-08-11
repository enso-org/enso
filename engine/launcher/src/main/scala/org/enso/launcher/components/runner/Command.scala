package org.enso.launcher.components.runner

import org.enso.launcher.Logger

import scala.sys.process.Process

case class Command(command: Seq[String], extraEnv: Seq[(String, String)]) {
  def run(): Int = {
    Logger.debug(s"Executing $toString")
    val process =
      Process(command, None, extraEnv: _*).run(connectInput = true)
    process.exitValue()
  }

  override def toString: String = {
    def escapeQuotes(string: String): String =
      "\"" + string.replace("\"", "\\\"") + "\""
    val environmentDescription =
      extraEnv.map(v => s"${v._1}=${escapeQuotes(v._2)} ").mkString
    val commandDescription = command.map(escapeQuotes).mkString(" ")
    environmentDescription + commandDescription
  }
}
