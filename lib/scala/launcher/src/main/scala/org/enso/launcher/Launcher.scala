package org.enso.launcher

object Launcher {
  def newProject(name: String, path: Option[String]): Unit = {
    println("TODO: project creation is not implemented")
    println((name, path))
  }

  def version(isJson: Boolean): Unit = {
    println(isJson)
  }
}
