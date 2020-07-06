package org.enso.launcher

import java.io.File

import org.enso.pkg.PackageManager

object Launcher {
  private val packageManager = PackageManager.Default

  def newProject(name: String, path: Option[String]): Unit = {
    // TODO [RW] this is not the final implementation
    val actualPath = path.getOrElse(name)
    packageManager.create(new File(actualPath), name)
    println(s"Project created in $actualPath")
  }

  def version(isJson: Boolean): Unit = {
    if (isJson) {
      println("json output not supported yet")
    } else {
      println("v0.0.0") // TODO [RW] version
    }
  }
}
