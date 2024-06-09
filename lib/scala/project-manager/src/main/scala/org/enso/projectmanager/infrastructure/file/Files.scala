package org.enso.projectmanager.infrastructure.file

import java.io.File

object Files {

  private val HOME_DIR = "~"

  /** Resolve the path and return an absolute file.
    *
    * @param path the path
    * @return the resolved absolute path
    */
  def getAbsoluteFile(path: String): File = {
    val resolvedPath =
      if (path.startsWith(HOME_DIR + File.separator)) {
        System.getProperty("user.home") + path.substring(1)
      } else {
        path
      }
    new File(resolvedPath).getAbsoluteFile
  }

}
