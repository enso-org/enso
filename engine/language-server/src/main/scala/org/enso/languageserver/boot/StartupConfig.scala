package org.enso.languageserver.boot

/** Startup configuration.
  *
  * @param skipGraalVMUpdater indicates if the check and installation of GraalVM should be performed
  */
case class StartupConfig(skipGraalVMUpdater: Boolean = false)
