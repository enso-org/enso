package org.enso.launcher.components.runner

/**
  * Represents settings that are used to launch the runtime JVM.
  *
  * @param useSystemJVM if set, the system configured JVM is used instead of
  *                     the one managed by the launcher
  * @param jvmOptions options that should be added to the launched JVM
  */
case class JVMSettings(useSystemJVM: Boolean, jvmOptions: Seq[(String, String)])
