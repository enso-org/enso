package org.enso.launcher.components.runner

case class JVMSettings(useSystemJVM: Boolean, jvmOptions: Seq[(String, String)])
