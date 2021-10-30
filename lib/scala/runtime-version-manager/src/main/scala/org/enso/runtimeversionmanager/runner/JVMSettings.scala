package org.enso.runtimeversionmanager.runner

/** Represents settings that are used to launch the runtime JVM.
  *
  * @param javaCommandOverride the command should be used to launch the JVM
  *                           instead of the default JVM provided with the
  *                           release; it can be an absolute path to a java
  *                           executable
  * @param jvmOptions options that should be added to the launched JVM
  */
case class JVMSettings(
  javaCommandOverride: Option[JavaCommand],
  jvmOptions: Seq[(String, String)]
)

object JVMSettings {

  /** Creates settings that are used to launch the runtime JVM.
    *
    * @param useSystemJVM if set, the system configured JVM is used instead of
    *                     the one managed by the launcher
    * @param jvmOptions options that should be added to the launched JVM
    */
  def apply(
    useSystemJVM: Boolean,
    jvmOptions: Seq[(String, String)]
  ): JVMSettings =
    new JVMSettings(
      if (useSystemJVM) Some(JavaCommand.systemJavaCommand) else None,
      jvmOptions
    )

  /** Creates a default instance of [[JVMSettings]] that just use the default
    * JVM with no options overrides.
    */
  def default: JVMSettings =
    JVMSettings(useSystemJVM = false, jvmOptions = Seq())
}
