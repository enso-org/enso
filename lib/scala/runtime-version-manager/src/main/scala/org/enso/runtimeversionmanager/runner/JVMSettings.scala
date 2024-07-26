package org.enso.runtimeversionmanager.runner

/** Represents settings that are used to launch the runtime JVM.
  *
  * @param javaCommandOverride the command should be used to launch the JVM
  *                           instead of the default JVM provided with the
  *                           release; it can be an absolute path to a java
  *                           executable
  * @param jvmOptions options that should be added to the launched JVM, will be prefixed with `-D`
  * @param extraOptions extra options that should be added to the launched JVM
  */
case class JVMSettings(
  javaCommandOverride: Option[JavaCommand],
  jvmOptions: Seq[(String, String)],
  extraOptions: Seq[(String, String)]
)

object JVMSettings {

  /** Creates settings that are used to launch the runtime JVM.
    *
    * @param useSystemJVM if set, the system configured JVM is used instead of
    *                     the one managed by the launcher
    * @param jvmOptions   options that should be added to the launched JVM, will be prefixed with `-D`
    * @param extraOptions extra options that should be added to the launched JVM
    */
  def apply(
    useSystemJVM: Boolean,
    jvmOptions: Seq[(String, String)],
    extraOptions: Seq[(String, String)]
  ): JVMSettings =
    new JVMSettings(
      if (useSystemJVM) Some(JavaCommand.systemJavaCommand) else None,
      jvmOptions,
      extraOptions
    )

  // See propositions in #9475 for alternatives
  private val nioOpen: (String, String) =
    ("add-opens", "java.base/java.nio=ALL-UNNAMED")

  /** Creates a default instance of [[JVMSettings]] that just use the default
    * JVM with no options overrides.
    */
  def default: JVMSettings = {
    val jvmOptions = Seq.newBuilder[(String, String)]
    val concurrencyConfigs = Seq(
      "scala.concurrent.context.minThreads",
      "scala.concurrent.context.numThreads",
      "scala.concurrent.context.maxThreads"
    )
    concurrencyConfigs.flatMap(jvmOptionIfSet).foreach(jvmOptions.addOne)
    JVMSettings(
      useSystemJVM = false,
      jvmOptions   = jvmOptions.result(),
      extraOptions = Seq(nioOpen)
    )
  }

  private def jvmOptionIfSet(name: String): Option[(String, String)] = {
    val propertyValue = System.getProperty(name)
    if (propertyValue != null && !propertyValue.isEmpty)
      Some((name, propertyValue))
    else None
  }

}
