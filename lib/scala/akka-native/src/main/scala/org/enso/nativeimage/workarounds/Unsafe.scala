package org.enso.nativeimage.workarounds

/** Gives access to an instance of [[sun.misc.Unsafe]] which contains low-level
  * functions that are used to replace the ones that cause problems in the
  * Native Image build.
  *
  * Workaround based on
  * https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-examples/src/main/scala-2.13/com/github/plokhotnyuk/jsoniter_scala/examples/UnsafeUtils.java
  */
object Unsafe {

  /** Instance of the [[sun.misc.Unsafe]] acquired using reflection that allows
    * to run the unsafe functions needed by the workaround.
    */
  val unsafeInstance: sun.misc.Unsafe = {
    val field = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe")
    field.setAccessible(true)
    field.get(null).asInstanceOf[sun.misc.Unsafe]
  }
}
