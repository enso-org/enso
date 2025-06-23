import sbt._

object Dependencies {
  // === JLine ==================================================================
  val jlineVersion = "3.26.3"
  val jlineNative = Seq(
    "org.jline" % "jline-native" % jlineVersion
  )
  val jline = Seq(
    "org.jline" % "jline-terminal"     % jlineVersion,
    "org.jline" % "jline-terminal-jni" % jlineVersion, // The terminal provider jna has been deprecated, check your configuration.
    "org.jline" % "jline-reader"       % jlineVersion
  ) ++ jlineNative
}
