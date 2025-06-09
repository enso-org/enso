import java.io.File

/** Helper class containing a summary of extracted native libs metadata
  * @param libs extracted native libs
  */
case class AnalysisOfExtractedNativeLibs(
  libs: Map[String, ExtractedNativeLibSummary]
) {
  def first: Option[ExtractedNativeLibSummary] = {
    assert(libs.size == 1)
    libs.values.headOption
  }

  def forJar(srcJar: File): Option[ExtractedNativeLibSummary] =
    libs.values.find(_.from == srcJar)

  def isOutdated: Boolean = libs.values.exists(_.isOutdated)

  def size: Int = libs.size

  def appended(
    other: AnalysisOfExtractedNativeLibs
  ): AnalysisOfExtractedNativeLibs = {
    AnalysisOfExtractedNativeLibs(libs ++ other.libs)
  }
}
object AnalysisOfExtractedNativeLibs {
  import sjsonnew.{:*:, LList, LNil}
  import sbt.util.CacheImplicits._

  implicit val encode: sjsonnew.IsoLList.Aux[
    AnalysisOfExtractedNativeLibs,
    sjsonnew.LCons[Map[String, ExtractedNativeLibSummary], sjsonnew.LList.LNil0]
  ] = LList.iso(
    { p: AnalysisOfExtractedNativeLibs => ("libs", p.libs) :*: LNil },
    { case (_, libs: Map[String, ExtractedNativeLibSummary]) :*: LNil =>
      AnalysisOfExtractedNativeLibs(libs)
    }
  )

  def apply(
    from: File,
    dynamicLibs: List[File],
    thinTarget: Option[File]
  ): AnalysisOfExtractedNativeLibs = {
    val fromPath = from.getAbsolutePath
    AnalysisOfExtractedNativeLibs(
      Map(fromPath -> ExtractedNativeLibSummary(from, dynamicLibs, thinTarget))
    )
  }
}
