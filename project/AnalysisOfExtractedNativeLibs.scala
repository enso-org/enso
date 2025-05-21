import java.io.File

/** Helper class containing a summary of extracted native libs metadata
  * @param libs extracted native libs
  */
case class AnalysisOfExtractedNativeLibs(
  libs: List[ExtractedNativeLibSummary]
) {
  def first: Option[ExtractedNativeLibSummary] = {
    assert(libs.size == 1)
    libs.headOption
  }
  def forJar(srcJar: File): Option[ExtractedNativeLibSummary] =
    libs.find(_.from == srcJar)

  def isOutdated: Boolean = libs.exists(_.isOutdated)
}
object AnalysisOfExtractedNativeLibs {
  import sjsonnew.{:*:, LList, LNil}
  import sbt.util.CacheImplicits._

  implicit val encode
    : sjsonnew.IsoLList.Aux[AnalysisOfExtractedNativeLibs, sjsonnew.LCons[List[
      ExtractedNativeLibSummary
    ], sjsonnew.LList.LNil0]] = LList.iso(
    { p: AnalysisOfExtractedNativeLibs => ("libs", p.libs) :*: LNil },
    { case (_, from: List[ExtractedNativeLibSummary]) :*: LNil =>
      AnalysisOfExtractedNativeLibs(from)
    }
  )

  def apply(
    from: File,
    dynamicLibs: List[File],
    thinTarget: Option[File]
  ): AnalysisOfExtractedNativeLibs =
    AnalysisOfExtractedNativeLibs(
      ExtractedNativeLibSummary(from, dynamicLibs, thinTarget) :: Nil
    )
}
