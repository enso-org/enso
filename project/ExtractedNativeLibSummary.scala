import java.io.File

/** A helper class with a summary of extracted native libs and a target (thin) jar
  * @param from source jar
  * @param dynamicLibs extracted native lib(s)
  * @param thinTarget modified source jar without any dynamic libraries
  */
case class ExtractedNativeLibSummary(
  from: File,
  dynamicLibs: List[File],
  thinTarget: Option[File]
) {
  def isOutdated: Boolean = {
    !(from.exists() && thinTarget.forall(_.exists()) && thinTarget.forall(
      _.exists()
    )) ||
    dynamicLibs.exists(t => t.lastModified() < from.lastModified()) ||
    thinTarget.exists(t => t.lastModified() < from.lastModified())
  }

  def matchesTargetArtifact(file: File): Boolean = {
    dynamicLibs.contains(file) || thinTarget.contains(file)
  }
}

object ExtractedNativeLibSummary {
  import sjsonnew.{:*:, LList, LNil}
  import sbt.util.CacheImplicits._

  implicit val encode: sjsonnew.IsoLList.Aux[
    ExtractedNativeLibSummary,
    sjsonnew.LCons[File, sjsonnew.LCons[List[File], sjsonnew.LCons[Option[
      File
    ], sjsonnew.LList.LNil0]]]
  ] = LList.iso(
    { p: ExtractedNativeLibSummary =>
      ("from", p.from) :*:
      ("dynamicLibs", p.dynamicLibs) :*:
      ("thisTarget", p.thinTarget) :*:
      LNil
    },
    {
      case (_, from: File) :*:
          (_, dynamicLibs: List[File]) :*:
          (_, thinTarget: Option[File]) :*:
          LNil =>
        ExtractedNativeLibSummary(from, dynamicLibs, thinTarget)
    }
  )
}
