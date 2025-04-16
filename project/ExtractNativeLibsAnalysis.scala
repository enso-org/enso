import java.io.File

case class ExtractNativeLibsAnalysis(libs: List[ExtractNativeLibAnalysis]) {
  def first: Option[ExtractNativeLibAnalysis] = {
    assert(libs.size == 1)
    libs.headOption
  }
  def forJar(srcJar: File): Option[ExtractNativeLibAnalysis] =
    libs.find(_.from == srcJar)

  def isOutdated(): Boolean = libs.exists(_.isOutdated())
}
object ExtractNativeLibsAnalysis {
  import sjsonnew.{:*:, LList, LNil}
  import sbt.util.CacheImplicits._

  implicit val encode
    : sjsonnew.IsoLList.Aux[ExtractNativeLibsAnalysis, sjsonnew.LCons[List[
      ExtractNativeLibAnalysis
    ], sjsonnew.LList.LNil0]] = LList.iso(
    { p: ExtractNativeLibsAnalysis => ("libs", p.libs) :*: LNil },
    { case (_, from: List[ExtractNativeLibAnalysis]) :*: LNil =>
      ExtractNativeLibsAnalysis(from)
    }
  )

  def apply(
    from: File,
    dynamicLibs: List[File],
    thinTarget: Option[File]
  ): ExtractNativeLibsAnalysis =
    ExtractNativeLibsAnalysis(
      ExtractNativeLibAnalysis(from, dynamicLibs, thinTarget) :: Nil
    )
}

case class ExtractNativeLibAnalysis(
  from: File,
  dynamicLibs: List[File],
  thinTarget: Option[File]
) {
  def isOutdated(): Boolean = {
    !(from.exists() && thinTarget.forall(_.exists()) && thinTarget.forall(
      _.exists()
    )) ||
    dynamicLibs.exists(t => t.lastModified() < from.lastModified()) ||
    thinTarget.exists(t => t.lastModified() < from.lastModified())
  }

  def matchesTargetJar(file: File): Boolean = {
    dynamicLibs.contains(file) || thinTarget.contains(file)
  }
}

object ExtractNativeLibAnalysis {
  import sjsonnew.{:*:, LList, LNil}
  import sbt.util.CacheImplicits._

  implicit val encode: sjsonnew.IsoLList.Aux[
    ExtractNativeLibAnalysis,
    sjsonnew.LCons[File, sjsonnew.LCons[List[File], sjsonnew.LCons[Option[
      File
    ], sjsonnew.LList.LNil0]]]
  ] = LList.iso(
    { p: ExtractNativeLibAnalysis =>
      ("from", p.from) :*: ("dynamicLibs", p.dynamicLibs) :*: (
        "thisTarget",
        p.thinTarget
      ) :*: LNil
    },
    {
      case (_, from: File) :*: (_, dynamicLibs: List[File]) :*: (
            _,
            thinTarget: Option[File]
          ) :*: LNil =>
        ExtractNativeLibAnalysis(from, dynamicLibs, thinTarget)
    }
  )
}
