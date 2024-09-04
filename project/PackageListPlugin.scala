import sbt.*
import sbt.Keys.*
import sbt.AutoPlugin

import java.io.File
import java.nio.file.Path
import scala.collection.mutable

/** sbt plugin for gathering set of packages contained in the current project.
  * The packages are gathered via recursive directory listing.
  */
object PackageListPlugin extends AutoPlugin {
  object autoImport {
    val packages = taskKey[Set[String]](
      "Set of packages in the source directory"
    )
  }

  import autoImport._

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    Compile / packages := listOfPackages(
      ScopeFilter(configurations = inConfigurations(Compile))
    ).value,
    Test / packages := listOfPackages(
      ScopeFilter(configurations = inConfigurations(Test))
    ).value
  )

  private def listOfPackages(
    scopeFilter: ScopeFilter
  ): Def.Initialize[Task[Set[String]]] = Def.task {
    val javaSrcDir  = javaSource.all(scopeFilter).value.head
    val scalaSrcDir = scalaSource.all(scopeFilter).value.head
    listOfPackages(List(javaSrcDir, scalaSrcDir))
  }

  /** From the given source directory, iterates all files and gathers the list of packages.
    * Implemented with recursive listing of directories.
    * @param srcDir The source directory, either Java or Scala.
    * @return Set of packages found in the source directory.
    */
  private def listOfPackages(srcDirs: List[File]): Set[String] = {
    val pkgs = mutable.HashSet.empty[String]
    srcDirs.foreach { srcDir =>
      if (srcDir.isDirectory) {
        listOfPackages(srcDir.toPath, srcDir, pkgs)
      }
    }
    pkgs.toSet
  }

  private def listOfPackages(
    rootDir: Path,
    curDir: File,
    pkgs: mutable.Set[String]
  ): Unit = {
    assert(curDir.isDirectory)
    val files = curDir.listFiles
    for (file <- files) {
      if (file.isFile) {
        val fPath        = file.toPath
        val relativePath = rootDir.relativize(fPath)
        val pkgPart      = relativePath.subpath(0, relativePath.getNameCount - 1)
        val pkg          = pkgPart.toString.replace(File.separator, ".")
        pkgs.add(pkg)
      } else if (file.isDirectory) {
        listOfPackages(rootDir, file, pkgs)
      }
    }

  }
}
