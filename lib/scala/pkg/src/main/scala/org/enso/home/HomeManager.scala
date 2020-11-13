package org.enso.home

import org.enso.filesystem.FileSystem
import org.enso.pkg.{Package, PackageManager}
import java.util.stream.Stream

/** Helper class for accessing files in the language home directory.
  *
  * @param languageHomePath path to language home `component` directory.
  * @param fs file system to use in this object's operations.
  * @tparam F the type of paths to use for representing on-disk objects.
  */
class HomeManager[F](languageHomePath: F, implicit val fs: FileSystem[F]) {
  import FileSystem.Syntax

  val packageManager = new PackageManager[F]()

  val rootPath: F = languageHomePath.getParent
  val libPath: F  = rootPath.getChild("std-lib")

  /** @return a stream of packages found in the `std-lib` home directory.
    */
  def loadStdLib: Stream[Package[F]] = {
    if (libPath.exists)
      libPath.list
        .filter(_.isDirectory)
        .flatMap(path =>
          packageManager
            .fromDirectory(path)
            .map(s => Stream.of(s))
            .getOrElse(Stream.empty())
        )
    else Stream.empty()
  }
}
