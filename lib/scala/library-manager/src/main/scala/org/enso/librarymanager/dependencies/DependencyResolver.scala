package org.enso.librarymanager.dependencies

import org.enso.editions.{Editions, LibraryName, LibraryVersion}
import org.enso.librarymanager.LibraryResolver
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.librarymanager.published.repository.RepositoryHelper.RepositoryMethods
import org.enso.libraryupload.DependencyExtractor
import org.enso.pkg.PackageManager
import org.enso.yaml.YamlHelper

import java.io.File
import java.nio.file.Files
import scala.util.Try

/** A helper class that allows to find all transitive dependencies of a specific
  * library.
  */
class DependencyResolver(
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryProvider: PublishedLibraryCache,
  edition: Editions.ResolvedEdition,
  preferLocalLibraries: Boolean,
  versionResolver: LibraryResolver,
  dependencyExtractor: DependencyExtractor[File]
) {

  /** Finds all transitive dependencies of the requested library.
    *
    * The resulting set of dependencies also includes the library itself.
    */
  def findDependencies(libraryName: LibraryName): Try[Set[Dependency]] =
    Try(findDependencies(libraryName, Set.empty))

  /** A helper function to discover all transitive dependencies, avoiding
    * looping on cycles.
    *
    * It keeps track of libraries that already have been 'visited' and if a
    * library that was already visited is queried again (which is caused by
    * import cycles), it returns an empty set - that is because since this
    * library was already visited, it and its dependencies must have already
    * been accounted for in one of the parent calls, so we can return this empty
    * set at this point, because later on these dependencies will be included.
    * If we didn't quit early here, we would get an infinite loop due to the
    * dependency cycle.
    */
  private def findDependencies(
    libraryName: LibraryName,
    parents: Set[LibraryName]
  ): Set[Dependency] = {
    if (parents.contains(libraryName)) {
      Set.empty
    } else {
      val version = versionResolver
        .resolveLibraryVersion(libraryName, edition, preferLocalLibraries)
        .toTry
        .get

      version match {
        case LibraryVersion.Local =>
          val libraryPath = localLibraryProvider.findLibrary(libraryName)
          val libraryPackage = libraryPath.map(path =>
            PackageManager.Default.loadPackage(path.toFile).get
          )

          val dependencies = libraryPackage match {
            case Some(pkg) =>
              dependencyExtractor.findDependencies(pkg)
            case None =>
              Set.empty
          }

          val itself = Dependency(libraryName, version, libraryPath.isDefined)

          dependencies.flatMap(
            findDependencies(_, parents + libraryName)
          ) + itself

        case publishedVersion @ LibraryVersion.Published(semver, _) =>
          val itself = Dependency(
            libraryName,
            version,
            publishedLibraryProvider.isLibraryCached(libraryName, semver)
          )

          val manifest = getManifest(libraryName, publishedVersion)

          manifest.dependencies.toSet.flatMap { name: LibraryName =>
            findDependencies(name, parents + libraryName)
          } + itself
      }
    }
  }

  private def getManifest(
    libraryName: LibraryName,
    version: LibraryVersion.Published
  ): LibraryManifest = {
    val cachedManifest = publishedLibraryProvider
      .findCachedLibrary(libraryName, version.version)
      .flatMap { libraryPath =>
        val manifestPath = libraryPath.resolve(LibraryManifest.filename)
        if (Files.exists(manifestPath))
          YamlHelper.load[LibraryManifest](manifestPath).toOption
        else None
      }
    cachedManifest.getOrElse {
      version.repository
        .accessLibrary(libraryName, version.version)
        .downloadManifest()
        .force()
    }
  }
}
