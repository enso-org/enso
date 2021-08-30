package org.enso.librarymanager.dependencies

import org.enso.editions.{Editions, LibraryName, LibraryVersion}
import org.enso.librarymanager.LibraryResolver
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryProvider
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.librarymanager.published.repository.RepositoryHelper.RepositoryMethods
import org.enso.yaml.YamlHelper

import java.nio.file.Files
import scala.util.Try

class DependencyResolver(
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryProvider: PublishedLibraryProvider,
  edition: Editions.ResolvedEdition,
  preferLocalLibraries: Boolean,
  versionResolver: LibraryResolver
) {

  /** Finds all transitive dependencies of the requested library. */
  def findDependencies(libraryName: LibraryName): Try[Set[Dependency]] =
    findDependencies(libraryName, Set.empty)

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
  ): Try[Set[Dependency]] = Try {
    if (parents.contains(libraryName)) {
      Set.empty
    } else {
      val version = versionResolver
        .resolveLibraryVersion(libraryName, edition, preferLocalLibraries)
        .toTry
        .get

      version match {
        case LibraryVersion.Local =>
          // TODO [RW] can we skip deps of local? we cannot effectively find
          //  them without parsing the whole library which is costly
          Set(
            Dependency(
              libraryName,
              version,
              localLibraryProvider.findLibrary(libraryName).isDefined
            )
          )

        case publishedVersion @ LibraryVersion.Published(semver, _) =>
          val itself = Dependency(
            libraryName,
            version,
            publishedLibraryProvider
              .findCachedLibrary(libraryName, semver)
              .isDefined
          )

          val manifest = getManifest(libraryName, publishedVersion)

          Set(itself) | manifest.dependencies.toSet.flatMap(
            findDependencies(_, parents + libraryName).get
          )
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
