package org.enso.compiler

import com.oracle.truffle.api.TruffleFile
import org.enso.editions.LibraryName
import org.enso.compiler.context.CompilerContext
import org.enso.pkg.{ComponentGroups, Package}

import scala.collection.immutable.ListSet
import scala.jdk.OptionConverters.RichOption

/** Manages loaded packages and modules. */
trait PackageRepository {

  /** Initialize the package repository.
    *
    * @return `Right` if the package repository initialized successfully,
    * and a `Left` containing an error otherwise.
    */
  def initialize(): Either[PackageRepository.Error, Unit]

  /** Informs the repository that it should populate the top scope with modules
    * belonging to a given package.
    *
    * @param libraryName the name of the library that should be loaded
    * @return `Right(())` if the package was already loaded or successfully
    *         downloaded. A `Left` containing an error otherwise.
    */
  def ensurePackageIsLoaded(
    libraryName: LibraryName
  ): Either[PackageRepository.Error, Unit]

  /** Checks if the library has already been loaded */
  def isPackageLoaded(libraryName: LibraryName): Boolean

  /** Get a sequence of currently loaded packages. */
  def getLoadedPackages: Seq[Package[TruffleFile]]

  /** Get a sequence of currently loaded packages. */
  def getLoadedPackagesJava: java.lang.Iterable[Package[TruffleFile]]

  /** Get a sequence of currently loaded modules. */
  def getLoadedModules: Seq[CompilerContext.Module]

  /** Get the mapping from qualified module names (equivalent to
    * [[QualifiedName.toString]]) to modules.
    *
    * This map may be updated concurrently.
    */
  def getModuleMap: PackageRepository.ModuleMap

  /** Gets a frozen form of the module map that cannot be updated concurrently.
    */
  def freezeModuleMap: PackageRepository.FrozenModuleMap

  /** Get the loaded library components. */
  def getComponents: PackageRepository.ComponentsMap

  /** Modules required for compilation after loading the component groups. */
  def getPendingModules: ListSet[CompilerContext.Module]

  /** Get a loaded module by its qualified name. */
  def getLoadedModule(qualifiedName: String): Option[CompilerContext.Module]

  /** Register the main project package. */
  def registerMainProjectPackage(
    libraryName: LibraryName,
    pkg: Package[TruffleFile]
  ): Unit

  /** @return the main project package, if it exists
    */
  def getMainProjectPackage: Option[Package[TruffleFile]]

  /** Register a single module, outside of any packages or part of an already
    * loaded package, that has been created manually during runtime.
    */
  def registerModuleCreatedInRuntime(module: CompilerContext.Module): Unit

  /** Register an empty package with the given name. Used for populating artificially,
    * e.g. in tests.
    *
    * @param namespace the namespace of the created package.
    * @param name the name of the created package.
    */
  def registerSyntheticPackage(namespace: String, name: String): Unit

  /** Removes a module with the given name from the list of loaded modules. */
  def deregisterModule(qualifiedName: String): Unit

  /** Modifies package and module names to reflect the project name change. */
  def renameProject(namespace: String, oldName: String, newName: String): Unit

  /** Checks if any library with a given namespace has been registered */
  def isNamespaceRegistered(namespace: String): Boolean

  /** Returns a package directory corresponding to the requested library */
  def getPackageForLibrary(lib: LibraryName): Option[Package[TruffleFile]]

  /** Returns a package directory corresponding to the requested library */
  def getPackageForLibraryJava(
    libraryName: LibraryName
  ): java.util.Optional[Package[TruffleFile]] =
    getPackageForLibrary(libraryName).toJava

  /** Returns all loaded modules of the requested library */
  def getModulesForLibrary(
    libraryName: LibraryName
  ): List[CompilerContext.Module]

  /** Returns a deserialized bindings map for the whole library, if available */
  def getLibraryBindings(
    libraryName: LibraryName,
    serializationManager: SerializationManager
  ): Option[ImportExportCache.CachedBindings]

}

object PackageRepository {

  type ModuleName      = String
  type ModuleMap       = collection.concurrent.Map[ModuleName, CompilerContext.Module]
  type FrozenModuleMap = Map[ModuleName, CompilerContext.Module]
  type ComponentsMap   = Map[LibraryName, ComponentGroups]

  /** A trait representing errors reported by this system */
  sealed trait Error

  object Error {

    /** Indicates that a resolution error has happened, for example the package
      * was not defined in the selected edition.
      */
    case class PackageCouldNotBeResolved(cause: Throwable) extends Error {
      override def toString: String =
        s"The package could not be resolved: ${cause.getMessage}"
    }

    /** Indicates that the package was missing and a download was attempted, but
      * it failed - for example due to connectivity problems or just because the
      * package did not exist in the repository.
      */
    case class PackageDownloadFailed(cause: Throwable) extends Error {
      override def toString: String =
        s"The package download has failed: ${cause.getMessage}"
    }

    /** Indicates that the package was already present in the cache (or within
      * local packages), but it could not be loaded, possibly to a filesystem
      * error or insufficient permissions.
      */
    case class PackageLoadingError(cause: String) extends Error {
      override def toString: String =
        s"The package could not be loaded: $cause"
    }
  }

}
