package org.enso.compiler

import com.oracle.truffle.api.TruffleFile
import com.typesafe.scalalogging.Logger
import org.enso.distribution.DistributionManager
import org.enso.editions.LibraryName
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.util.TruffleFileSystem
import org.enso.interpreter.runtime.{Context, Module}
import org.enso.librarymanager.ResolvingLibraryProvider
import org.enso.librarymanager.local.DefaultLocalLibraryProvider
import org.enso.logger.masking.MaskedPath
import org.enso.pkg.{Package, PackageManager, QualifiedName}

import java.nio.file.Path
import scala.util.Try

trait PackageRepository {

  /** Informs the repository that it should populate the top scope with modules
    * belonging to a given package.
    *
    * @param namespace the namespace of the package.
    * @param name the package name.
    * @return `Right(())` if the package was already loaded or successfully
    *         downloaded. A `Left` containing an error otherwise.
    */
  def ensurePackageIsLoaded(
    libraryName: LibraryName
  ): Either[PackageRepository.Error, Unit]

  def getLoadedPackages(): Seq[Package[TruffleFile]]

  def getLoadedModules(): Seq[Module]

  def getLoadedModule(qualifiedName: String): Option[Module]

  def registerPackage(libraryName: LibraryName, pkg: Package[TruffleFile]): Unit

  def registerMainProjectPackage(
    libraryName: LibraryName,
    pkg: Package[TruffleFile]
  ): Unit

  def registerModuleCreatedInRuntime(module: Module): Unit

  def deregisterModule(qualifiedName: String): Unit

  def renameProject(namespace: String, oldName: String, newName: String): Unit
}

object PackageRepository {

  /** A trait representing errors reported by this system */
  sealed trait Error

  object Error {
    case class PackageCouldNotBeResolved(cause: Throwable) extends Error {
      override def toString: String =
        s"The package could not be resolved: ${cause.getMessage}"
    }
    case class PackageDownloadFailed(cause: Throwable) extends Error {
      override def toString: String =
        s"The package download has failed: ${cause.getMessage}"
    }
    case class PackageLoadingError(cause: String) extends Error {
      override def toString: String =
        s"The package could not be loaded: $cause"
    }
  }

  class Default(
    libraryProvider: ResolvingLibraryProvider,
    context: Context,
    builtins: Builtins
  ) extends PackageRepository {

    private val logger         = Logger[Default]
    implicit private val fs    = new TruffleFileSystem
    private val packageManager = new PackageManager[TruffleFile]

    /** The mapping containing all loaded packages.
      *
      * It should be modified only from within synchronized sections, but it may
      * be always read. Thus elements should be added to this mapping only after
      * all library loading bookkeeping has been finished - so that if other,
      * unsynchronized threads read this map, every element it contains is
      * already fully processed.
      */
    val loadedPackages
      : collection.concurrent.Map[LibraryName, Option[Package[TruffleFile]]] = {
      val builtinsName = LibraryName(Builtins.NAMESPACE, Builtins.PACKAGE_NAME)
      collection.concurrent.TrieMap(builtinsName -> None)
    }

    val loadedModules: collection.concurrent.Map[String, Module] =
      collection.concurrent.TrieMap(Builtins.MODULE_NAME -> builtins.getModule)

    override def registerPackage(
      libraryName: LibraryName,
      pkg: Package[TruffleFile]
    ): Unit = registerPackageInternal(libraryName, pkg, isLibrary = true)

    def registerMainProjectPackage(
      libraryName: LibraryName,
      pkg: Package[TruffleFile]
    ): Unit = registerPackageInternal(libraryName, pkg, isLibrary = false)

    private def registerPackageInternal(
      libraryName: LibraryName,
      pkg: Package[TruffleFile],
      isLibrary: Boolean
    ): Unit = {
      val extensions = pkg.listPolyglotExtensions("java")
      extensions.foreach(context.getEnvironment.addToHostClassPath)

      pkg.listSources
        .map { srcFile =>
          new Module(srcFile.qualifiedName, pkg, srcFile.file)
        }
        .foreach(registerModule)

      if (isLibrary) {
        context
        // TODO notify the content root manager
        // TODO [RW, MK, DB] how to pass the Endpoint to here?
      }

      loadedPackages.put(libraryName, Some(pkg))
    }

    /** This package modifies the [[loadedPackages]], so it should be only
      * called from within synchronized sections.
      */
    private def loadPackage(
      libraryName: LibraryName,
      root: Path
    ): Either[Error, Unit] = Try {
      logger.debug(
        s"Loading library $libraryName from " +
        s"[${MaskedPath(root).applyMasking()}]."
      )
      val rootFile = context.getEnvironment.getInternalTruffleFile(
        root.toAbsolutePath.normalize.toString
      )
      val pkg = packageManager.loadPackage(rootFile).get
      registerPackage(libraryName, pkg)
    }.toEither.left.map { error => Error.PackageLoadingError(error.getMessage) }

    override def ensurePackageIsLoaded(
      libraryName: LibraryName
    ): Either[Error, Unit] =
      if (loadedPackages.contains(libraryName)) Right(())
      else {
        logger.trace(s"Resolving library $libraryName.")
        val libraryPath = libraryProvider.findLibrary(libraryName)
        this.synchronized {
          // We check again inside of the monitor, in case that some other
          // thread has just added this library.
          if (loadedPackages.contains(libraryName)) Right(())
          else
            libraryPath
              .flatMap(loadPackage(libraryName, _))
              .left
              .map {
                case ResolvingLibraryProvider.Error.NotResolved(details) =>
                  Error.PackageCouldNotBeResolved(details)
                case ResolvingLibraryProvider.Error.DownloadFailed(reason) =>
                  Error.PackageDownloadFailed(reason)
                case ResolvingLibraryProvider.Error.RequestedLocalLibraryDoesNotExist =>
                  Error.PackageLoadingError(
                    "The local library has not been found on the local " +
                    "libraries search paths."
                  )
              }
        }
      }

    override def getLoadedModules(): Seq[Module] = loadedModules.values.toSeq

    override def getLoadedPackages(): Seq[Package[TruffleFile]] =
      loadedPackages.values.toSeq.flatten

    override def getLoadedModule(qualifiedName: String): Option[Module] =
      loadedModules.get(qualifiedName)

    override def registerModuleCreatedInRuntime(module: Module): Unit =
      registerModule(module)

    private def registerModule(module: Module): Unit =
      loadedModules.put(module.getName.toString, module)

    override def deregisterModule(qualifiedName: String): Unit =
      loadedModules.remove(qualifiedName)

    override def renameProject(
      namespace: String,
      oldName: String,
      newName: String
    ): Unit = this.synchronized {
      renamePackages(namespace, oldName, newName)
      renameModules(namespace, oldName, newName)
    }

    private def renamePackages(
      namespace: String,
      oldName: String,
      newName: String
    ): Unit = {
      val toChange = loadedPackages.toSeq.filter { case (name, _) =>
        name.prefix == namespace && name.name == oldName
      }

      for ((key, _) <- toChange) {
        loadedPackages.remove(key)
      }

      for ((key, pkgOption) <- toChange) {
        val newPkg = pkgOption.map(_.setPackageName(newName))
        val newKey = key.copy(name = newName)
        loadedPackages.put(newKey, newPkg)
      }
    }

    private def renameModules(
      namespace: String,
      oldName: String,
      newName: String
    ): Unit = {
      val separator: String = QualifiedName.separator
      val keys = loadedModules.keySet.filter(name =>
        name.startsWith(namespace + separator + oldName + separator)
      )

      for {
        key    <- keys
        module <- loadedModules.remove(key)
      } {
        module.renameProject(newName)
        loadedModules.put(module.getName.toString, module)
      }
    }
  }

  private class TemporaryLocalProvider(distributionManager: DistributionManager)
      extends ResolvingLibraryProvider {

    private val localRepo = new DefaultLocalLibraryProvider(distributionManager)

    override def findLibrary(
      name: LibraryName
    ): Either[ResolvingLibraryProvider.Error, Path] =
      localRepo.findLibrary(name).toRight {
        ResolvingLibraryProvider.Error.RequestedLocalLibraryDoesNotExist
      }
  }

  def makeLegacyRepository(
    distributionManager: DistributionManager,
    context: Context,
    builtins: Builtins
  ): PackageRepository = new Default(
    new TemporaryLocalProvider(distributionManager),
    context,
    builtins
  )
}
