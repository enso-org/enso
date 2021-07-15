package org.enso.languageserver.libraries

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import org.enso.distribution.DistributionManager
import org.enso.editions.{Editions, LibraryName}
import org.enso.languageserver.libraries.LocalLibraryManagerProtocol._
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.pkg.PackageManager

import java.io.File
import java.nio.file.Files
import scala.util.{Failure, Success, Try}

class LocalLibraryManager(
  currentProjectRoot: File,
  distributionManager: DistributionManager
) extends Actor
    with LazyLogging {
  override def receive: Receive = { case request: Request =>
    request match {
      case GetMetadata(_) =>
        logger.warn(
          "Getting local library metadata is currently not implemented."
        )
        sender() ! Success(GetMetadataResponse(None, None))
      case SetMetadata(_, _, _) =>
        logger.error(
          "Setting local library metadata is currently not implemented."
        )
        sender() ! Failure(new NotImplementedError())
      case ListLocalLibraries =>
        sender() ! Failure(new NotImplementedError())
      case Create(libraryName, authors, maintainers, license) =>
        sender() ! createLibrary(libraryName, authors, maintainers, license)
      case Publish(_, _, _) =>
        logger.error("Publishing libraries is currently not implemented.")
        sender() ! Failure(new NotImplementedError())
    }
  }

  private def createLibrary(
    libraryName: LibraryName,
    authors: Seq[String],
    maintainers: Seq[String],
    license: String
  ): Try[Unit] = Try {
    // TODO [RW] modify protocol to be able to create Contact instances
    val _ = (authors, maintainers)

    // TODO [RW] make the exceptions more relevant
    val librariesRoot =
      distributionManager.paths.localLibrariesSearchPaths.headOption.getOrElse {
        throw new RuntimeException("Cannot find local library path")
      }

    val libraryPath =
      LocalLibraryProvider.resolveLibraryPath(librariesRoot, libraryName)
    if (Files.exists(libraryPath)) {
      throw new RuntimeException("Local library already exists")
    }

    PackageManager.Default.create(
      libraryPath.toFile,
      name      = libraryName.name,
      namespace = libraryName.namespace,
      edition   = findCurrentProjectEdition(),
      license   = license
    )
  }

  private def findCurrentProjectEdition(): Option[Editions.RawEdition] = {
    val pkg = PackageManager.Default.loadPackage(currentProjectRoot).get
    pkg.config.edition
  }
}
