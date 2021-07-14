package org.enso.languageserver.libraries

import akka.actor.Actor
import org.enso.distribution.{DistributionManager, EditionManager, LanguageHome}
import org.enso.editions.DefaultEdition
import org.enso.languageserver.libraries.EditionManagerProtocol._
import org.enso.pkg.PackageManager

import java.nio.file.Path
import scala.annotation.unused

class EditionHelper(
  @unused projectRoot: Path,
  languageHome: LanguageHome,
  @unused distributionManager: DistributionManager
) {
  private val projectPackage =
    PackageManager.Default.loadPackage(projectRoot.toFile).get

  // TODO get language home
  private val editionManager = new EditionManager(
    languageHome.editions :: distributionManager.paths.editionSearchPaths.toList
  )

  // TODO [RW] get the default edition from config (#1864)
  private val rawProjectEdition =
    projectPackage.config.edition.getOrElse(DefaultEdition.getDefaultEdition)

  override def receive: Receive = { case request: Request =>
    request match {
      case ListAvailable(update)                             =>
      case Resolve(editionReference)                         =>
      case SetParentEdition(newEditionName)                  =>
      case SetLocalLibrariesPreference(preferLocalLibraries) =>
      case ListDefinedLibraries(editionReference)            =>
    }
  }
}
