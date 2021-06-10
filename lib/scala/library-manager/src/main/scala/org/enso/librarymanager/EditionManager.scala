package org.enso.librarymanager

import org.enso.distribution.DistributionManager
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.editions.provider.FileSystemEditionProvider
import org.enso.editions.{EditionResolver, Editions}

import java.nio.file.Path
import scala.annotation.nowarn

case class EditionManager(distributionManager: DistributionManager) {
  private val EDITIONS_DIRECTORY = "editions"

  private lazy val downloadedEditionsPath: Path =
    distributionManager.paths.dataRoot / EDITIONS_DIRECTORY
  @nowarn("msg=dead code")
  private lazy val customEditionsPath: Path = ???
  val editionProvider = FileSystemEditionProvider(
    List(customEditionsPath, downloadedEditionsPath)
  )

  val editionResolver = EditionResolver(editionProvider)

  def resolveEdition(edition: Editions.RawEdition): Editions.ResolvedEdition = {
    editionResolver.resolve(edition)
    ???
  }
}
