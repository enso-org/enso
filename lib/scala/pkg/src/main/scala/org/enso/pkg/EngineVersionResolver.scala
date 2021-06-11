package org.enso.pkg

import org.enso.editions.Editions.RawEdition
import org.enso.editions.provider.EditionProvider
import org.enso.editions.{EditionResolutionError, EditionResolver, EnsoVersion}

case class EngineVersionResolver(editionProvider: EditionProvider) {
  private val editionResolver = EditionResolver(editionProvider)

  def resolveEnsoVersion(
    edition: RawEdition
  ): Either[EditionResolutionError, EnsoVersion] = {
    for {
      edition <- editionResolver.resolve(edition)
    } yield edition.getEngineVersion
  }

  def resolveEnsoVersion(
    pkg: Package[_]
  ): Either[EditionResolutionError, EnsoVersion] =
    resolveEnsoVersion(pkg.config.edition)
}
