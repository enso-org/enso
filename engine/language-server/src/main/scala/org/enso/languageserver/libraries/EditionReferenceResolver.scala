package org.enso.languageserver.libraries

import org.enso.editions.{DefaultEdition, Editions}
import org.enso.editions.provider.EditionProvider
import org.enso.pkg.Package

import java.io.File
import scala.util.Try

class EditionReferenceResolver(
  projectPackage: Package[File],
  editionProvider: EditionProvider
) {
  def resolveReference(
    editionReference: EditionReference
  ): Try[Editions.RawEdition] = editionReference match {
    case EditionReference.NamedEdition(editionName) =>
      editionProvider.findEditionForName(editionName)
    case EditionReference.CurrentProjectEdition =>
      Try {
        projectPackage.config.edition.getOrElse {
          // TODO [RW] default edition from config (#1864)
          DefaultEdition.getDefaultEdition
        }
      }
  }
}
