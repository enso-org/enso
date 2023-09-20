package org.enso.editions.updater

import org.enso.editions.Editions
import org.enso.editions.provider.{
  EditionLoadingError,
  EditionNotFound,
  EditionProvider,
  FileSystemEditionProvider
}

import java.nio.file.Path

/** An [[EditionProvider]] implementation that works like
  * [[FileSystemEditionProvider]] but the first time that an edition could not
  * be found, it downloads the latest editions from the sources, attempting to
  * locate the missing edition there.
  */
class UpdatingEditionProvider(
  searchPaths: List[Path],
  cachePath: Path,
  sources: Seq[String]
) extends EditionProvider {

  private val actualSearchPaths = (searchPaths ++ List(cachePath)).distinct

  private val provider = new FileSystemEditionProvider(actualSearchPaths)
  private val updater  = new EditionUpdater(cachePath, sources)

  /** @inheritdoc */
  override def findEditionForName(
    name: String
  ): Either[EditionLoadingError, Editions.Raw.Edition] =
    provider.findEditionForName(name) match {
      case Left(EditionNotFound(_)) =>
        updater.downloadEdition(name)
        provider.findEditionForName(name)
      case Left(otherError) => Left(otherError)
      case Right(value)     => Right(value)
    }

  /** Finds all editions available on the [[searchPaths]]. */
  override def findAvailableEditions(): Seq[String] =
    provider.findAvailableEditions()

  /** Finds all available editions, performing an update if asked to. */
  def findAvailableEditions(update: Boolean): Seq[String] = {
    if (update) {
      updater.updateEditions()
    }

    provider.findAvailableEditions()
  }
}
