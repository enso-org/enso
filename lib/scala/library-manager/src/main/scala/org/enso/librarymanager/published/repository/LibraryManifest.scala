package org.enso.librarymanager.published.repository

import io.circe.Decoder
import org.enso.editions.LibraryName

case class LibraryManifest(
  archives: Seq[String],
  dependencies: Seq[LibraryName],
  tagLine: Option[String],
  description: Option[String]
)

object LibraryManifest {
  object Fields {
    val archives     = "archives"
    val dependencies = "dependencies"
    val tagLine      = "tag-line"
    val description  = "description"
  }

  implicit val decoder: Decoder[LibraryManifest] = { json =>
    for {
      archives <- json.get[Seq[String]](Fields.archives)
      dependencies <- json.getOrElse[Seq[LibraryName]](Fields.dependencies)(
        Seq()
      )
      tagLine     <- json.get[Option[String]](Fields.tagLine)
      description <- json.get[Option[String]](Fields.description)
    } yield LibraryManifest(
      archives     = archives,
      dependencies = dependencies,
      tagLine      = tagLine,
      description  = description
    )
  }
}
