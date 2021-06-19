package org.enso.editions.repository

import io.circe._

case class Manifest(editions: Seq[String])

object Manifest {
  object Fields {
    val editions = "editions"
  }

  implicit val decoder: Decoder[Manifest] = { json =>
    for {
      editions <- json.get[Seq[String]](Fields.editions)
    } yield Manifest(editions)
  }
}
