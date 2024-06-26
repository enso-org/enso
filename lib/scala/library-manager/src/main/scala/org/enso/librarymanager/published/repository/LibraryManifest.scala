package org.enso.librarymanager.published.repository

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.EncoderOps
import org.enso.editions.LibraryName
import org.enso.yaml.{SnakeYamlDecoder, SnakeYamlEncoder}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{MappingNode, Node}

import java.io.StringReader
import java.util
import scala.util.Try

/** The manifest file containing metadata related to a published library.
  *
  * @param archives sequence of sub-archives that the library package is
  *                 composed of
  * @param dependencies sequence of direct dependencies of the library
  * @param tagLine a short description of the library
  * @param description a longer description of the library, for the Marketplace
  */
case class LibraryManifest(
  archives: Seq[String],
  dependencies: Seq[LibraryName],
  tagLine: Option[String],
  description: Option[String]
)

object LibraryManifest {

  /** Creates an empty manifest.
    *
    * Such a manifest is invalid as at least one archive should be specified in
    * a valid manifest.
    *
    * It can however be useful as a temporary value for logic that updates or
    * creates a new manifest.
    */
  def empty: LibraryManifest = LibraryManifest(Seq.empty, Seq.empty, None, None)

  object Fields {
    val archives     = "archives"
    val dependencies = "dependencies"
    val tagLine      = "tag-line"
    val description  = "description"
  }

  implicit val decoderSnake: SnakeYamlDecoder[LibraryManifest] =
    new SnakeYamlDecoder[LibraryManifest] {
      override def decode(node: Node): Either[Throwable, LibraryManifest] =
        node match {
          case mappingNode: MappingNode =>
            val archivesDecoder = implicitly[SnakeYamlDecoder[Seq[String]]]
            val dependenciesDecoder =
              implicitly[SnakeYamlDecoder[Seq[LibraryName]]]
            val optStringDecoder = implicitly[SnakeYamlDecoder[Option[String]]]
            val kv               = mappingKV(mappingNode)
            for {
              archives <- kv
                .get(Fields.archives)
                .map(archivesDecoder.decode(_))
                .getOrElse(Right(Seq.empty))
              dependencies <- kv
                .get(Fields.dependencies)
                .map(dependenciesDecoder.decode(_))
                .getOrElse(Right(Seq.empty))
              tagLine <- kv
                .get(Fields.tagLine)
                .map(optStringDecoder.decode(_))
                .getOrElse(Right(None))
              description <- kv
                .get(Fields.description)
                .map(optStringDecoder.decode(_))
                .getOrElse(Right(None))
            } yield LibraryManifest(
              archives,
              dependencies,
              tagLine,
              description
            )
          case _ =>
            Left(new YAMLException("Unexpected edition name"))
        }
    }

  implicit val encoderSnake: SnakeYamlEncoder[LibraryManifest] =
    new SnakeYamlEncoder[LibraryManifest] {
      override def encode(value: LibraryManifest): AnyRef = {
        val archivesEncoder     = implicitly[SnakeYamlEncoder[Seq[String]]]
        val dependenciesEncoder = implicitly[SnakeYamlEncoder[Seq[LibraryName]]]
        val elements            = new util.ArrayList[(String, Object)]()
        if (value.archives.nonEmpty)
          elements.add(
            (Fields.archives, archivesEncoder.encode(value.archives))
          )
        if (value.dependencies.nonEmpty)
          elements.add(
            (
              Fields.dependencies,
              dependenciesEncoder.encode(value.dependencies)
            )
          )
        value.tagLine.foreach(v => elements.add((Fields.tagLine, v)))
        value.description.foreach(v => elements.add((Fields.description, v)))
        toMap(elements)
      }
    }

  /** A [[Decoder]] instance for parsing [[LibraryManifest]]. */
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

  /** An [[Encoder]] instance for parsing [[LibraryManifest]]. */
  implicit val encoder: Encoder[LibraryManifest] = { manifest =>
    val baseFields = Seq(
      Fields.archives     -> manifest.archives.asJson,
      Fields.dependencies -> manifest.dependencies.asJson
    )

    val allFields = baseFields ++
      manifest.tagLine.map(Fields.tagLine -> _.asJson).toSeq ++
      manifest.description.map(Fields.description -> _.asJson).toSeq

    Json.obj(allFields: _*)
  }

  /** Parser the provided string and returns a LibraryManifest, if valid */
  def fromYaml(yamlString: String): Try[LibraryManifest] = {
    val snakeYaml = new org.yaml.snakeyaml.Yaml()
    Try(snakeYaml.compose(new StringReader(yamlString))).toEither
      .flatMap(implicitly[SnakeYamlDecoder[LibraryManifest]].decode(_))
      .toTry
  }

  /** The name of the manifest file as included in the directory associated with
    * a given library in the library repository.
    */
  val filename = "manifest.yaml"
}
