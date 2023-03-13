package org.enso.languageserver.filemanager

import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}
import org.enso.logger.masking.{MaskedPath, ToLogString}

import java.io.File
import java.util.UUID

/** A representation of a content root. */
sealed trait ContentRoot {

  /** The unique identifier of the content root. */
  def id: UUID
}

object ContentRoot {

  /** A filesystem root.
    *
    * @param id the unique identifier of the content root
    * @param path absolute path of the content root
    */
  case class FileSystemRoot(override val id: UUID, path: String)
      extends ContentRoot

  /** A root representing user's home on the filesystem.
    *
    * @param id the unique identifier of the content root
    */
  case class Home(override val id: UUID) extends ContentRoot

  /** Main project root.
    *
    * @param id the unique identifier of the content root
    */
  case class Project(override val id: UUID) extends ContentRoot

  /** A root of an imported library.
    *
    * @param id the unique identifier of the content root
    * @param namespace namespace of the library
    * @param name name of the library
    * @param version version of the library
    */
  case class Library(
    override val id: UUID,
    namespace: String,
    name: String,
    version: String
  ) extends ContentRoot

  /** A custom root, currently not used.
    *
    * @param id the unique identifier of the content root
    */
  case class Custom(override val id: UUID) extends ContentRoot

  private object CodecField {
    val Id        = "id"
    val Type      = "type"
    val Namespace = "namespace"
    val Name      = "name"
    val Version   = "version"
    val Path      = "path"
  }

  private object CodecType {
    val FileSystemRoot = "FileSystemRoot"
    val Home           = "Home"
    val Project        = "Project"
    val Library        = "Library"
    val Custom         = "Custom"
  }

  /** An [[Encoder]] instance for [[ContentRoot]]. */
  implicit val encoder: Encoder[ContentRoot] = {
    case FileSystemRoot(id, path) =>
      Json.obj(
        CodecField.Type -> CodecType.FileSystemRoot.asJson,
        CodecField.Id   -> id.asJson,
        CodecField.Path -> path.asJson
      )
    case Home(id) =>
      Json.obj(
        CodecField.Type -> CodecType.Home.asJson,
        CodecField.Id   -> id.asJson
      )
    case Project(id) =>
      Json.obj(
        CodecField.Type -> CodecType.Project.asJson,
        CodecField.Id   -> id.asJson
      )
    case Library(id, namespace, name, version) =>
      Json.obj(
        CodecField.Type      -> CodecType.Library.asJson,
        CodecField.Id        -> id.asJson,
        CodecField.Namespace -> namespace.asJson,
        CodecField.Name      -> name.asJson,
        CodecField.Version   -> version.asJson
      )
    case Custom(id) =>
      Json.obj(
        CodecField.Type -> CodecType.Custom.asJson,
        CodecField.Id   -> id.asJson
      )
  }
}

/** A representation of a content root with a file that represents its
  * filesystem location
  *
  * @param contentRoot the raw content root
  * @param file the file on the filesystem that is the content root
  */
case class ContentRootWithFile(
  contentRoot: ContentRoot,
  file: File
) extends ToLogString {

  /** The unique identifier of the content root. */
  def id: UUID = contentRoot.id

  /** Convert this to a content root for use in the protocol.
    *
    * @return a protocol content root
    */
  def toContentRoot: ContentRoot = contentRoot

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String =
    s"ContentRootWithFile(contentRoot=$contentRoot," +
    s"file=${MaskedPath(file.toPath).toLogString(shouldMask)}" +
    ")"
}
