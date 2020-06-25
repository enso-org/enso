package org.enso.languageserver.runtime

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.searcher.Suggestion

object SearchProtocol {

  sealed trait SuggestionsDatabaseUpdate
  object SuggestionsDatabaseUpdate {

    /** Create or replace the database entry.
      *
      * @param id suggestion id
      * @param suggestion the new suggestion
      */
    case class Add(id: Long, suggestion: Suggestion)
        extends SuggestionsDatabaseUpdate

    /** Remove the database entry.
      *
      * @param id the suggestion id
      */
    case class Remove(id: Long) extends SuggestionsDatabaseUpdate

    /** Modify the database entry.
      *
      * @param id the suggestion id
      * @param name the new suggestion name
      * @param arguments the new suggestion arguments
      * @param selfType the new self type of the suggestion
      * @param returnType the new return type of the suggestion
      * @param documentation the new documentation string
      * @param scope the suggestion scope
      */
    case class Modify(
      id: Long,
      name: Option[String],
      arguments: Option[Seq[Suggestion.Argument]],
      selfType: Option[String],
      returnType: Option[String],
      documentation: Option[String],
      scope: Option[Suggestion.Scope]
    ) extends SuggestionsDatabaseUpdate
  }

  case class SuggestionsDatabaseUpdateNotification(
    updates: Seq[SuggestionsDatabaseUpdate],
    currentVersion: Long
  )

  private object CodecField {

    val Type = "type"

    val Id = "id"

    val Name = "name"

    val Arguments = "arguments"

    val SelfType = "selfType"

    val ReturnType = "returnType"

    val Documentation = "documentation"

    val Scope = "scope"

    val Suggestion = "suggestion"
  }

  private object CodecType {

    val Add = "Add"

    val Remove = "Remove"

    val Modify = "Modify"
  }

  implicit val decoder: Decoder[SuggestionsDatabaseUpdate] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case CodecType.Add =>
          for {
            id         <- cursor.downField(CodecField.Id).as[Long]
            suggestion <- cursor.downField(CodecField.Suggestion).as[Suggestion]
          } yield SuggestionsDatabaseUpdate.Add(id, suggestion)

        case CodecType.Modify =>
          for {
            id   <- cursor.downField(CodecField.Id).as[Long]
            name <- cursor.downField(CodecField.Name).as[Option[String]]
            arguments <-
              cursor
                .downField(CodecField.Arguments)
                .as[Option[Seq[Suggestion.Argument]]]
            selfType <- cursor.downField(CodecField.SelfType).as[Option[String]]
            returnType <-
              cursor.downField(CodecField.ReturnType).as[Option[String]]
            doc <- cursor.downField(CodecField.Documentation).as[Option[String]]
            scope <-
              cursor.downField(CodecField.Scope).as[Option[Suggestion.Scope]]
          } yield SuggestionsDatabaseUpdate
            .Modify(id, name, arguments, selfType, returnType, doc, scope)

        case CodecType.Remove =>
          for {
            id <- cursor.downField(CodecField.Id).as[Long]
          } yield SuggestionsDatabaseUpdate.Remove(id)
      }
    }

  implicit val encoder: Encoder[SuggestionsDatabaseUpdate] =
    Encoder.instance[SuggestionsDatabaseUpdate] {
      case SuggestionsDatabaseUpdate.Add(id, suggestion) =>
        Json.obj(
          CodecField.Type       -> CodecType.Add.asJson,
          CodecField.Id         -> id.asJson,
          CodecField.Suggestion -> suggestion.asJson
        )

      case SuggestionsDatabaseUpdate.Modify(
            id,
            name,
            arguments,
            selfType,
            returnType,
            doc,
            scope
          ) =>
        Json
          .obj(
            CodecField.Type          -> CodecType.Modify.asJson,
            CodecField.Id            -> id.asJson,
            CodecField.Name          -> name.asJson,
            CodecField.Arguments     -> arguments.asJson,
            CodecField.SelfType      -> selfType.asJson,
            CodecField.ReturnType    -> returnType.asJson,
            CodecField.Documentation -> doc.asJson,
            CodecField.Scope         -> scope.asJson
          )
          .dropNullValues

      case SuggestionsDatabaseUpdate.Remove(id) =>
        Json.obj(
          CodecField.Type -> CodecType.Remove.asJson,
          CodecField.Id   -> id.asJson
        )
    }
}
