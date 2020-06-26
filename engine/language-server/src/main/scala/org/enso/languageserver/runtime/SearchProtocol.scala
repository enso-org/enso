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

    private object CodecField {

      val Type = "type"
    }

    private object CodecType {

      val Add = "Add"

      val Delete = "Delete"

      val Update = "Update"
    }

    implicit val decoder: Decoder[SuggestionsDatabaseUpdate] =
      Decoder.instance { cursor =>
        cursor.downField(CodecField.Type).as[String].flatMap {
          case CodecType.Add =>
            Decoder[SuggestionsDatabaseUpdate.Add].tryDecode(cursor)

          case CodecType.Update =>
            Decoder[SuggestionsDatabaseUpdate.Modify].tryDecode(cursor)

          case CodecType.Delete =>
            Decoder[SuggestionsDatabaseUpdate.Remove].tryDecode(cursor)
        }
      }

    implicit val encoder: Encoder[SuggestionsDatabaseUpdate] =
      Encoder.instance[SuggestionsDatabaseUpdate] {
        case add: SuggestionsDatabaseUpdate.Add =>
          Encoder[SuggestionsDatabaseUpdate.Add]
            .apply(add)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Add.asJson))
            .dropNullValues

        case modify: SuggestionsDatabaseUpdate.Modify =>
          Encoder[SuggestionsDatabaseUpdate.Modify]
            .apply(modify)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Update.asJson))
            .dropNullValues

        case remove: SuggestionsDatabaseUpdate.Remove =>
          Encoder[SuggestionsDatabaseUpdate.Remove]
            .apply(remove)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Delete.asJson))
      }

    private object SuggestionType {

      val Atom = "atom"

      val Method = "method"

      val Function = "function"

      val Local = "local"
    }

    implicit val suggestionEncoder: Encoder[Suggestion] =
      Encoder.instance[Suggestion] {
        case atom: Suggestion.Atom =>
          Encoder[Suggestion.Atom]
            .apply(atom)
            .deepMerge(Json.obj(CodecField.Type -> SuggestionType.Atom.asJson))
            .dropNullValues

        case method: Suggestion.Method =>
          Encoder[Suggestion.Method]
            .apply(method)
            .deepMerge(
              Json.obj(CodecField.Type -> SuggestionType.Method.asJson)
            )
            .dropNullValues

        case function: Suggestion.Function =>
          Encoder[Suggestion.Function]
            .apply(function)
            .deepMerge(
              Json.obj(CodecField.Type -> SuggestionType.Function.asJson)
            )
            .dropNullValues

        case local: Suggestion.Local =>
          Encoder[Suggestion.Local]
            .apply(local)
            .deepMerge(Json.obj(CodecField.Type -> SuggestionType.Local.asJson))
            .dropNullValues
      }

    implicit val suggestionDecoder: Decoder[Suggestion] =
      Decoder.instance { cursor =>
        cursor.downField(CodecField.Type).as[String].flatMap {
          case SuggestionType.Atom =>
            Decoder[Suggestion.Atom].tryDecode(cursor)

          case SuggestionType.Method =>
            Decoder[Suggestion.Method].tryDecode(cursor)

          case SuggestionType.Function =>
            Decoder[Suggestion.Function].tryDecode(cursor)

          case SuggestionType.Local =>
            Decoder[Suggestion.Local].tryDecode(cursor)
        }
      }
  }

  case class SuggestionsDatabaseUpdateNotification(
    updates: Seq[SuggestionsDatabaseUpdate],
    currentVersion: Long
  )

}
