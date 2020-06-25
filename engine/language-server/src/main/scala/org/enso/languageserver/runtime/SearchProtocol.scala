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

      val Delete = "Delete"

      val Update = "Update"
    }

    implicit val decoder: Decoder[SuggestionsDatabaseUpdate] =
      Decoder.instance { cursor =>
        cursor.downField(CodecField.Type).as[String].flatMap {
          case CodecType.Add =>
            for {
              id <- cursor.downField(CodecField.Id).as[Long]
              suggestion <-
                cursor.downField(CodecField.Suggestion).as[Suggestion]
            } yield SuggestionsDatabaseUpdate.Add(id, suggestion)

          case CodecType.Update =>
            for {
              id   <- cursor.downField(CodecField.Id).as[Long]
              name <- cursor.downField(CodecField.Name).as[Option[String]]
              arguments <-
                cursor
                  .downField(CodecField.Arguments)
                  .as[Option[Seq[Suggestion.Argument]]]
              selfType <-
                cursor.downField(CodecField.SelfType).as[Option[String]]
              returnType <-
                cursor.downField(CodecField.ReturnType).as[Option[String]]
              doc <-
                cursor.downField(CodecField.Documentation).as[Option[String]]
              scope <-
                cursor.downField(CodecField.Scope).as[Option[Suggestion.Scope]]
            } yield SuggestionsDatabaseUpdate
              .Modify(id, name, arguments, selfType, returnType, doc, scope)

          case CodecType.Delete =>
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
              CodecField.Type          -> CodecType.Update.asJson,
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
            CodecField.Type -> CodecType.Delete.asJson,
            CodecField.Id   -> id.asJson
          )
      }

    private object SuggestionType {

      val Atom = "atom"

      val Method = "method"

      val Function = "function"

      val Local = "local"
    }

    implicit val suggestionEncoder: Encoder[Suggestion] =
      Encoder.instance[Suggestion] {
        case Suggestion.Atom(name, arguments, returnType, doc) =>
          Json
            .obj(
              CodecField.Type          -> SuggestionType.Atom.asJson,
              CodecField.Name          -> name.asJson,
              CodecField.Arguments     -> arguments.asJson,
              CodecField.ReturnType    -> returnType.asJson,
              CodecField.Documentation -> doc.asJson
            )
            .dropNullValues

        case Suggestion.Method(name, args, selfType, returnType, doc) =>
          Json
            .obj(
              CodecField.Type          -> SuggestionType.Method.asJson,
              CodecField.Name          -> name.asJson,
              CodecField.Arguments     -> args.asJson,
              CodecField.SelfType      -> selfType.asJson,
              CodecField.ReturnType    -> returnType.asJson,
              CodecField.Documentation -> doc.asJson
            )
            .dropNullValues

        case Suggestion.Function(name, args, returnType, scope) =>
          Json
            .obj(
              CodecField.Type       -> SuggestionType.Function.asJson,
              CodecField.Name       -> name.asJson,
              CodecField.Arguments  -> args.asJson,
              CodecField.ReturnType -> returnType.asJson,
              CodecField.Scope      -> scope.asJson
            )
            .dropNullValues

        case Suggestion.Local(name, returnType, scope) =>
          Json
            .obj(
              CodecField.Type       -> SuggestionType.Local.asJson,
              CodecField.Name       -> name.asJson,
              CodecField.ReturnType -> returnType.asJson,
              CodecField.Scope      -> scope.asJson
            )
            .dropNullValues
      }

    implicit val suggestionDecoder: Decoder[Suggestion] =
      Decoder.instance { cursor =>
        cursor.downField(CodecField.Type).as[String].flatMap {
          case SuggestionType.Atom =>
            for {
              name <- cursor.downField(CodecField.Name).as[String]
              args <-
                cursor
                  .downField(CodecField.Arguments)
                  .as[Seq[Suggestion.Argument]]
              returnType <- cursor.downField(CodecField.ReturnType).as[String]
              doc <-
                cursor.downField(CodecField.Documentation).as[Option[String]]
            } yield Suggestion.Atom(name, args, returnType, doc)

          case SuggestionType.Method =>
            for {
              name <- cursor.downField(CodecField.Name).as[String]
              args <-
                cursor
                  .downField(CodecField.Arguments)
                  .as[Seq[Suggestion.Argument]]
              selfType   <- cursor.downField(CodecField.SelfType).as[String]
              returnType <- cursor.downField(CodecField.ReturnType).as[String]
              doc <-
                cursor.downField(CodecField.Documentation).as[Option[String]]
            } yield Suggestion.Method(name, args, selfType, returnType, doc)

          case SuggestionType.Function =>
            for {
              name <- cursor.downField(CodecField.Name).as[String]
              args <-
                cursor
                  .downField(CodecField.Arguments)
                  .as[Seq[Suggestion.Argument]]
              returnType <- cursor.downField(CodecField.ReturnType).as[String]
              scope      <- cursor.downField(CodecField.Scope).as[Suggestion.Scope]
            } yield Suggestion.Function(name, args, returnType, scope)

          case SuggestionType.Local =>
            for {
              name       <- cursor.downField(CodecField.Name).as[String]
              returnType <- cursor.downField(CodecField.ReturnType).as[String]
              scope      <- cursor.downField(CodecField.Scope).as[Suggestion.Scope]
            } yield Suggestion.Local(name, returnType, scope)
        }
      }
  }

  case class SuggestionsDatabaseUpdateNotification(
    updates: Seq[SuggestionsDatabaseUpdate],
    currentVersion: Long
  )

}
