package org.enso.languageserver.runtime

import enumeratum._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.searcher.Suggestion
import org.enso.text.editing.model.Position

object SearchProtocol {

  type SuggestionId = Long

  sealed trait SuggestionsDatabaseUpdate
  object SuggestionsDatabaseUpdate {

    /** Create or replace the database entry.
      *
      * @param id the suggestion id
      * @param suggestion the new suggestion
      */
    case class Add(id: SuggestionId, suggestion: Suggestion)
        extends SuggestionsDatabaseUpdate

    /** Remove the database entry.
      *
      * @param id the suggestion id
      */
    case class Remove(id: SuggestionId) extends SuggestionsDatabaseUpdate

    private object CodecField {

      val Type = "type"
    }

    private object CodecType {

      val Add = "Add"

      val Remove = "Remove"
    }

    implicit val decoder: Decoder[SuggestionsDatabaseUpdate] =
      Decoder.instance { cursor =>
        cursor.downField(CodecField.Type).as[String].flatMap {
          case CodecType.Add =>
            Decoder[SuggestionsDatabaseUpdate.Add].tryDecode(cursor)

          case CodecType.Remove =>
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

        case remove: SuggestionsDatabaseUpdate.Remove =>
          Encoder[SuggestionsDatabaseUpdate.Remove]
            .apply(remove)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Remove.asJson))
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

  /** The type of a suggestion. */
  sealed trait SuggestionKind extends EnumEntry
  object SuggestionKind
      extends Enum[SuggestionKind]
      with CirceEnum[SuggestionKind] {

    /** An atom suggestion. */
    case object Atom extends SuggestionKind

    /** A method suggestion. */
    case object Method extends SuggestionKind

    /** A function suggestion. */
    case object Function extends SuggestionKind

    /** Local binding suggestion. */
    case object Local extends SuggestionKind

    override val values = findValues

    /** Create API kind from the [[Suggestion.Kind]]
      *
      * @param kind the suggestion kind
      * @return the API kind
      */
    def apply(kind: Suggestion.Kind): SuggestionKind =
      kind match {
        case Suggestion.Kind.Atom     => Atom
        case Suggestion.Kind.Method   => Method
        case Suggestion.Kind.Function => Function
        case Suggestion.Kind.Local    => Local
      }

    /** Convert from API kind to [[Suggestion.Kind]]
      *
      * @param kind the API kind
      * @return the suggestion kind
      */
    def toSuggestion(kind: SuggestionKind): Suggestion.Kind =
      kind match {
        case Atom     => Suggestion.Kind.Atom
        case Method   => Suggestion.Kind.Method
        case Function => Suggestion.Kind.Function
        case Local    => Suggestion.Kind.Local
      }
  }

  /** A notification about changes in the suggestions database.
    *
    * @param updates the list of database updates
    * @param currentVersion current version of the suggestions database
    */
  case class SuggestionsDatabaseUpdateNotification(
    updates: Seq[SuggestionsDatabaseUpdate],
    currentVersion: Long
  )

  /** The request to receive contents of the suggestions database. */
  case object GetSuggestionsDatabase

  /** The reply to the [[GetSuggestionsDatabase]] request.
    *
    * @param entries the entries of the suggestion database
    * @param currentVersion current version of the suggestions database
    */
  case class GetSuggestionsDatabaseResult(
    entries: Seq[SuggestionsDatabaseUpdate],
    currentVersion: Long
  )

  /** The request to receive the current version of the suggestions database. */
  case object GetSuggestionsDatabaseVersion

  /** The reply to the [[GetSuggestionsDatabaseVersion]] request.
    *
    * @param version current version of the suggestions database
    */
  case class GetSuggestionsDatabaseVersionResult(version: Long)

  /** The completion request.
    *
    * @param module the edited module
    * @param position the cursor position
    * @param selfType filter entries matching the self type
    * @param returnType filter entries matching the return type
    * @param tags filter entries by suggestion type
    */
  case class Completion(
    module: String,
    position: Position,
    selfType: Option[String],
    returnType: Option[String],
    tags: Option[Seq[SuggestionKind]]
  )

  /** Te reply to the [[Completion]] request.
    *
    * @param currentVersion current version of the suggestions database
    * @param results the list of suggestion ids matched the search query
    */
  case class CompletionResult(currentVersion: Long, results: Seq[SuggestionId])

}
