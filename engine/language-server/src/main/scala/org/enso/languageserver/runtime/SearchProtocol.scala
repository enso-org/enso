package org.enso.languageserver.runtime

import enumeratum._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.languageserver.filemanager.{FileSystemFailure, Path}
import org.enso.polyglot.Suggestion
import org.enso.searcher.SuggestionEntry
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

    /** Modify the database entry.
      *
      * @param id the suggestion id
      * @param returnType the new return type
      */
    case class Modify(id: SuggestionId, returnType: String)
        extends SuggestionsDatabaseUpdate

    private object CodecField {

      val Type = "type"
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
            Decoder[SuggestionsDatabaseUpdate.Add].tryDecode(cursor)

          case CodecType.Remove =>
            Decoder[SuggestionsDatabaseUpdate.Remove].tryDecode(cursor)

          case CodecType.Modify =>
            Decoder[SuggestionsDatabaseUpdate.Modify].tryDecode(cursor)
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

        case modify: SuggestionsDatabaseUpdate.Modify =>
          Encoder[SuggestionsDatabaseUpdate.Modify]
            .apply(modify)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Modify.asJson))
            .dropNullValues
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
    * @param currentVersion current version of the suggestions database
    * @param updates the list of database updates
    */
  case class SuggestionsDatabaseUpdateNotification(
    currentVersion: Long,
    updates: Seq[SuggestionsDatabaseUpdate]
  )

  /** The request to receive contents of the suggestions database. */
  case object GetSuggestionsDatabase

  /** The reply to the [[GetSuggestionsDatabase]] request.
    *
    * @param currentVersion current version of the suggestions database
    * @param entries the entries of the suggestion database
    */
  case class GetSuggestionsDatabaseResult(
    currentVersion: Long,
    entries: Seq[SuggestionEntry]
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
    * @param file the edited file
    * @param position the cursor position
    * @param selfType filter entries matching the self type
    * @param returnType filter entries matching the return type
    * @param tags filter entries by suggestion type
    */
  case class Completion(
    file: Path,
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

  /** Base trait for search request errors. */
  sealed trait SearchFailure

  /** Signals about file system error. */
  case class FileSystemError(e: FileSystemFailure) extends SearchFailure

  /** Signals that the project not found in the root directory. */
  case object ProjectNotFoundError extends SearchFailure

  /** Signals that the module name can not be resolved for the given file.
    *
    * @param file the file path
    */
  case class ModuleNameNotResolvedError(file: Path) extends SearchFailure
}
