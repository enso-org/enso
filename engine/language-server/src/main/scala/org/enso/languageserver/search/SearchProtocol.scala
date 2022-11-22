package org.enso.languageserver.search

import enumeratum._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.languageserver.filemanager.{FileSystemFailure, Path}
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{DocSection, Suggestion}
import org.enso.searcher.SuggestionEntry
import org.enso.text.editing.model.Position

object SearchProtocol {

  type SuggestionId = Long

  private object CodecField {

    val Type = "type"

    val ExternalId = "externalId"

    val Module = "module"

    val Name = "name"

    val Params = "params"

    val ReturnType = "returnType"

    val ParentType = "parentType"

    val Documentation = "documentation"

    val DocumentationHtml = "documentationHtml"

    val DocumentationSections = "documentationSections"

    val Reexport = "reexport"
  }

  private object CodecType {

    val Add = "Add"

    val Remove = "Remove"

    val Modify = "Modify"
  }

  private object SuggestionType {

    val Module = "module"

    val Type = "type"

    val Constructor = "constructor"

    val Method = "method"

    val Function = "function"

    val Local = "local"
  }

  object DocSectionType {

    val Tag = "tag"

    val Paragraph = "paragraph"

    val Keyed = "keyed"

    val Marked = "marked"
  }

  object DocSectionMarkType {

    val Important = "Important"

    val Info = "Info"

    val Example = "Example"
  }

  implicit val docSectionEncoder: Encoder[DocSection] =
    Encoder.instance[DocSection] {
      case tag: DocSection.Tag =>
        Encoder[DocSection.Tag]
          .apply(tag)
          .deepMerge(Json.obj(CodecField.Type -> DocSectionType.Tag.asJson))

      case paragraph: DocSection.Paragraph =>
        Encoder[DocSection.Paragraph]
          .apply(paragraph)
          .deepMerge(
            Json.obj(CodecField.Type -> DocSectionType.Paragraph.asJson)
          )

      case keyed: DocSection.Keyed =>
        Encoder[DocSection.Keyed]
          .apply(keyed)
          .deepMerge(Json.obj(CodecField.Type -> DocSectionType.Keyed.asJson))

      case marked: DocSection.Marked =>
        Encoder[DocSection.Marked]
          .apply(marked)
          .deepMerge(Json.obj(CodecField.Type -> DocSectionType.Marked.asJson))
    }

  implicit val docSectionDecoder: Decoder[DocSection] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case DocSectionType.Tag =>
          Decoder[DocSection.Tag].tryDecode(cursor)

        case DocSectionType.Paragraph =>
          Decoder[DocSection.Paragraph].tryDecode(cursor)

        case DocSectionType.Keyed =>
          Decoder[DocSection.Keyed].tryDecode(cursor)

        case DocSectionType.Marked =>
          Decoder[DocSection.Marked].tryDecode(cursor)
      }
    }

  implicit val docSectionMarkEncoder: Encoder[DocSection.Mark] =
    Encoder.instance {
      case _: DocSection.Mark.Important =>
        DocSectionMarkType.Important.asJson
      case _: DocSection.Mark.Info =>
        DocSectionMarkType.Info.asJson
      case _: DocSection.Mark.Example =>
        DocSectionMarkType.Example.asJson
    }

  implicit val docSectionMarkDecoder: Decoder[DocSection.Mark] =
    Decoder.instance { cursor =>
      cursor.as[String].map {
        case DocSectionMarkType.Important => DocSection.Mark.Important()
        case DocSectionMarkType.Info      => DocSection.Mark.Info()
        case DocSectionMarkType.Example   => DocSection.Mark.Example()
      }
    }

  implicit val suggestionEncoder: Encoder[Suggestion] =
    Encoder.instance[Suggestion] {
      case module: Suggestion.Module =>
        Encoder[Suggestion.Module]
          .apply(module)
          .deepMerge(Json.obj(CodecField.Type -> SuggestionType.Module.asJson))

      case tpe: Suggestion.Type =>
        Encoder[Suggestion.Type]
          .apply(tpe)
          .deepMerge(
            Json.obj(
              CodecField.Type       -> SuggestionType.Type.asJson,
              CodecField.ReturnType -> Json.Null
            )
          )
          .dropNullValues

      case constructor: Suggestion.Constructor =>
        Encoder[Suggestion.Constructor]
          .apply(constructor)
          .deepMerge(
            Json.obj(CodecField.Type -> SuggestionType.Constructor.asJson)
          )
          .dropNullValues

      case method: Suggestion.Method =>
        Encoder[Suggestion.Method]
          .apply(method)
          .deepMerge(
            Json.obj(CodecField.Type -> SuggestionType.Method.asJson)
          )
          .dropNullValues

      case conversion: Suggestion.Conversion =>
        Encoder[Suggestion.Method]
          .apply(conversionToMethod(conversion))
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

  private def conversionToMethod(
    conversion: Suggestion.Conversion
  ): Suggestion.Method = {
    val arg = Suggestion.Argument(
      Suggestion.Kind.Conversion.From,
      conversion.sourceType,
      false,
      false,
      None
    )
    Suggestion.Method(
      conversion.externalId,
      conversion.module,
      Suggestion.Kind.Conversion.From,
      arg +: conversion.arguments,
      conversion.returnType,
      conversion.returnType,
      isStatic = false,
      conversion.documentation,
      conversion.documentationHtml,
      None,
      conversion.reexport
    )
  }

  private val suggestionTypeDecoder: Decoder[Suggestion.Type] =
    Decoder.instance { cursor =>
      for {
        externalId <- cursor
          .downField(CodecField.ExternalId)
          .as[Option[Suggestion.ExternalId]]
        module <- cursor.downField(CodecField.Module).as[String]
        name   <- cursor.downField(CodecField.Name).as[String]
        params <- cursor
          .downField(CodecField.Params)
          .as[Seq[Suggestion.Argument]]
        parentType <- cursor.downField(CodecField.ParentType).as[Option[String]]
        documentation <- cursor
          .downField(CodecField.Documentation)
          .as[Option[String]]
        documentationHtml <- cursor
          .downField(CodecField.DocumentationHtml)
          .as[Option[String]]
        documentationSections <- cursor
          .downField(CodecField.DocumentationSections)
          .as[Option[List[DocSection]]]
        reexport <- cursor.downField(CodecField.Reexport).as[Option[String]]
      } yield {
        val returnType =
          QualifiedName.fromString(module).createChild(name).toString
        Suggestion.Type(
          externalId,
          module,
          name,
          params,
          returnType,
          parentType,
          documentation,
          documentationHtml,
          documentationSections,
          reexport
        )
      }
    }

  implicit val suggestionDecoder: Decoder[Suggestion] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case SuggestionType.Module =>
          Decoder[Suggestion.Module].tryDecode(cursor)

        case SuggestionType.Type =>
          suggestionTypeDecoder.tryDecode(cursor)

        case SuggestionType.Constructor =>
          Decoder[Suggestion.Constructor].tryDecode(cursor)

        case SuggestionType.Method =>
          Decoder[Suggestion.Method].tryDecode(cursor)

        case SuggestionType.Function =>
          Decoder[Suggestion.Function].tryDecode(cursor)

        case SuggestionType.Local =>
          Decoder[Suggestion.Local].tryDecode(cursor)
      }
    }

  /** The object representing a field modification.
    *
    * @param tag the modifying action
    * @param value the updated value
    */
  case class FieldUpdate[A](tag: FieldAction, value: Option[A])

  /** The modifying action on the field. */
  sealed trait FieldAction extends EnumEntry
  object FieldAction extends Enum[FieldAction] with CirceEnum[FieldAction] {

    case object Remove extends FieldAction
    case object Set    extends FieldAction

    override def values = findValues
  }

  /** An operation applied to the suggestion argument. */
  sealed trait SuggestionArgumentUpdate
  object SuggestionArgumentUpdate {

    /** Add the argument to a list.
      *
      * @param index the position of the argument
      * @param argument the argument to add
      */
    case class Add(index: Int, argument: Suggestion.Argument)
        extends SuggestionArgumentUpdate

    /** Remove the argument from a list.
      *
      * @param index the position of the arugment
      */
    case class Remove(index: Int) extends SuggestionArgumentUpdate

    /** Modify the argument at the specified index.
      *
      * @param index the position of the argument
      * @param name the name to update
      * @param reprType the argument type to update
      * @param isSuspended the suspended flag to update
      * @param hasDefault the default flag to update
      * @param defaultValue the default value to update
      */
    case class Modify(
      index: Int,
      name: Option[FieldUpdate[String]]         = None,
      reprType: Option[FieldUpdate[String]]     = None,
      isSuspended: Option[FieldUpdate[Boolean]] = None,
      hasDefault: Option[FieldUpdate[Boolean]]  = None,
      defaultValue: Option[FieldUpdate[String]] = None
    ) extends SuggestionArgumentUpdate

    implicit val suggestionArgumentActionDecoder
      : Decoder[SuggestionArgumentUpdate] =
      Decoder.instance { cursor =>
        cursor.downField(CodecField.Type).as[String].flatMap {
          case CodecType.Add =>
            Decoder[SuggestionArgumentUpdate.Add].tryDecode(cursor)

          case CodecType.Remove =>
            Decoder[SuggestionArgumentUpdate.Remove].tryDecode(cursor)

          case CodecType.Modify =>
            Decoder[SuggestionArgumentUpdate.Modify].tryDecode(cursor)
        }
      }

    implicit val suggestionArgumentActionEncoder
      : Encoder[SuggestionArgumentUpdate] =
      Encoder.instance[SuggestionArgumentUpdate] {
        case add: SuggestionArgumentUpdate.Add =>
          Encoder[SuggestionArgumentUpdate.Add]
            .apply(add)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Add.asJson))
            .dropNullValues

        case remove: SuggestionArgumentUpdate.Remove =>
          Encoder[SuggestionArgumentUpdate.Remove]
            .apply(remove)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Remove.asJson))
            .dropNullValues

        case modify: SuggestionArgumentUpdate.Modify =>
          Encoder[SuggestionArgumentUpdate.Modify]
            .apply(modify)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Modify.asJson))
            .dropNullValues
      }
  }

  /** Base trait for suggestion database updaetes. */
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
      * @param externalId the external id to update
      * @param arguments the arguments to update
      * @param module the module name to update
      * @param selfType the self type to update
      * @param returnType the return type to update
      * @param documentation the documentation string to update
      * @param documentationHtml the HTML documentation to update
      * @param scope the scope to update
      * @param reexport the module reexporting the suggestion
      */
    case class Modify(
      id: SuggestionId,
      externalId: Option[FieldUpdate[Suggestion.ExternalId]] = None,
      arguments: Option[Seq[SuggestionArgumentUpdate]]       = None,
      module: Option[FieldUpdate[String]]                    = None,
      selfType: Option[FieldUpdate[String]]                  = None,
      returnType: Option[FieldUpdate[String]]                = None,
      documentation: Option[FieldUpdate[String]]             = None,
      documentationHtml: Option[FieldUpdate[String]]         = None,
      scope: Option[FieldUpdate[Suggestion.Scope]]           = None,
      reexport: Option[FieldUpdate[String]]                  = None
    ) extends SuggestionsDatabaseUpdate

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
  }

  /** The type of a suggestion. */
  sealed trait SuggestionKind extends EnumEntry
  object SuggestionKind
      extends Enum[SuggestionKind]
      with CirceEnum[SuggestionKind] {

    /** A module suggestion. */
    case object Module extends SuggestionKind

    /** An type suggestion. */
    case object Type extends SuggestionKind

    /** An type constructor suggestion. */
    case object Constructor extends SuggestionKind

    /** A method suggestion. */
    case object Method extends SuggestionKind

    /** A conversion suggestion. */
    case object Conversion extends SuggestionKind

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
        case Suggestion.Kind.Module      => Module
        case Suggestion.Kind.Type        => Type
        case Suggestion.Kind.Constructor => Constructor
        case Suggestion.Kind.Method      => Method
        case Suggestion.Kind.Conversion  => Conversion
        case Suggestion.Kind.Function    => Function
        case Suggestion.Kind.Local       => Local
      }

    /** Convert from API kind to [[Suggestion.Kind]]
      *
      * @param kind the API kind
      * @return the suggestion kind
      */
    def toSuggestion(kind: SuggestionKind): Suggestion.Kind =
      kind match {
        case Module      => Suggestion.Kind.Module
        case Type        => Suggestion.Kind.Type
        case Constructor => Suggestion.Kind.Constructor
        case Method      => Suggestion.Kind.Method
        case Conversion  => Suggestion.Kind.Conversion
        case Function    => Suggestion.Kind.Function
        case Local       => Suggestion.Kind.Local
      }
  }

  /** The entry in the suggestions database.
    *
    * @param id the suggestion id
    * @param suggestion the suggestion
    */
  case class SuggestionDatabaseEntry(id: SuggestionId, suggestion: Suggestion)

  object SuggestionDatabaseEntry {

    /** Create the database entry from the polyglot suggestion entry.
      *
      * @param entry the suggestion entry
      * @return the database entry
      */
    def apply(entry: SuggestionEntry): SuggestionDatabaseEntry =
      new SuggestionDatabaseEntry(entry.id, entry.suggestion)

    private object CodecField {

      val Id = "id"

      val Suggestion = "suggestion"
    }

    implicit val encoder: Encoder[SuggestionDatabaseEntry] =
      Encoder.instance { entry =>
        Json.obj(
          CodecField.Id         -> entry.id.asJson,
          CodecField.Suggestion -> Encoder[Suggestion].apply(entry.suggestion)
        )
      }

    implicit val decoder: Decoder[SuggestionDatabaseEntry] =
      Decoder.instance { cursor =>
        for {
          id         <- cursor.downField(CodecField.Id).as[SuggestionId]
          suggestion <- cursor.downField(CodecField.Suggestion).as[Suggestion]
        } yield SuggestionDatabaseEntry(id, suggestion)
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
    entries: Seq[SuggestionDatabaseEntry]
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

  /** The reply to the [[Completion]] request.
    *
    * @param currentVersion current version of the suggestions database
    * @param results the list of suggestion ids matched the search query
    */
  case class CompletionResult(currentVersion: Long, results: Seq[SuggestionId])

  /** Base trait for export statements. */
  sealed trait Export {
    def module: String
  }
  object Export {

    /** Qualified module re-export.
      *
      * @param module the module name that exports the given module
      * @param alias new module name if the module was renamed in the export
      * clause
      */
    case class Qualified(module: String, alias: Option[String]) extends Export

    /** Unqualified module export.
      *
      * @param module the module name that exports the given module
      */
    case class Unqualified(module: String) extends Export

    private object CodecType {

      val Qualified = "Qualified"

      val Unqualified = "Unqualified"
    }

    implicit val encoder: Encoder[Export] =
      Encoder.instance {
        case qualified: Qualified =>
          Encoder[Export.Qualified]
            .apply(qualified)
            .deepMerge(Json.obj(CodecField.Type -> CodecType.Qualified.asJson))
            .dropNullValues

        case unqualified: Unqualified =>
          Encoder[Export.Unqualified]
            .apply(unqualified)
            .deepMerge(
              Json.obj(CodecField.Type -> CodecType.Unqualified.asJson)
            )
            .dropNullValues
      }

    implicit val decoder: Decoder[Export] =
      Decoder.instance { cursor =>
        cursor.downField(CodecField.Type).as[String].flatMap {
          case CodecType.Qualified =>
            Decoder[Export.Qualified].tryDecode(cursor)

          case CodecType.Unqualified =>
            Decoder[Export.Unqualified].tryDecode(cursor)
        }
      }
  }

  /** The request to invalidate the modules index. */
  case object InvalidateModulesIndex

  /** The request to invalidate the suggestions database. */
  case object InvalidateSuggestionsDatabase

  /** The reply to the invalidate request. */
  case object InvalidateSuggestionsDatabaseResult

  /** Base trait for search request errors. */
  sealed trait SearchFailure

  /** Signals about file system error. */
  case class FileSystemError(e: FileSystemFailure) extends SearchFailure

  /** Signals that the project not found in the root directory. */
  case object ProjectNotFoundError extends SearchFailure

  /** Signals that the requested suggestion was not found. */
  case object SuggestionNotFoundError extends SearchFailure

  /** Signals that the module name can not be resolved for the given file.
    *
    * @param file the file path
    */
  case class ModuleNameNotResolvedError(file: Path) extends SearchFailure
}
