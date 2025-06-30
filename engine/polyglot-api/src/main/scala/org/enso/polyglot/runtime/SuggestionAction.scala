package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import org.enso.logger.masking.ToLogString
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api.SuggestionArgumentAction

/** An operation applied to the update */
sealed trait SuggestionAction extends ToLogString
object SuggestionAction {

  /** Add the suggestion. */
  @named("suggestionActionAdd")
  case class Add() extends SuggestionAction {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      "Add()"
  }

  /** Remove the suggestion. */
  @named("suggestionActionRemove")
  case class Remove() extends SuggestionAction {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      "Remove()"
  }

  /** Modify the suggestion.
    *
    * @param externalId the external id to update
    * @param arguments the arguments to update
    * @param returnType the return type to update
    * @param documentation the documentation string to update
    * @param scope the scope to update
    * @param reexport the reexport field to update
    */
  @named("suggestionActionModify")
  case class Modify(
    externalId: Option[Option[Suggestion.ExternalID]] = None,
    arguments: Option[Seq[SuggestionArgumentAction]]  = None,
    returnType: Option[String]                        = None,
    documentation: Option[Option[String]]             = None,
    scope: Option[Suggestion.Scope]                   = None,
    reexport: Option[Option[String]]                  = None
  ) extends SuggestionAction
      with ToLogString {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      "Modify(" +
      s"externalId=$externalId" +
      s",arguments=${arguments.map(_.map(_.toLogString(shouldMask)))}" +
      s",returnType=$returnType" +
      s",documentation=" +
      (if (shouldMask) documentation.map(_.map(_ => STUB))
       else documentation) +
      s",scope=$scope" +
      s",reexport=$reexport" +
      ")"
  }
}
