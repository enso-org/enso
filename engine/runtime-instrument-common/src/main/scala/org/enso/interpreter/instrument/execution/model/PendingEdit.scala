package org.enso.interpreter.instrument.execution.model

import org.enso.polyglot.runtime.Runtime.Api.ExpressionId
import org.enso.text.editing.model.{IdMap, TextEdit}

sealed trait PendingEdit {
  def edit:    TextEdit
  def execute: Boolean
  def idMap:   Option[IdMap]
}
object PendingEdit {

  /** The edit that was not applied.
    *
    * @param edit a diff describing changes made to a file
    * @param execute whether to execute the program after applying the edit
    * @param idMap external identifiers
    */
  case class ApplyEdit(edit: TextEdit, execute: Boolean, idMap: Option[IdMap])
      extends PendingEdit

  case class SetExpressionValue(edit: TextEdit, id: ExpressionId, value: String)
      extends PendingEdit {

    /** @inheritdoc */
    override val execute: Boolean = true

    /** @inheritdoc */
    override val idMap: Option[IdMap] = None
  }

}
