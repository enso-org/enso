package org.enso.interpreter.instrument.execution.model

import org.enso.text.editing.model.TextEdit

/** The edit that was not applied.
  *
  * @param edit a diff describing changes made to a file
  * @param execute whether to execute the program after applying the edit
  */
case class PendingEdit(edit: TextEdit, execute: Boolean)
