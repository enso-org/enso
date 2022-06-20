package org.enso.interpreter.instrument.execution.model

import org.enso.text.editing.model.TextEdit

case class PendingEdit(edit: TextEdit, execute: Boolean)
