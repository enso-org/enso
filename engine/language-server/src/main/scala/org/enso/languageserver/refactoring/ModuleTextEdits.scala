package org.enso.languageserver.refactoring

import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model.TextEdit

/** A list of edits applied to a module.
  *
  * @param module the qualified module name
  * @param edits the list of text edits
  */
case class ModuleTextEdits(module: String, edits: Seq[TextEdit])

object ModuleTextEdits {

  /** Create an instance of [[ModuleTextEdits]] from the runtime representation.
    *
    * @param runtime the runtime representation of module text edits
    * @return a new instance of [[ModuleTextEdits]]
    */
  def apply(runtime: Api.ModuleTextEdits): ModuleTextEdits =
    ModuleTextEdits(runtime.module, runtime.edits)
}
