package org.enso.languageserver.runtime

import org.enso.polyglot.runtime.Runtime.Api

/** An object pointing to a method definition.
  *
  * @param module the module of the method file
  * @param definedOnType method type
  * @param name method name
  */
case class MethodPointer(module: String, definedOnType: String, name: String) {

  /** Convert to corresponding [[Api]] message. */
  def toApi: Api.MethodPointer =
    Api.MethodPointer(module, definedOnType, name)
}
