package org.enso.languageserver.runtime

/** An object pointing to a method definition.
  *
  * @param module the module of the method file
  * @param definedOnType method type
  * @param name method name
  */
case class MethodPointer(module: String, definedOnType: String, name: String)
