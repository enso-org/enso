package org.enso.languageserver.runtime

import org.enso.languageserver.filemanager.Path

/**
  * An object pointing to a method definition.
  *
  * @param file path to the method file
  * @param definedOnType method type
  * @param name method name
  */
case class MethodPointer(file: Path, definedOnType: String, name: String)
