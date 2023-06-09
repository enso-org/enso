package org.enso.languageserver.runtime

import org.enso.polyglot.runtime.Runtime.Api

/** A representation of a method call.
  *
  * @param methodPointer the method pointer of a call
  * @param notAppliedArguments indexes of arguments that have not been applied
  * to this method
  */
case class MethodCall(
  methodPointer: MethodPointer,
  notAppliedArguments: Vector[Int]
) {

  /** Convert this method call to the corresponding [[Api]] message. */
  def toApi: Api.MethodCall =
    Api.MethodCall(methodPointer.toApi, notAppliedArguments)
}

/** An object pointing to a method definition.
  *
  * @param module the module of the method file
  * @param definedOnType method type
  * @param name method name
  */
case class MethodPointer(module: String, definedOnType: String, name: String) {

  /** Convert this method pointer to the corresponding [[Api]] message. */
  def toApi: Api.MethodPointer =
    Api.MethodPointer(module, definedOnType, name)
}
