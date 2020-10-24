package org.enso.polyglot

import org.graalvm.polyglot.Value

/** Represents an Enso Module.
  *
  * @param value the polyglot value of this scope
  */
class Module(private val value: Value) {
  import MethodNames.Module._

  /** @return the associated type of this module
    */
  def getAssociatedConstructor: Value =
    value.invokeMember(GET_ASSOCIATED_CONSTRUCTOR)

  /** Gets a constructor definition by name
    * @param name the constructor name
    * @return the polyglot representation of the constructor.
    */
  def getConstructor(name: String): Value =
    value.invokeMember(GET_CONSTRUCTOR, name)

  /** Gets a method by the type it's defined on and name.
    *
    * @param constructor the constructor the method is defined on
    * @param name the name of the method
    * @return the runtime representation of the method
    */
  def getMethod(constructor: Value, name: String): Function =
    new Function(value.invokeMember(GET_METHOD, constructor, name))

  /** Evaluates an arbitrary expression as if it were placed in a function
    * body inside this module.
    * @param code the expression to evaluate
    * @return the return value of the expression
    */
  def evalExpression(code: String): Value =
    value.invokeMember(EVAL_EXPRESSION, code)

  /** Triggers reparsing of module sources. Used to notify the module that
    * sources have changed.
    */
  def reparse(): Unit = {
    value.invokeMember(REPARSE): Unit
  }

  def setSource(source: String): Unit = {
    value.invokeMember(SET_SOURCE, source): Unit
  }

  def setSourceFile(file: String): Unit = {
    value.invokeMember(SET_SOURCE_FILE, file): Unit
  }
}
