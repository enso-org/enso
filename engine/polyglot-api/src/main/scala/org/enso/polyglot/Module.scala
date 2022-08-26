package org.enso.polyglot

import org.graalvm.polyglot.Value

/** Represents an Enso Module.
  *
  * @param value the polyglot value of this scope
  */
class Module(private val value: Value) {
  import MethodNames.Module._

  /** @return the name of the module
    */
  def getName: String = value.invokeMember(GET_NAME).asString()

  /** @return the associated type of this module
    */
  def getAssociatedType: Value =
    value.invokeMember(GET_ASSOCIATED_TYPE)

  /** Gets a constructor definition by name.
    *
    * @param name the constructor name
    * @return the polyglot representation of the constructor.
    */
  def getConstructor(name: String): Value =
    value.invokeMember(GET_CONSTRUCTOR, name)

  def getType(name: String): Value =
    value.invokeMember(GET_TYPE, name)

  /** Gets a method by the type it's defined on and name.
    *
    * @param constructor the constructor the method is defined on
    * @param name the name of the method
    * @return the runtime representation of the method
    */
  def getMethod(constructor: Value, name: String): Option[Function] = {
    val newVal = value.invokeMember(GET_METHOD, constructor, name);
    if (newVal.isNull) { None }
    else {
      Some(new Function(newVal))
    }
  }

  /** Evaluates an arbitrary expression as if it were placed in a function
    * body inside this module.
    *
    * @param code the expression to evaluate
    * @return the return value of the expression
    */
  def evalExpression(code: String): Value =
    value.invokeMember(EVAL_EXPRESSION, code)

  /** Triggers generation of documentation from module sources.
    *
    * @return value with `GENERATE_DOCS` invoked on it.
    */
  def generateDocs(): Value = {
    value.invokeMember(GENERATE_DOCS)
  }

  /** Triggers gathering of import statements from module sources.
    *
    * @return value with `GATHER_IMPORT_STATEMENTS` invoked on it.
    */
  def gatherImportStatements(): Seq[String] = {
    val array = value.invokeMember(GATHER_IMPORT_STATEMENTS)
    val size  = array.getArraySize
    for (i <- 0L until size) yield array.getArrayElement(i).asString()
  }

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
