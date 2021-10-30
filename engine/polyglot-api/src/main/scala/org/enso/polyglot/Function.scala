package org.enso.polyglot

import org.graalvm.polyglot.Value

/** Represents an Enso function.
  *
  * @param value the polyglot value of this function
  */
class Function(val value: Value) {

  /** Executes the function with given parameters.
    *
    * @param args the execution arguments
    * @return the result of execution
    */
  def execute(args: AnyRef*): Value = value.execute(args: _*)

  /** Checks function equality by checking the identity of the underlying
    * objects.
    *
    * @param obj the other comparison operand
    * @return `true` if the functions correspond to the same underlying object,
    *        `false` otherwise.
    */
  override def equals(obj: Any): Boolean = obj match {
    case fun: Function =>
      value.invokeMember(MethodNames.Function.EQUALS, fun.value).asBoolean()
    case _ => false
  }
}
