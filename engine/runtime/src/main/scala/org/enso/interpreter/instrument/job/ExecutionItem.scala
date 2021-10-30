package org.enso.interpreter.instrument.job

import java.util.UUID

import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode
import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api

/** An execution item. */
sealed trait ExecutionItem

object ExecutionItem {

  /** The explicit method call.
    *
    * @param module the module containing the method
    * @param constructor the type on which the method is defined
    * @param function the method name
    */
  case class Method(
    module: QualifiedName,
    constructor: QualifiedName,
    function: String
  ) extends ExecutionItem

  object Method {

    /** Construct the method call from the [[Api.StackItem.ExplicitCall]].
      *
      * @param call the Api call
      * @return the method call
      */
    def apply(call: Api.StackItem.ExplicitCall): Method =
      Method(
        QualifiedName.fromString(call.methodPointer.module),
        QualifiedName.fromString(call.methodPointer.definedOnType),
        call.methodPointer.name
      )
  }

  /** The call data captured during the program execution.
    *
    * @param expressionId the expression identifier
    * @param callData the fucntion call data
    */
  case class CallData(
    expressionId: UUID,
    callData: FunctionCallInstrumentationNode.FunctionCall
  ) extends ExecutionItem
}
