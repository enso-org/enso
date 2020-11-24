package org.enso.compiler.pass.optimise.config

import org.enso.interpreter.node.ExpressionNode
import org.enso.interpreter.node.callable.thunk.ForceNode
import org.enso.interpreter.node.controlflow.IfThenElseNode
import org.enso.interpreter.runtime.callable.UnresolvedSymbol
import org.enso.interpreter.runtime.callable.argument.CallArgument
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}

import scala.annotation.unused

object IfThenElse {

  /** Builds an optimised node for the if-then-else expression.
    *
    * @param moduleScope the module scope where the node is being built
    * @param localScope the local scope where the node is being built
    * @param args the arguments to the function
    * @return an optimised node representing an if-then-else expression
    */
  def build(moduleScope: ModuleScope)(@unused localScope: LocalScope)(
    args: List[CallArgument]
  ): ExpressionNode = {
    val condition = ForceNode.build(args.head.getExpression)
    val onTrue    = args(1).getExpression
    val onFalse   = args(2).getExpression
    val unresolvedIfThenElse =
      UnresolvedSymbol.build("if_then_else", moduleScope)
    IfThenElseNode.build(condition, onTrue, onFalse, unresolvedIfThenElse)
  }
}
