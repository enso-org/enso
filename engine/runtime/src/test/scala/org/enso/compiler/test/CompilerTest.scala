package org.enso.compiler.test

import org.enso.compiler.codegen.AstToIR
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.flexer.Reader
import org.enso.syntax.text.{AST, Parser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

trait CompilerTest extends AnyWordSpecLike with Matchers with CompilerRunner
trait CompilerRunner {

  // === IR Utilities =========================================================

  /** Converts program text to IR.
    *
    * @param source the source code
    * @return the [[IR]] representing `source`
    */
  def toIR(source: String): IR = {
    val parser: Parser = Parser()
    val unresolvedAST: AST.Module =
      parser.run(new Reader(source))
    val resolvedAST: AST.Module = parser.dropMacroMeta(unresolvedAST)

    val mExpr = AstToIR.translateInline(resolvedAST)

    mExpr match {
      case Some(expr) => expr
      case None       => AstToIR.translate(resolvedAST)
    }
  }

  /** Executes the specified list of passes in order on the provided [[IR]].
    *
    * @param ir the ir to run the passes on
    * @param passes the passes to run
    * @return the result of executing `passes` in sequence on `ir`
    */
  def runPasses(ir: IR, passes: List[IRPass]): IR = ir match {
    case expr: IR.Expression =>
      passes.foldLeft(expr)(
        (intermediate, pass) => pass.runExpression(intermediate)
      )
    case mod: IR.Module =>
      passes.foldLeft(mod)((intermediate, pass) => pass.runModule(intermediate))
    case _ => throw new RuntimeException(s"Cannot run passes on $ir.")
  }

  // === IR Testing Utils =====================================================

  /** Hoists the provided expression into the body of a method.
    *
    * @param ir the expression to hoist
    * @return a method containing `ir` as its body
    */
  def asMethod(ir: IR.Expression): IR.Module.Scope.Definition.Method = {
    IR.Module.Scope.Definition
      .Method(
        IR.Name.Literal("TestType", None),
        IR.Name.Literal("testMethod", None),
        ir,
        None
      )
  }

  /** Hoists the provided expression as the default value of an atom argument.
    *
    * @param ir the expression to hoist
    * @return an atom with one argument `arg` with default value `ir`
    */
  def asAtomDefaultArg(ir: IR.Expression): IR.Module.Scope.Definition.Atom = {
    IR.Module.Scope.Definition.Atom(
      IR.Name.Literal("TestAtom", None),
      List(
        IR.DefinitionArgument
          .Specified(
            IR.Name.Literal("arg", None),
            Some(ir),
            suspended = false,
            None
          )
      ),
      None
    )
  }

  /** Creates a module containing both an atom and a method that use the
    * provided expression.
    *
    * The expression is used in the default for an atom argument, as in
    * [[asAtomDefaultArg()]], and in the body of a method, as in [[asMethod()]].
    *
    * @param expr the expression
    * @return a module containing an atom def and method def using `expr`
    */
  def moduleDefsFrom(expr: IR.Expression): IR.Module = {
    IR.Module(List(), List(asAtomDefaultArg(expr), asMethod(expr)), None)
  }
}
