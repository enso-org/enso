package org.enso.compiler.test

import org.enso.compiler.codegen.AstToIR
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassManager
import org.enso.syntax.text.{AST, Parser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

trait CompilerTest extends AnyWordSpecLike with Matchers with CompilerRunner
trait CompilerRunner {

  // === IR Utilities =========================================================

  /** Adds an extension method for converting a string to its AST
    * representation.
    *
    * @param source the source code to convert
    */
  implicit class ToAST(source: String) {

    /** Produces the [[AST]] representation of [[source]].
     *
     * @return [[source]] as an AST
     */
    def toAST: AST = {
      val parser: Parser = Parser()
      val unresolvedAST  = parser.run(source)

      parser.dropMacroMeta(unresolvedAST)
    }
  }

  /** An extension method to allow converting string source code to IR as a
    * module.
    *
    * @param source the source code to convert
    */
  implicit class ToIrModule(source: String) {

    /** Converts program text to a top-level Enso module.
      *
      * @return the [[IR]] representing [[source]]
      */
    def toIrModule: IR.Module = {
      AstToIR.translate(source.toAST)
    }
  }

  /** An extension method to allow converting string source code to IR as an
    * expression.
    *
    * @param source the source code to convert
    */
  implicit class ToIrExpression(source: String) {

    /** Converts the program text to an Enso expression.
      *
      * @return the [[IR]] representing [[source]], if it is a valid expression
      */
    def toIrExpression: Option[IR.Expression] = {
      AstToIR.translateInline(source.toAST)
    }
  }

  /** Provides an extension method allowing the running of a specified list of
    * passes on the provided IR.
    *
    * @param ir the IR to run the passes on
    */
  implicit class RunPassesOnModule(ir: IR.Module) {

    /** Executes the passes using `passManager` on the input [[ir]].
      *
      * @param passManager the pass configuration
      * @param moduleContext the module context it is executing in
      * @return the result of executing the passes in `passManager` on [[ir]]
      */
    def runPasses(
      passManager: PassManager,
      moduleContext: ModuleContext
    ): IR.Module = {
      passManager.runPassesOnModule(ir, moduleContext)
    }
  }

  /** Provides an extension method allowing the running of a specified list of
    * passes on the provided IR.
    *
    * @param ir the IR to run the passes on
    */
  implicit class RunPassesOnExpression(ir: IR.Expression) {

    /** Executes the passes using `passManager` on the input [[ir]].
      *
      * @param passManager the pass configuration
      * @param inlineContext the inline context it is executing in
      * @return the result of executing the passes in `passManager` on [[ir]]
      */
    def runPasses(
      passManager: PassManager,
      inlineContext: InlineContext
    ): IR.Expression = {
      passManager.runPassesInline(ir, inlineContext)
    }
  }

  /** Adds an extension method to preprocess the source as IR.
    *
    * @param source the source code to preprocess
    */
  implicit class Preprocess(source: String)(
    implicit passManager: PassManager
  ) {

    /** Translates the source code into appropriate IR for testing this pass.
      *
      * @return IR appropriate for testing the alias analysis pass as a module
      */
    def preprocessModule(implicit moduleContext: ModuleContext): IR.Module = {
      source.toIrModule.runPasses(passManager, moduleContext)
    }

    /** Translates the source code into appropriate IR for testing this pass
      *
      * @return IR appropriate for testing the alias analysis pass as an
      *         expression
      */
    def preprocessExpression(
      implicit inlineContext: InlineContext
    ): Option[IR.Expression] = {
      source.toIrExpression.map(_.runPasses(passManager, inlineContext))
    }
  }

  /** Generates a random identifier.
    *
    * @return a random identifier
    */
  def genId: IR.Identifier = IR.randomId

  /** Creates an IR name from a string.
    *
    * @param str the string to turn into a name
    * @return an IR name representing the name `str`
    */
  def nameFromString(str: String): IR.Name.Literal = {
    IR.Name.Literal(str, None)
  }

  // === IR Testing Utils =====================================================

  /** A variety of extension methods on IR expressions to aid testing.
    *
    * @param ir the expression to add extension methods to
    */
  implicit class ExpressionAs(ir: IR.Expression) {

    /** Hoists the provided expression into the body of a method.
      *
      * @return a method containing `ir` as its body
      */
    def asMethod: IR.Module.Scope.Definition.Method = {
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
      * @return an atom with one argument `arg` with default value `ir`
      */
    def asAtomDefaultArg: IR.Module.Scope.Definition.Atom = {
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
      * @return a module containing an atom def and method def using `expr`
      */
    def asModuleDefs: IR.Module = {
      IR.Module(List(), List(ir.asAtomDefaultArg, ir.asMethod), None)
    }
  }
}
