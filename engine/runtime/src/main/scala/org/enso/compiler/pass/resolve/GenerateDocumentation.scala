package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Case.Branch
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.resolve.DocumentationComments.Doc
import org.enso.docs.generator.DocParserWrapper

/** Generates documentation on resolved IR.
  */
case object GenerateDocumentation extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = DocumentationComments.Doc

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq(DocumentationComments)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq()

  /** Collects comments for a module and assigns them to the commented
    * entities as metadata.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = resolveModule(ir)

  /** Collects comments for an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information
    *                      needed for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = resolveExpression(ir)

  // === Pass Internals =======================================================

  /** Resolves documentation comments for expressions where it is necessary.
    *
    * @param ir the IR node to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveExpression(ir: IR.Expression): IR.Expression =
    ir.transformExpressions({
      case block: IR.Expression.Block =>
        val newLines       = resolveList(block.expressions :+ block.returnValue)
        val newExpressions = newLines.init.map(resolveExpression)
        val newReturn      = resolveExpression(newLines.last)
        block.copy(expressions = newExpressions, returnValue = newReturn)
      case caseExpr: IR.Case.Expr =>
        val newScrutinee = resolveExpression(caseExpr.scrutinee)
        val newBranches  = resolveBranches(caseExpr.branches)
        caseExpr.copy(scrutinee = newScrutinee, branches = newBranches)
    })

  /** Resolves documentation comments in an arbitrary list of IRs.
    *
    * @param items the list of IRs
    * @tparam T the type of IR in the list
    * @return `items`, with any doc comments associated with nodes as metadata
    */
  private def resolveList[T <: IR](items: List[T]): List[T] = {
    var lastDoc: Option[IR.Comment.Documentation] = None
    items.flatMap {
      case doc: IR.Comment.Documentation =>
        val html = DocParserWrapper.runOnPureDoc(doc.doc)
        lastDoc = Some(
          IR.Comment.Documentation(
            html,
            doc.location,
            doc.passData,
            doc.diagnostics
          )
        )
        None
      case other =>
        val res = lastDoc match {
          case Some(doc) =>
            other.updateMetadata(
              this -->> Doc(DocParserWrapper.runOnPureDoc(doc.doc))
            )
          case None => other
        }
        lastDoc = None
        Some(res)
    }
  }

  /** Resolves documentation comments in a list of branches, potentially
    * containing the dummy documentation branches.
    *
    * @param items the list of branches
    * @return `items`, with any doc comments associated with nodes as metadata
    */
  private def resolveBranches(items: Seq[Branch]): Seq[Branch] = {
    var lastDoc: Option[String] = None
    items.flatMap {
      case Branch(IR.Pattern.Documentation(doc, _, _, _), _, _, _, _) =>
        lastDoc = Some(DocParserWrapper.runOnPureDoc(doc))
        None
      case branch @ Branch(pattern, expression, _, _, _) =>
        val resolved =
          branch.copy(
            pattern    = pattern.mapExpressions(resolveExpression),
            expression = resolveExpression(expression)
          )
        val res = lastDoc match {
          case Some(doc) =>
            resolved.updateMetadata(
              this -->> Doc(DocParserWrapper.runOnPureDoc(doc))
            )
          case None => resolved
        }
        lastDoc = None
        Some(res)
    }
  }

  /** Resolves documentation comments in a top-level definition.
    *
    * @param ir the definition to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveDefinition(
    ir: IR.Module.Scope.Definition
  ): IR.Module.Scope.Definition =
    ir match {
      case _: Method.Conversion =>
        throw new CompilerError("Conversion methods are not yet supported.")
      case method: IR.Module.Scope.Definition.Method.Binding =>
        method.copy(body = resolveExpression(method.body))
      case method: IR.Module.Scope.Definition.Method.Explicit =>
        method.copy(body = resolveExpression(method.body))
      case tpe: IR.Module.Scope.Definition.Type =>
        tpe.copy(body = resolveList(tpe.body).map(resolveIr))
      case doc: IR.Comment.Documentation =>
        val html = DocParserWrapper.runOnPureDoc(doc.doc)
        IR.Comment.Documentation(
          html,
          doc.location,
          doc.passData,
          doc.diagnostics
        )
      case d: IR.Module.Scope.Definition.Atom => d
      case tySig: IR.Type.Ascription          => tySig
      case err: IR.Error                      => err
      case ann: IR.Name.Annotation            => ann
    }

  /** Resolves documentation comments in a module.
    *
    * @param ir the module to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveModule(ir: IR.Module): IR.Module = {
    val newBindings = resolveList(ir.bindings).map(resolveDefinition)
    ir.copy(bindings = newBindings)
  }

  /** Resolves documentation comments in an arbitrary IR.
    *
    * @param ir the ir to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveIr(ir: IR): IR =
    ir match {
      case module: IR.Module              => resolveModule(module)
      case expr: IR.Expression            => resolveExpression(expr)
      case df: IR.Module.Scope.Definition => resolveDefinition(df)
      case imp: IR.Module.Scope.Import    => imp
      case exp: IR.Module.Scope.Export    => exp
      case arg: IR.CallArgument           => arg
      case arg: IR.DefinitionArgument     => arg
      case pat: IR.Pattern                => pat
    }
}
