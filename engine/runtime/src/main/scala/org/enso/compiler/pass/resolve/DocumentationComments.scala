package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Module,
  Name,
  Pattern,
  Type
}
import org.enso.compiler.core.ir.expression.{Case, Comment, Error}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.module.scope.Export
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{ComplexType, GenerateMethodBodies}

/** Associates doc comments with the commented entities as metadata.
  *
  * If the first module definition is a documentation comment, it is treated as
  * the module documentation.
  *
  * This pass has no configuration.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object DocumentationComments extends IRPass {
  override type Metadata = Doc
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = Seq()
  override lazy val invalidatedPasses: Seq[IRPass] = Seq(
    ComplexType,
    GenerateMethodBodies
  )

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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = resolveModule(ir)

  /** Collects comments for an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information
    *                      needed for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = resolveExpression(ir)

  /** @inheritdoc */

  // === Pass Internals =======================================================

  /** Resolves documentation comments for expressions where it is necessary.
    *
    * @param ir the IR node to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveExpression(ir: Expression): Expression =
    ir.transformExpressions({
      case block: Expression.Block =>
        val newLines       = resolveList(block.expressions :+ block.returnValue)
        val newExpressions = newLines.init.map(resolveExpression)
        val newReturn      = resolveExpression(newLines.last)
        block.copy(expressions = newExpressions, returnValue = newReturn)
      case caseExpr: Case.Expr =>
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
    var lastDoc: Option[Comment.Documentation] = None
    items.flatMap {
      case annotation: Name.Annotation =>
        Some(annotation.asInstanceOf[T])
      case doc: Comment.Documentation =>
        lastDoc = Some(doc)
        None
      case other =>
        val res = lastDoc match {
          case Some(doc) => other.updateMetadata(this -->> Doc(doc.doc))
          case None      => other
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
  private def resolveBranches(items: Seq[Case.Branch]): Seq[Case.Branch] = {
    var lastDoc: Option[String] = None
    items.flatMap {
      case Case.Branch(Pattern.Documentation(doc, _, _, _), _, _, _, _, _) =>
        lastDoc = Some(doc)
        None
      case branch @ Case.Branch(pattern, expression, _, _, _, _) =>
        val resolved =
          branch.copy(
            pattern    = pattern.mapExpressions(resolveExpression),
            expression = resolveExpression(expression)
          )
        val res = lastDoc match {
          case Some(doc) => resolved.updateMetadata(this -->> Doc(doc))
          case None      => resolved
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
    ir: Definition
  ): Definition =
    ir match {
      case _: definition.Method.Conversion =>
        throw new CompilerError(
          "Conversion methods should not yet be present in the compiler " +
          "pipeline."
        )
      case _: Definition.Type =>
        throw new CompilerError(
          "Union types should not yet be present in the compiler pipeline."
        )
      case method: definition.Method.Binding =>
        method.copy(body = resolveExpression(method.body))
      case method: definition.Method.Explicit =>
        method.copy(body = resolveExpression(method.body))
      case tpe: Definition.SugaredType =>
        tpe.copy(body = resolveList(tpe.body).map(resolveIr))
      case doc: Comment.Documentation  => doc
      case tySig: Type.Ascription      => tySig
      case err: Error                  => err
      case ann: Name.GenericAnnotation => ann
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "documentation comment resolution."
        )
    }

  /** Resolves documentation comments in a module.
    *
    * @param ir the module to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveModule(ir: Module): Module = {
    // All entities that came from source (the only ones we care about).
    val allModuleEntities: List[IR] =
      (ir.imports ++ ir.exports ++ ir.bindings)
        .filter(_.location.isDefined)
        .sortWith { (l, r) =>
          val leftLocation = l.location.getOrElse(
            throw new IllegalStateException(
              "Location has been checked to be present."
            )
          )
          val rightLocation = r.location.getOrElse(
            throw new IllegalStateException(
              "Location has been checked to be present."
            )
          )
          leftLocation.start < rightLocation.start
        }
    val newBindings = (allModuleEntities.headOption match {
      case Some(doc: Comment.Documentation) =>
        ir.updateMetadata(this -->> Doc(doc.doc))
        resolveList(ir.bindings.drop(1))
      case _ => resolveList(ir.bindings)
    }).map(resolveDefinition)
    ir.copy(bindings = newBindings)
  }

  /** Resolves documentation comments in an arbitrary IR.
    *
    * @param ir the ir to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveIr(ir: IR): IR =
    ir match {
      case module: Module          => resolveModule(module)
      case expr: Expression        => resolveExpression(expr)
      case df: Definition          => resolveDefinition(df)
      case data: Definition.Data   => data
      case imp: Import             => imp
      case exp: Export.Module      => exp
      case arg: CallArgument       => arg
      case arg: DefinitionArgument => arg
      case pat: Pattern            => pat
    }

  // === Metadata =============================================================

  /** The documentation metadata for a node.
    *
    * @param documentation the documentation as a string
    */
  sealed case class Doc(documentation: String) extends IRPass.IRMetadata {
    override val metadataName: String =
      "DocumentationComments.Doc(\"" + documentation + "\")"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): Doc = this

    /** @inheritdoc */
    override def restoreFromSerialization(compiler: Compiler): Option[Doc] =
      Some(this)

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.IRMetadata] = Some(this)
  }
}
