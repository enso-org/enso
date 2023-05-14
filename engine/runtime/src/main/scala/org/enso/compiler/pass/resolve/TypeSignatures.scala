package org.enso.compiler.pass.resolve

import org.enso.compiler.Compiler
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.lint.UnusedBindings

/** This pass is responsible for resolving type signatures and associating
  * them as metadata with the typed object.
  *
  * Please note that this pass currently does not support typed patterns (and
  * hence doesn't support inline types in lambdas). This support will come later
  * with the work on expanding pattern contexts.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object TypeSignatures extends IRPass {
  override type Metadata = Signature
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    TypeFunctions,
    ModuleAnnotations
  )
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    CachePreferenceAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    TailCall,
    UnusedBindings
  )

  /** Resolves type signatures in a module.
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

  /** Resolves type signatures in an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = resolveExpression(ir)

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  // === Pass Internals =======================================================

  /** Resolves type signatures in a module.
    *
    * @param mod the module to resolve signatures in
    * @return `mod`, with type signatures resolved
    */
  private def resolveModule(mod: IR.Module): IR.Module = {
    var lastSignature: Option[IR.Type.Ascription] = None

    val newBindings: List[IR.Module.Scope.Definition] = mod.bindings.flatMap {
      case sig: IR.Type.Ascription =>
        val res = lastSignature.map(IR.Error.Unexpected.TypeSignature(_))
        lastSignature = Some(sig)
        res
      case meth: IR.Module.Scope.Definition.Method =>
        val newMethod = meth.mapExpressions(resolveExpression)
        val res = lastSignature match {
          case Some(asc @ IR.Type.Ascription(typed, sig, _, _, _)) =>
            val methodRef = meth.methodReference
            val newMethodWithDoc = asc
              .getMetadata(DocumentationComments)
              .map(doc =>
                newMethod.updateMetadata(DocumentationComments -->> doc)
              )
              .getOrElse(newMethod)
            val newMethodWithAnnotations = asc
              .getMetadata(ModuleAnnotations)
              .map(annotations =>
                newMethodWithDoc.updateMetadata(
                  ModuleAnnotations -->> annotations
                )
              )
              .getOrElse(newMethodWithDoc)

            typed match {
              case ref: IR.Name.MethodReference =>
                if (ref isSameReferenceAs methodRef) {
                  Some(
                    newMethodWithAnnotations.updateMetadata(
                      this -->> Signature(sig)
                    )
                  )
                } else {
                  List(
                    IR.Error.Unexpected.TypeSignature(asc),
                    newMethodWithAnnotations
                  )
                }
              case _ =>
                List(
                  IR.Error.Unexpected.TypeSignature(asc),
                  newMethodWithAnnotations
                )
            }
          case None => Some(newMethod)
        }

        lastSignature = None
        res
      case ut: IR.Module.Scope.Definition.Type =>
        Some(
          ut
            .copy(
              params  = ut.params.map(resolveArgument),
              members = ut.members.map(resolveDefinitionData)
            )
            .mapExpressions(resolveExpression)
        )
      case err: IR.Error                  => Some(err)
      case ann: IR.Name.GenericAnnotation => Some(ann)
      case _: IR.Module.Scope.Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present during type " +
          "signature resolution."
        )
      case _: IR.Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "type signature resolution."
        )
      case _: IR.Comment.Documentation =>
        throw new CompilerError(
          "Documentation comments should not be present during type " +
          "signature resolution."
        )
    } ::: lastSignature
      .map(asc => IR.Error.Unexpected.TypeSignature(asc))
      .toList

    mod.copy(
      bindings = newBindings
    )
  }

  /** Resolves type signatures in an arbitrary expression.
    *
    * @param expr the expression to resolve signatures in
    * @return `expr`, with any type signatures resolved
    */
  private def resolveExpression(expr: IR.Expression): IR.Expression = {
    expr.transformExpressions {
      case block: IR.Expression.Block => resolveBlock(block)
      case sig: IR.Type.Ascription    => resolveAscription(sig)
    }
  }

  private def resolveDefinitionData(
    data: IR.Module.Scope.Definition.Data
  ): IR.Module.Scope.Definition.Data = {
    data.copy(
      arguments = data.arguments.map(resolveArgument)
    )
  }

  private def resolveArgument(
    argument: IR.DefinitionArgument
  ): IR.DefinitionArgument =
    argument match {
      case specified @ IR.DefinitionArgument.Specified(
            _,
            Some(ascribedType),
            _,
            _,
            _,
            _,
            _
          ) =>
        val sig = resolveExpression(ascribedType.duplicate())
        specified.copy(
          name = specified.name.updateMetadata(this -->> Signature(sig)),
          ascribedType =
            Some(ascribedType.updateMetadata(this -->> Signature(sig)))
        )
      case argument => argument
    }

  /** Resolves type signatures in an ascription.
    *
    * @param sig the signature to convert
    * @return the typed expression in `sig`, with `signature` attached
    */
  private def resolveAscription(sig: IR.Type.Ascription): IR.Expression = {
    val newTyped = sig.typed.mapExpressions(resolveExpression)
    val newSig   = sig.signature.mapExpressions(resolveExpression)
    newTyped.updateMetadata(this -->> Signature(newSig))
  }

  /** Resolves type signatures in a block.
    *
    * @param block the block to resolve signatures in
    * @return `block`, with any type signatures resolved
    */
  private def resolveBlock(block: IR.Expression.Block): IR.Expression.Block = {
    var lastSignature: Option[IR.Type.Ascription] = None
    val allBlockExpressions =
      block.expressions :+ block.returnValue

    val newExpressions = allBlockExpressions.flatMap {
      case sig: IR.Type.Ascription =>
        val res = lastSignature match {
          case Some(oldSig) => Some(IR.Error.Unexpected.TypeSignature(oldSig))
          case None         => None
        }

        lastSignature = Some(sig)
        res
      case binding: IR.Expression.Binding =>
        val newBinding = binding.mapExpressions(resolveExpression)
        val res = lastSignature match {
          case Some(asc @ IR.Type.Ascription(typed, sig, _, _, _)) =>
            val name = binding.name
            val newBindingWithDoc = asc
              .getMetadata(DocumentationComments)
              .map(doc =>
                newBinding.updateMetadata(DocumentationComments -->> doc)
              )
              .getOrElse(newBinding)

            typed match {
              case typedName: IR.Name =>
                if (typedName.name == name.name) {
                  Some(
                    newBindingWithDoc.updateMetadata(this -->> Signature(sig))
                  )
                } else {
                  List(
                    IR.Error.Unexpected.TypeSignature(asc),
                    newBindingWithDoc
                  )
                }
              case _ =>
                List(
                  IR.Error.Unexpected.TypeSignature(asc),
                  newBindingWithDoc
                )
            }
          case None => Some(newBinding)
        }

        lastSignature = None
        res
      case a => Some(resolveExpression(a))
    } ::: lastSignature.map(IR.Error.Unexpected.TypeSignature(_)).toList

    block.copy(
      expressions = newExpressions.init,
      returnValue = newExpressions.last
    )
  }

  // === Metadata =============================================================

  /** A representation of a type signature.
    *
    * @param signature the expression for the type signature
    */
  case class Signature(signature: IR.Expression) extends IRPass.Metadata {
    override val metadataName: String = "TypeSignatures.Signature"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): Signature = {
      signature.preorder.foreach(_.passData.prepareForSerialization(compiler))
      this
    }

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[Signature] = {
      signature.preorder.foreach { node =>
        if (!node.passData.restoreFromSerialization(compiler)) {
          return None
        }
      }
      Some(this)
    }

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.Metadata] =
      Some(this.copy(signature = signature.duplicate()))
  }
}
