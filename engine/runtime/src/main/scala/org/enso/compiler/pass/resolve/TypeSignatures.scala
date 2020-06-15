package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.lint.UnusedBindings

import scala.annotation.unused

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
    TypeFunctions
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
    @unused moduleContext: ModuleContext
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
    @unused inlineContext: InlineContext
  ): IR.Expression = resolveExpression(ir)

  // === Pass Internals =======================================================

  /** Resolves type signatures in a module.
    *
    * @param mod the module to resolve signatures in
    * @return `mod`, with type signatures resolved
    */
  def resolveModule(mod: IR.Module): IR.Module = {
    var lastSignature: Option[IR.Type.Ascription] = None

    val newBindings: List[IR.Module.Scope.Definition] = mod.bindings.flatMap {
        case sig: IR.Type.Ascription =>
          val res = lastSignature match {
            case Some(oldSig) => Some(IR.Error.Unexpected.TypeSignature(oldSig))
            case None         => None
          }

          lastSignature = Some(sig)
          res
        case meth: IR.Module.Scope.Definition.Method =>
          val newMethod = meth.mapExpressions(resolveExpression)
          val res = lastSignature match {
            case Some(asc @ IR.Type.Ascription(typed, sig, _, _, _)) =>
              val methodRef = meth.methodReference

              typed match {
                case ref: IR.Name.MethodReference =>
                  if (ref isSameReferenceAs methodRef) {
                    Some(newMethod.updateMetadata(this -->> Signature(sig)))
                  } else {
                    List(IR.Error.Unexpected.TypeSignature(asc), newMethod)
                  }
                case _ =>
                  List(IR.Error.Unexpected.TypeSignature(asc), newMethod)
              }
            case None => Some(newMethod)
          }

          lastSignature = None
          res
        case atom: IR.Module.Scope.Definition.Atom =>
          Some(atom.mapExpressions(resolveExpression))
        case err: IR.Error => Some(err)
        case _: IR.Module.Scope.Definition.Type =>
          throw new CompilerError(
            "Complex type definitions should not be present during type " +
            "signature resolution."
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
  def resolveExpression(expr: IR.Expression): IR.Expression = {
    expr.transformExpressions {
      case block: IR.Expression.Block => resolveBlock(block)
      case sig: IR.Type.Ascription    => resolveAscription(sig)
    }
  }

  /** Resolves type signatures in an ascription.
    *
    * @param sig the signature to convert
    * @return the typed expression in `sig`, with `signature` attached
    */
  def resolveAscription(sig: IR.Type.Ascription): IR.Expression = {
    val newTyped = sig.typed.mapExpressions(resolveExpression)
    val newSig   = sig.signature.mapExpressions(resolveExpression)
    newTyped.updateMetadata(this -->> Signature(newSig))
  }

  /** Resolves type signatures in a block.
    *
    * @param block the block to resolve signatures in
    * @return `block`, with any type signatures resolved
    */
  def resolveBlock(block: IR.Expression.Block): IR.Expression.Block = {
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

              typed match {
                case typedName: IR.Name =>
                  if (typedName.name == name.name) {
                    Some(newBinding.updateMetadata(this -->> Signature(sig)))
                  } else {
                    List(IR.Error.Unexpected.TypeSignature(asc), newBinding)
                  }
                case _ =>
                  List(IR.Error.Unexpected.TypeSignature(asc), newBinding)
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
  }
}
