package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.expression.{errors, Comment, Error}
import org.enso.compiler.core.CompilerError
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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = resolveModule(ir)

  /** Resolves type signatures in an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = resolveExpression(ir)

  /** @inheritdoc */

  // === Pass Internals =======================================================

  /** Resolves type signatures in a module.
    *
    * @param mod the module to resolve signatures in
    * @return `mod`, with type signatures resolved
    */
  private def resolveModule(mod: Module): Module = {
    var lastSignature: Option[Type.Ascription] = None

    val newBindings: List[Definition] = mod.bindings.flatMap {
      case sig: Type.Ascription =>
        val res = lastSignature.map(errors.Unexpected.TypeSignature(_))
        lastSignature = Some(sig)
        res
      case meth: definition.Method =>
        val newMethod = meth.mapExpressions(resolveExpression)
        newMethod.body.preorder.foreach {
          case fn: Function => verifyAscribedArguments(fn.arguments)
          case _            =>
        }
        val res = lastSignature match {
          case Some(asc @ Type.Ascription(typed, sig, _, _, _)) =>
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
              case ref: Name.MethodReference =>
                if (ref isSameReferenceAs methodRef) {
                  Some(
                    newMethodWithAnnotations.updateMetadata(
                      this -->> Signature(sig)
                    )
                  )
                } else {
                  List(
                    errors.Unexpected.TypeSignature(asc),
                    newMethodWithAnnotations
                  )
                }
              case _ =>
                List(
                  errors.Unexpected.TypeSignature(asc),
                  newMethodWithAnnotations
                )
            }
          case None => Some(newMethod)
        }

        lastSignature = None
        res
      case ut: Definition.Type =>
        ut.members.foreach(d => verifyAscribedArguments(d.arguments))
        Some(
          ut
            .copy(
              params  = ut.params.map(resolveArgument),
              members = ut.members.map(resolveDefinitionData)
            )
            .mapExpressions(resolveExpression)
        )
      case err: Error                  => Some(err)
      case ann: Name.GenericAnnotation => Some(ann)
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present during type " +
          "signature resolution."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "type signature resolution."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation comments should not be present during type " +
          "signature resolution."
        )
    } ::: lastSignature
      .map(asc => errors.Unexpected.TypeSignature(asc))
      .toList

    mod.copy(
      bindings = newBindings
    )
  }

  /** Attaches {@link Signature} to each arguments of a function
    * with ascribed type for correct resolution by {@link TypesNames}
    * pass.
    *
    * @param fn the function to check arguments for
    */
  private def verifyAscribedArguments(
    arguments: List[DefinitionArgument]
  ): Unit = {
    arguments.foreach(arg =>
      arg.ascribedType.map(t => arg.updateMetadata(this -->> Signature(t)))
    )
  }

  /** Resolves type signatures in an arbitrary expression.
    *
    * @param expr the expression to resolve signatures in
    * @return `expr`, with any type signatures resolved
    */
  private def resolveExpression(expr: Expression): Expression = {
    expr.transformExpressions {
      case block: Expression.Block => resolveBlock(block)
      case sig: Type.Ascription    => resolveAscription(sig)
    }
  }

  private def resolveDefinitionData(
    data: Definition.Data
  ): Definition.Data = {
    data.copy(
      arguments = data.arguments.map(resolveArgument)
    )
  }

  private def resolveArgument(
    argument: DefinitionArgument
  ): DefinitionArgument =
    argument match {
      case specified @ DefinitionArgument.Specified(
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
  private def resolveAscription(sig: Type.Ascription): Expression = {
    val newTyped = sig.typed.mapExpressions(resolveExpression)
    val newSig   = sig.signature.mapExpressions(resolveExpression)
    newTyped.updateMetadata(this -->> Signature(newSig))
  }

  /** Resolves type signatures in a block.
    *
    * @param block the block to resolve signatures in
    * @return `block`, with any type signatures resolved
    */
  private def resolveBlock(block: Expression.Block): Expression.Block = {
    var lastSignature: Option[Type.Ascription] = None
    val allBlockExpressions =
      block.expressions :+ block.returnValue

    val newExpressions = allBlockExpressions.flatMap {
      case sig: Type.Ascription =>
        val res = lastSignature match {
          case Some(oldSig) => Some(errors.Unexpected.TypeSignature(oldSig))
          case None         => None
        }

        lastSignature = Some(sig)
        res
      case binding: Expression.Binding =>
        val newBinding = binding.mapExpressions(resolveExpression)
        val res = lastSignature match {
          case Some(asc @ Type.Ascription(typed, sig, _, _, _)) =>
            val name = binding.name
            val newBindingWithDoc = asc
              .getMetadata(DocumentationComments)
              .map(doc =>
                newBinding.updateMetadata(DocumentationComments -->> doc)
              )
              .getOrElse(newBinding)

            typed match {
              case typedName: Name =>
                if (typedName.name == name.name) {
                  Some(
                    newBindingWithDoc.updateMetadata(this -->> Signature(sig))
                  )
                } else {
                  List(
                    errors.Unexpected.TypeSignature(asc),
                    newBindingWithDoc
                  )
                }
              case _ =>
                List(
                  errors.Unexpected.TypeSignature(asc),
                  newBindingWithDoc
                )
            }
          case None => Some(newBinding)
        }

        lastSignature = None
        res
      case a => Some(resolveExpression(a))
    } ::: lastSignature.map(errors.Unexpected.TypeSignature(_)).toList

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
  case class Signature(signature: Expression) extends IRPass.IRMetadata {
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
    override def duplicate(): Option[IRPass.IRMetadata] =
      Some(this.copy(signature = signature.duplicate()))
  }
}
