package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.AscriptionReason
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
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRProcessingPass
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.lint.UnusedBindings

import java.util.function.Consumer

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

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    TypeFunctions,
    ModuleAnnotations
  )
  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List(
    AliasAnalysis,
    CachePreferenceAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    org.enso.compiler.pass.analyse.TailCall.INSTANCE,
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
  ): Module = {
    val scopeMap = moduleContext.bindingsAnalysis()
    resolveModule(
      moduleContext,
      ir,
      scopeMap
        .resolveQualifiedName(List("Standard", "Base", "Any", "Any"))
        .isRight
    )
  }

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
  private def resolveModule(
    moduleContext: ModuleContext,
    mod: Module,
    canResolveAny: Boolean
  ): Module = {
    var lastSignature: Option[Type.Ascription] = None

    val newBindings: List[Definition] = mod.bindings.flatMap {
      case sig: Type.Ascription =>
        val res = lastSignature.map(errors.Unexpected.TypeSignature(_))
        lastSignature = Some(sig)
        res
      case meth: definition.Method =>
        val newMethod = meth.mapExpressions(resolveExpression)
        IR.preorder(
          newMethod.body,
          {
            case fn: Function => verifyAscribedArguments(fn.arguments)
            case _            =>
          }: Consumer[IR]
        )

        val res = lastSignature match {
          case Some(asc: Type.Ascription) =>
            val methodRef = meth.methodReference
            val newMethodWithDoc = asc
              .getMetadata(DocumentationComments)
              .map(doc =>
                newMethod.updateMetadata(
                  new MetadataPair(DocumentationComments, doc)
                )
              )
              .getOrElse(newMethod)
            val newMethodWithAnnotations = asc
              .getMetadata(ModuleAnnotations)
              .map(annotations =>
                newMethodWithDoc.updateMetadata(
                  new MetadataPair(ModuleAnnotations, annotations)
                )
              )
              .getOrElse(newMethodWithDoc)

            asc.typed() match {
              case ref: Name.MethodReference =>
                if (ref isSameReferenceAs methodRef) {
                  Some(
                    newMethodWithAnnotations.updateMetadata(
                      new MetadataPair(
                        this,
                        Signature(asc.signature(), asc.reason())
                      )
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
          case None =>
            // No explicit type signature *before* the method was provided.
            // Reconstruct type signature from inlined types in arguments/return type, if present.
            rebuildSignatureFromInlinedTypes(
              moduleContext,
              meth.body,
              canResolveAny
            )
              .filter(_.nonEmpty)
              .foreach { inferred =>
                val typeFun = Type.Function
                  .builder()
                  .args(inferred.init)
                  .result(inferred.last)
                  .build()
                newMethod.updateMetadata(
                  new MetadataPair(this, Signature(typeFun))
                )
              }
            Some(newMethod)
        }
        lastSignature = None
        res
      case ut: Definition.Type =>
        ut.members.foreach(d => verifyAscribedArguments(d.arguments))
        Some(
          ut.copyBuilder()
            .params(ut.params().map(resolveArgument))
            .members(ut.members().map(resolveDefinitionData))
            .build()
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

    mod.copyWithBindings(
      newBindings
    )
  }

  private def rebuildSignatureFromInlinedTypes(
    moduleContext: ModuleContext,
    expr: Expression,
    canResolveAny: Boolean
  ): Option[List[Expression]] = {
    expr match {
      case lambda: Function.Lambda =>
        lambda.arguments match {
          case (defArg: DefinitionArgument.Specified) :: args
              if defArg.name().isInstanceOf[Name.Self] =>
            val bodyTypeArgs =
              rebuildSignatureFromInlinedTypes(
                moduleContext,
                lambda.body,
                canResolveAny
              )
            val argTypes =
              args.flatMap(
                _.getMetadata(this)
                  .map(_.signature)
                  .orElse(
                    if (canResolveAny) Some(moduleContext.anyIr) else None
                  )
              )
            if (argTypes.length == args.length)
              bodyTypeArgs.map(b => argTypes ::: b)
            else
              None
          case args =>
            val bodyTypeArgs =
              rebuildSignatureFromInlinedTypes(
                moduleContext,
                lambda.body,
                canResolveAny
              )
            val argTypes =
              args.flatMap(
                _.getMetadata(this)
                  .map(_.signature)
                  .orElse(
                    if (canResolveAny) Some(moduleContext.anyIr) else None
                  )
              )
            if (argTypes.length == args.length)
              bodyTypeArgs.map(b => argTypes ::: b)
            else
              None
        }
      case _ =>
        expr match {
          case tpe: Type.Ascription =>
            tpe.typed.getMetadata(this).map(_.signature :: Nil)
          case _ =>
            None
        }
    }
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
      arg.ascribedType.map(t =>
        arg.updateMetadata(new MetadataPair(this, Signature(t)))
      )
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
    data.copyWithArguments(
      data.arguments.map(resolveArgument)
    )
  }

  private def resolveArgument(
    argument: DefinitionArgument
  ): DefinitionArgument =
    argument match {
      case specified: DefinitionArgument.Specified
          if specified.ascribedType.isDefined =>
        val ascribedType = specified.ascribedType.get
        val sig          = resolveExpression(ascribedType.duplicate())
        specified.copyWithNameAndAscribedType(
          specified.name.updateMetadata(
            new MetadataPair(this, Signature(sig))
          ),
          Some(
            ascribedType.updateMetadata(new MetadataPair(this, Signature(sig)))
          )
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
    newTyped
      .setLocation(sig.location())
      .updateMetadata(
        new MetadataPair(this, Signature(newSig, sig.reason))
      )
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
          case Some(asc: Type.Ascription) =>
            val name = binding.name
            val newBindingWithDoc = asc
              .getMetadata(DocumentationComments)
              .map(doc =>
                newBinding.updateMetadata(
                  new MetadataPair(DocumentationComments, doc)
                )
              )
              .getOrElse(newBinding)

            asc.typed() match {
              case typedName: Name =>
                if (typedName.name == name.name) {
                  Some(
                    newBindingWithDoc.updateMetadata(
                      new MetadataPair(
                        this,
                        Signature(asc.signature(), asc.reason())
                      )
                    )
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
    } ::: lastSignature
      .map({
        case asc: Type.Ascription =>
          asc.updateMetadata(
            new MetadataPair(this, Signature(asc.signature(), asc.reason()))
          )
        case any => errors.Unexpected.TypeSignature(any)
      })
      .toList

    block.copy(
      expressions = newExpressions.init,
      returnValue = newExpressions.last
    )
  }

  // === Metadata =============================================================

  /** A representation of a type signature.
    *
    * @param signature the expression for the type signature
    * @param reason explaining why we have such a signature
    */
  case class Signature(
    signature: Expression,
    reason: AscriptionReason = AscriptionReason.empty()
  ) extends IRPass.IRMetadata {
    override val metadataName: String = "TypeSignatures.Signature"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): Signature = {
      IR.preorder(signature, _.passData.prepareForSerialization(compiler))
      this
    }

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[Signature] = {
      IR.preorder(
        signature,
        { node =>
          if (!node.passData.restoreFromSerialization(compiler)) {
            return None
          }
        }
      )
      Some(this)
    }

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.IRMetadata] =
      Some(this.copy(signature = signature.duplicate(), reason = reason))
  }
}
