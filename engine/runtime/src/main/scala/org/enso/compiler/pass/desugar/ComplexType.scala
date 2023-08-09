package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.IdentifiedLocation
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.core.IR.Name.MethodReference
import org.enso.compiler.core.ir.{DiagnosticStorage, MetadataStorage}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.{
  ApplicationSaturation,
  LambdaConsolidate
}
import org.enso.compiler.pass.resolve.{
  DocumentationComments,
  IgnoredBindings,
  ModuleAnnotations
}
import org.enso.compiler.core.ir.MetadataStorage._

/** Desugars complex type definitions to simple type definitions in the module
  * scope.
  *
  * Note that this pass currently ignores the creation of a function
  * representing the type (e.g. `maybe a = Nothing | Just a` as this does not
  * have a runtime representation at the current time.
  *
  * This pass has no configuration.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object ComplexType extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(ModuleAnnotations)
  override val invalidatedPasses: Seq[IRPass] =
    List(
      AliasAnalysis,
      ApplicationSaturation,
      DataflowAnalysis,
      DemandAnalysis,
      FunctionBinding,
      GenerateMethodBodies,
      IgnoredBindings,
      LambdaConsolidate,
      LambdaShorthandToLambda,
      NestedPatternMatch,
      OperatorToFunction,
      SectionsToBinOp,
      TailCall,
      UnusedBindings
    )

  /** Performs desugaring of complex type definitions for a module.
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
  ): IR.Module =
    ir.copy(
      bindings = ir.bindings.flatMap {
        case typ: Definition.SugaredType => desugarComplexType(typ)
        case b                           => List(b)
      }
    )

  /** An identity operation on an arbitrary expression.
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
  ): IR.Expression = ir

  // === Pass Internals =======================================================

  /** Desugars a complex type definition into a series of top-level definitions.
    *
    * @param typ the type definition to desugar
    * @return the top-level definitions corresponding to the desugaring of `typ`
    */
  private def desugarComplexType(
    typ: IR.Module.Scope.Definition.SugaredType
  ): List[IR.Module.Scope.Definition] = {
    val annotations     = typ.getMetadata(ModuleAnnotations)
    var lastAnnotations = Seq.empty[IR.Name.GenericAnnotation]
    var seenAnnotations = Set.empty[IR.Name.GenericAnnotation]
    val atomDefs = typ.body
      .flatMap {
        case ann: IR.Name.GenericAnnotation =>
          lastAnnotations :+= ann
          None
        case d: IR.Module.Scope.Definition.Data =>
          val res = Some(d.copy(annotations = d.annotations ++ lastAnnotations))
          seenAnnotations ++= lastAnnotations
          lastAnnotations = Seq()
          res
        case _ =>
          None
      }
      // TODO[MK] this is probably removable
      .map(atom =>
        annotations
          .map(ann => {
            val old = atom
              .getMetadata(ModuleAnnotations)
              .map(_.annotations)
              .getOrElse(Nil)
            atom.updateMetadata(
              ModuleAnnotations -->> ann.copy(ann.annotations ++ old)
            )
          })
          .getOrElse(atom)
      )

    val remainingEntities = typ.body.filterNot {
      case _: IR.Module.Scope.Definition.Data => true
      case ann: IR.Name.GenericAnnotation     => seenAnnotations.contains(ann)
      case _                                  => false
    }

    var lastSignature: Option[IR.Type.Ascription] = None

    /** Pairs up signatures with method definitions, and then generates the
      * appropriate method definitions for the atoms in scope.
      *
      * @param name the name of the method
      * @param defn the definition of the method
      * @return a list of method definitions for `name`
      */
    def matchSignaturesAndGenerate(
      name: IR.Name,
      defn: IR
    ): List[IR.Module.Scope.Definition] = {
      var unusedSig: Option[IR.Type.Ascription] = None
      val sig = lastSignature match {
        case Some(IR.Type.Ascription(typed, _, _, _, _)) =>
          typed match {
            case IR.Name.Literal(nameStr, _, _, _, _) =>
              if (name.name == nameStr) {
                lastSignature
              } else {
                unusedSig = lastSignature
                None
              }
            case _ =>
              unusedSig = lastSignature
              None
          }
        case None => None
      }

      lastSignature = None
      val unusedList: List[Definition] = unusedSig.toList
      unusedList ::: genMethodDef(
        defn,
        typ.name,
        sig
      )
    }

    val entityResults: List[Definition] = remainingEntities.flatMap {
      case sig: IR.Type.Ascription =>
        val res = lastSignature
        lastSignature = Some(sig)
        res
      case binding @ IR.Expression.Binding(name, _, _, _, _) =>
        matchSignaturesAndGenerate(name, binding)
      case funSugar @ IR.Function.Binding(name, _, _, _, _, _, _) =>
        matchSignaturesAndGenerate(name, funSugar)
      case err: IR.Error                  => Seq(err)
      case ann: IR.Name.GenericAnnotation => Seq(ann)
      case _ =>
        throw new CompilerError("Unexpected IR node in complex type body.")
    }
    val allEntities = entityResults ::: lastSignature.toList

    val sumType = IR.Module.Scope.Definition.Type(
      typ.name,
      typ.arguments,
      atomDefs,
      typ.location
    )

    val withAnnotations = annotations
      .map(ann => sumType.updateMetadata(ModuleAnnotations -->> ann))
      .getOrElse(sumType)

    val withDoc = typ
      .getMetadata(DocumentationComments)
      .map(ann =>
        withAnnotations.updateMetadata(DocumentationComments -->> ann)
      )
      .getOrElse(sumType)

    withDoc :: allEntities
  }

  /** Generates a method definition from a definition in complex type def body.
    *
    * The signature _must_ correctly match the method definition.
    *
    * @param ir the definition to generate a method from
    * @param typeName the type name on which the method is being defined
    * @param signature the type signature for the method, if it exists
    * @return `ir` as a method
    */
  private def genMethodDef(
    ir: IR,
    typeName: IR.Name,
    signature: Option[IR.Type.Ascription]
  ): List[IR.Module.Scope.Definition] = {
    ir match {
      case IR.Expression.Binding(name, expr, location, passData, diagnostics) =>
        val realExpr = expr match {
          case b @ IR.Expression.Block(_, _, _, suspended, _, _) if suspended =>
            b.copy(suspended = false)
          case _ => expr
        }

        genForName(
          typeName,
          name,
          List(),
          realExpr,
          location,
          passData,
          diagnostics,
          signature
        )
      case IR.Function.Binding(
            name,
            args,
            body,
            location,
            _,
            passData,
            diagnostics
          ) =>
        genForName(
          typeName,
          name,
          args,
          body,
          location,
          passData,
          diagnostics,
          signature
        )
      case _ =>
        throw new CompilerError(
          "Unexpected IR node during complex type desugaring."
        )
    }
  }

  /** Generates a top-level method definition for the provided parameters.
    *
    * @param typeName the type name the method is being defined on
    * @param name the method being defined
    * @param args the definition arguments to the method
    * @param body the body of the method
    * @param location the source location of the method
    * @param signature the method's type signature, if it exists
    * @return a top-level method definition
    */
  private def genForName(
    typeName: IR.Name,
    name: IR.Name,
    args: List[IR.DefinitionArgument],
    body: IR.Expression,
    location: Option[IdentifiedLocation],
    passData: MetadataStorage,
    diagnostics: DiagnosticStorage,
    signature: Option[IR.Type.Ascription]
  ): List[IR.Module.Scope.Definition] = {
    val methodRef = IR.Name.MethodReference(
      Some(IR.Name.Qualified(List(typeName), typeName.location)),
      name,
      MethodReference.genLocation(List(typeName, name))
    )

    val newSig =
      signature.map(sig => sig.copy(typed = methodRef.duplicate()).duplicate())

    val binding = Method.Binding(
      methodRef.duplicate(),
      args.map(_.duplicate()),
      body.duplicate(),
      location,
      passData.duplicate,
      diagnostics.copy
    )

    newSig.toList :+ binding
  }
}
