package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.ModuleReference
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{
  ComplexType,
  FunctionBinding,
  GenerateMethodBodies
}
import org.enso.compiler.pass.resolve.{
  MethodDefinitions,
  ModuleAnnotations,
  Patterns
}

import scala.annotation.unused

/** Recognizes all defined bindings in the current module and constructs
  * a mapping data structure that can later be used for symbol resolution.
  */
case object BindingAnalysis extends IRPass {

  override type Metadata = BindingsMap

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(ComplexType, FunctionBinding, GenerateMethodBodies)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] =
    Seq(MethodDefinitions, Patterns)

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir            the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val definedSumTypes = ir.bindings.collect {
      case sumType: IR.Module.Scope.Definition.UnionType =>
        BindingsMap.Type(sumType.name.name, sumType.members.map(_.name))
    }

    val definedConstructors = ir.bindings.collect {
      case cons: IR.Module.Scope.Definition.Atom =>
        // FIXME: move to a different pass
        val isBuiltinType = cons
          .getMetadata(ModuleAnnotations)
          .exists(_.annotations.exists(_.name == "@Builtin_Type"))
        BindingsMap.Cons(
          cons.name.name,
          cons.arguments.length,
          cons.arguments.forall(_.defaultValue.isDefined),
          isBuiltinType
        )
    }
    val importedPolyglot = ir.imports.collect {
      case poly: IR.Module.Scope.Import.Polyglot =>
        BindingsMap.PolyglotSymbol(poly.getVisibleName)
    }
    val moduleMethods = ir.bindings
      .collect { case method: IR.Module.Scope.Definition.Method.Explicit =>
        val ref = method.methodReference
        ref.typePointer match {
          case Some(IR.Name.Qualified(List(), _, _, _)) =>
            Some(ref.methodName.name)
          case Some(IR.Name.Qualified(List(n), _, _, _)) =>
            val shadowed = definedConstructors.exists(_.name == n.name)
            if (!shadowed && n.name == moduleContext.module.getName.item)
              Some(ref.methodName.name)
            else None
          case Some(IR.Name.Literal(n, _, _, _, _)) =>
            val shadowed = definedConstructors.exists(_.name == n)
            if (!shadowed && n == moduleContext.module.getName.item)
              Some(ref.methodName.name)
            else None
          case None => Some(ref.methodName.name)
          case _    => None
        }
      }
      .flatten
      .map(BindingsMap.ModuleMethod)
    val methodsWithAutogen =
      BindingsMap.ModuleMethod(
        BindingsMap.Generated.ensoProjectMethodName
      ) :: moduleMethods
    ir.updateMetadata(
      this -->> BindingsMap(
        definedSumTypes,
        definedConstructors,
        importedPolyglot,
        methodsWithAutogen,
        ModuleReference.Concrete(moduleContext.module)
      )
    )
  }

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir            the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
