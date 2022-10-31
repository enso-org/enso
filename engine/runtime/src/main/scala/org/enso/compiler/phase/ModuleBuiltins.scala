package org.enso.compiler.phase

import org.enso.compiler.core.IR
import org.enso.compiler.pass.resolve.ExpressionAnnotations
import org.enso.interpreter.Language
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.Module

import scala.jdk.CollectionConverters.CollectionHasAsScala

class ModuleBuiltins(builtins: Builtins) {

  /** Adds implicit builtin methods definitions for the given module.
    * The method only adds definitions of builtins for which no explicit declaration exists.
    * Only static builtins of the module are considered, builtins of a type are registered during Builtins
    * initialization.
    *
    * @param module module to which definitions are to be added
    * @param ir current IR representation of the `module`
    * @param language language of the context
    * @return enhanced IR with potentially new builtin method definitions
    */
  def inject(module: Module, ir: IR.Module, language: Language): IR.Module = {
    val modName = module.getName.item
    val allBuiltins = builtins
      .getBuiltinFunctionsForModule(modName, language)
      .asScala

    if (allBuiltins.nonEmpty) {
      val existingMethodBindings = ir.bindings.collect {
        case binding: IR.Module.Scope.Definition.Method.Binding =>
          binding.methodName.name
      }
      val relevantBuiltins = allBuiltins.filterNot(builtinMeta =>
        existingMethodBindings.contains(builtinMeta.getFunctionName)
      )
      val implicitMethods = relevantBuiltins.map { builtinMeta =>
        val name = builtinMeta.getFunctionName
        val annotation = IR.Application.Prefix(
          IR.Name.Annotation(
            ExpressionAnnotations.builtinMethodName,
            location = None
          ),
          arguments = List(
            IR.CallArgument.Specified(
              name = None,
              value =
                IR.Literal.Text(text = s"$modName.$name", location = None),
              location = None
            )
          ),
          hasDefaultsSuspended = false,
          location             = None
        )
        val body: IR.Expression =
          IR.Function.Lambda(
            arguments = Nil,
            body      = annotation,
            location  = None
          )
        val methodReference: IR.Name.MethodReference =
          IR.Name.MethodReference(
            typePointer = None,
            methodName =
              IR.Name.Literal(name, isMethod = true, location = None),
            location = None
          )

        val definitionArgs =
          builtinMeta.getFunction.getSchema.getArgumentInfos.toList
            .drop(1)
            .map(arg =>
              IR.DefinitionArgument.Specified(
                name = IR.Name
                  .Literal(arg.getName, isMethod = false, location = None),
                ascribedType = None,
                defaultValue = None,
                suspended    = arg.isSuspended,
                location     = None
              )
            )
        IR.Module.Scope.Definition.Method
          .Binding(methodReference, definitionArgs, body, location = None)
      }

      ir.copy(bindings = ir.bindings ++ implicitMethods)
    } else ir
  }
}
