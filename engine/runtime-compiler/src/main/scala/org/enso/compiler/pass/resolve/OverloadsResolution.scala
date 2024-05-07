package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  Diagnostic,
  Expression,
  IdentifiedLocation,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.expression.{errors, Comment}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.CompilerError
import org.enso.compiler.data.BindingsMap.ModuleReference
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{ComplexType, GenerateMethodBodies}

import scala.collection.mutable

/** This pass performs static detection of method overloads and emits errors
  * at the overload definition site if they are detected. It also checks for
  * overloads of atom contructors using the same rule.
  *
  * Method resolution proceeds in the following order:
  *
  * 1. Methods defined directly on the atom are resolved first.
  * 2. Extension methods in the current scope are resolved next.
  * 3. Extension methods in imported scopes are resolved last.
  *
  * It is not possible to define two extension methods with same name in different
  * location - that results in an ambiguity error. Note that an *extension* method
  * is a *static method* that is defined outside a type, for example in the following snippet,
  * `foo` is an extension method, and `bar` is a static method:
  * ```
  * type T
  *     bar = 42 # A static method
  * T.foo = 23 # An extension method
  * ```
  * Extension method is also a static method. But a static method is not necessarily an extension method.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object OverloadsResolution extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    GenerateMethodBodies
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List()

  /** Performs static detection of method overloads within a given module.
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
    var seenTypes: Set[String] = Set()
    // Key is an optional name of a type, values are set of tuples (method name, isStatic)
    // seenMethods are only the methods defined in the current module.
    var seenMethods: Map[Option[String], Set[(String, Boolean)]] = Map()

    val types = ir.bindings.collect { case tp: Definition.Type =>
      tp
    }
    ir.bindings.collect { case meth: definition.Method.Explicit =>
      seenMethods += meth.typeName.map(_.name) -> Set()
      meth
    }
    val conversionsForType: mutable.Map[Option[String], Set[String]] =
      mutable.Map()

    val newBindings = ir.bindings.map {
      case tp: Definition.Type =>
        val typeName = tp.name.name
        if (seenTypes.contains(typeName)) {
          errors.Redefined.Type(tp.name, tp.location)
        } else {
          seenTypes += typeName
          tp
        }

      case method: definition.Method.Explicit =>
        if (
          seenMethods(method.typeName.map(_.name))
            .contains((method.methodName.name, method.isStatic))
        ) {
          errors.Redefined
            .Method(method.typeName, method.methodName, method.location)
        } else {
          types.find(_.name.name.equals(method.methodName.name)) match {
            case Some(clashedAtom) if method.typeName.isEmpty =>
              errors.Redefined.MethodClashWithAtom(
                clashedAtom.name,
                method.methodName,
                method.location
              )
            case _ =>
              // Also check if some of the imported modules define the same method.
              // Note that we are not checking instance methods here on purpose. If `method`
              // is an instance method, it means that it is inside a type definition. And
              // types can be shadowed. Therefore, instance methods cannot be ambiguous.
              if (
                isExtensionMethod(
                  method,
                  ir.bindings
                ) && isExtensionMethodDefinedInImportedModule(
                  method,
                  moduleContext
                )
              ) {
                errors.Redefined
                  .Method(method.typeName, method.methodName, method.location)
              } else {
                val currentMethods: Set[(String, Boolean)] =
                  seenMethods(method.typeName.map(_.name))
                seenMethods += (method.typeName.map(_.name) ->
                (currentMethods + ((method.methodName.name, method.isStatic))))
                method
              }
          }
        }

      case m: definition.Method.Conversion =>
        val fromName = m.sourceTypeName.asInstanceOf[Name]
        conversionsForType.get(m.typeName.map(_.name)) match {
          case Some(elems) =>
            if (elems.contains(fromName.name)) {
              errors.Redefined.Conversion(m.typeName, fromName, m.location)
            } else {
              conversionsForType.update(
                m.typeName.map(_.name),
                conversionsForType(m.typeName.map(_.name)) + fromName.name
              )
              m
            }
          case None =>
            conversionsForType.put(m.typeName.map(_.name), Set(fromName.name))
            m
        }

      case diagnostic: Diagnostic      => diagnostic
      case ann: Name.GenericAnnotation => ann
      case _: Type.Ascription =>
        throw new CompilerError(
          "Type ascriptions should not be present during the overloads resolution."
        )
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Method bindings should not be present during the overloads resolution."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Builtin annotations should not be present during the overloads resolution."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation comments should not be present during the overloads resolution."
        )
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Sugared types should not be present during the overloads resolution."
        )
    }

    ir.copy(bindings = newBindings)
  }

  /** This pass does nothing for the expression flow.
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
  ): Expression = ir

  /** Returns true if the given extension or static `method` is already defined in one of
    * the imported modules. That should result in an ambiguity error.
    * @param method Static or extension method to check for redefinition.
    * @param moduleContext Current module context. Imported modules are taken from this context.
    * @return True if the method is already defined in one of the imported modules.
    */
  private def isExtensionMethodDefinedInImportedModule(
    method: definition.Method.Explicit,
    moduleContext: ModuleContext
  ): Boolean = {
    assert(method.isStatic)
    assert(method.typeName.isDefined)
    val typeName   = method.typeName.get.name
    val methodName = method.methodName.name
    val extMethod  = StaticOrExtensionMethod(typeName, methodName)
    val bindingMap = moduleContext.bindingsAnalysis()
    if (bindingMap == null) {
      return false
    }
    val curModuleName = bindingMap.currentModule.getName
    val impTargets    = bindingMap.resolvedImports.map(_.target)
    impTargets.exists { impTarget =>
      impTarget.module match {
        case ModuleReference.Concrete(impModule) =>
          // We can, for example, import a type constructor from current module. So we have
          // to check here if the imported module is not the current module.
          if (!impModule.getName.equals(curModuleName)) {
            val importedExtMethods =
              collectStaticAndExtensionMethods(impModule.getIr.bindings)
            importedExtMethods.contains(extMethod)
          } else {
            false
          }
        case ModuleReference.Abstract(_) =>
          // ModuleReference might not be resolved. In that case, we just return false
          false
      }
    }
  }

  /** Returns true iff the given `methodDef` is an extension method, but not a static method.
    * @param bindings List of bindings in the current module
    */
  private def isExtensionMethod(
    methodDef: definition.Method.Explicit,
    bindings: List[Definition]
  ): Boolean = {
    if (methodDef.isStatic && methodDef.typeName.isDefined) {
      methodDef.methodReference.typePointer match {
        case None => false
        case Some(typePointer) =>
          findType(bindings, typePointer) match {
            case Some(foundType) =>
              !isDefinedInsideType(methodDef, foundType)
            case None =>
              // If the type cannot be found inside current module, the method is definitely an
              // extension method as the method is definitely defined outside the type.
              true
          }
      }
    } else {
      // Non-static method cannot be an extension method.
      false
    }
  }

  /** Collects both static and extension methods from the given list of bindings.
    */
  private def collectStaticAndExtensionMethods(
    bindings: List[Definition]
  ): List[StaticOrExtensionMethod] = {
    bindings.flatMap { binding =>
      binding match {
        case method: definition.Method.Explicit if method.isStatic =>
          method.typeName.map { typeName =>
            StaticOrExtensionMethod(typeName.name, method.methodName.name)
          }
        case _ => None
      }
    }
  }

  /** Tries to find a type definition with the given `typeName` in the given `bindings`.
    */
  private def findType(
    bindings: List[Definition],
    typeName: Name
  ): Option[Definition.Type] = {
    bindings.collectFirst {
      case tp: Definition.Type if areSameNames(typeName, tp.name) =>
        tp
    }
  }

  private def areSameNames(
    firstName: Name,
    secondName: Name
  ): Boolean = {
    (firstName.location(), secondName.location()) match {
      case (Some(firstLoc), Some(secondLoc)) =>
        firstName.name.equals(secondName.name) &&
        firstLoc.start() == secondLoc.start() &&
        firstLoc.end() == secondLoc.end()
      case _ =>
        // If one of the names, or both, do not have an associated location,
        // they are automatically considered different.
        false
    }
  }

  /** Returns true if given `method` is defined inside the given `tp`. In other words,
    * returns true if the given `method` is *static* rather than *extension*.
    * @param method Method to check if it is defined inside the given type.
    * @param tp Type to check if the method is defined inside it.
    * @return True if the method is defined inside the type.
    */
  private def isDefinedInsideType(
    method: definition.Method.Explicit,
    tp: Definition.Type
  ): Boolean = {
    (method.location, tp.location) match {
      case (Some(methodLoc), Some(tpLoc)) =>
        isWithinBounds(methodLoc, tpLoc)
      case _ => false
    }
  }

  /** Returns true iff `innerLoc` is within the bounds of `outerLoc`, i.e., if `innerLoc`
    * is contained inside `outerLoc`.
    */
  private def isWithinBounds(
    innerLoc: IdentifiedLocation,
    outerLoc: IdentifiedLocation
  ): Boolean = {
    innerLoc.end() <= outerLoc.end() &&
    outerLoc.start() <= innerLoc.start() &&
    innerLoc.start() <= outerLoc.end()
  }

  private case class StaticOrExtensionMethod(
    typeName: String,
    methodName: String
  )
}
