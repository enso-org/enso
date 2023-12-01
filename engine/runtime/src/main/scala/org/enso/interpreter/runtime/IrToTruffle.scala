package org.enso.interpreter.runtime

import com.oracle.truffle.api.source.{Source, SourceSection}
import com.oracle.truffle.api.interop.InteropLibrary
import org.enso.compiler.context.CompilerContext
import org.enso.compiler.context.FramePointer
import org.enso.compiler.context.LocalScope
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.ConstantsNames
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Empty,
  Expression,
  Function,
  IdentifiedLocation,
  Literal,
  Module,
  Name,
  Pattern,
  `type`,
  Type => Tpe
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.module.scope.imports
import org.enso.compiler.core.ir.Name.Special
import org.enso.compiler.core.ir.expression.{
  errors,
  Application,
  Case,
  Comment,
  Error,
  Foreign,
  Operator,
  Section
}
import org.enso.compiler.data.BindingsMap.{
  ExportedModule,
  ResolvedConstructor,
  ResolvedModule
}
import org.enso.compiler.data.{BindingsMap, CompilerConfig}
import org.enso.compiler.exception.BadPatternMatch
import org.enso.compiler.pass.analyse.AliasAnalysis.Graph.{Scope => AliasScope}
import org.enso.compiler.pass.analyse.AliasAnalysis.{Graph => AliasGraph}
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  BindingAnalysis,
  DataflowAnalysis,
  TailCall
}
import org.enso.compiler.pass.resolve.{
  ExpressionAnnotations,
  GenericAnnotations,
  GlobalNames,
  MethodDefinitions,
  Patterns,
  TypeNames,
  TypeSignatures
}
import org.enso.polyglot.ForeignLanguage
import org.enso.interpreter.node.callable.argument.ReadArgumentNode
import org.enso.interpreter.node.callable.argument.ReadArgumentCheckNode
import org.enso.interpreter.node.callable.function.{
  BlockNode,
  CreateFunctionNode
}
import org.enso.interpreter.node.callable.thunk.{CreateThunkNode, ForceNode}
import org.enso.interpreter.node.callable.{
  ApplicationNode,
  InvokeCallableNode,
  SequenceLiteralNode
}
import org.enso.interpreter.node.controlflow.caseexpr._
import org.enso.interpreter.node.expression.atom.{
  ConstantNode,
  QualifiedAccessorNode
}
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode
import org.enso.interpreter.node.expression.constant._
import org.enso.interpreter.node.expression.foreign.ForeignMethodCallNode
import org.enso.interpreter.node.expression.literal.LiteralNode
import org.enso.interpreter.node.scope.{AssignmentNode, ReadLocalVariableNode}
import org.enso.interpreter.node.{
  BaseNode,
  ClosureRootNode,
  MethodRootNode,
  ExpressionNode => RuntimeExpression
}
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.runtime.callable
import org.enso.interpreter.runtime.callable.argument.{ArgumentDefinition}
import org.enso.interpreter.runtime.callable.atom.{Atom, AtomConstructor}
import org.enso.interpreter.runtime.callable.function.{
  FunctionSchema,
  Function => RuntimeFunction
}
import org.enso.interpreter.runtime.callable.{
  UnresolvedConversion,
  UnresolvedSymbol,
  Annotation => RuntimeAnnotation
}
import org.enso.interpreter.runtime.data.Type
import org.enso.interpreter.runtime.data.text.Text
import org.enso.interpreter.runtime.scope.{ModuleScope}
import org.enso.interpreter.{Constants, EnsoLanguage}

import java.math.BigInteger
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

/** This is an implementation of a codegeneration pass that lowers the Enso
  * [[IR]] into the truffle structures that are actually executed.
  *
  * It should be noted that, as is, there is no support for cross-module links,
  * with each lowering pass operating solely on a single module.
  *
  * @param context        the language context instance for which this is executing
  * @param source         the source code that corresponds to the text for which code
  *                       is being generated
  * @param moduleScope    the scope of the module for which code is being generated
  * @param compilerConfig the configuration for the compiler
  */
class IrToTruffle(
  val context: EnsoContext,
  val source: Source,
  val moduleScope: ModuleScope,
  val compilerConfig: CompilerConfig
) {

  val language: EnsoLanguage = context.getLanguage

  // ==========================================================================
  // === Top-Level Runners ====================================================
  // ==========================================================================

  /** Executes the codegen pass on the input [[IR]].
    *
    * Please note that the IR passed to this function should not contain _any_
    * errors (members of [[Error]]). These must be dealt with and reported
    * before codegen runs, as they will cause a compiler error.
    *
    * In future, this restriction will be relaxed to admit errors that are
    * members of [[org.enso.compiler.core.ir.Diagnostic.Kind.Interactive]], such that we can display
    * these to users during interactive execution.
    *
    * @param ir the IR to generate code for
    */
  def run(ir: Module): Unit = processModule(ir)

  /** Executes the codegen pass on an inline input.
    *
    * @param ir         the IR to generate code for
    * @param localScope the scope in which the inline input exists
    * @param scopeName  the name of `localScope`
    * @return an truffle expression representing `ir`
    */
  def runInline(
    ir: Expression,
    localScope: LocalScope,
    scopeName: String
  ): RuntimeExpression = {
    new ExpressionProcessor(localScope, scopeName).runInline(ir)
  }

  // ==========================================================================
  // === IR Processing Functions ==============================================
  // ==========================================================================

  /** Generates truffle nodes from the top-level definitions of an Enso module
    * and registers these definitions in scope in the compiler.
    *
    * It does not directly return any constructs, but instead registers these
    * constructs for later access in the compiler and language context.
    *
    * @param module the module for which code should be generated
    */
  private def processModule(module: Module): Unit = {
    generateReExportBindings(module)
    val bindingsMap =
      module
        .unsafeGetMetadata(
          BindingAnalysis,
          "No binding analysis at the point of codegen."
        )

    bindingsMap.resolvedExports
      .collect { case ExportedModule(ResolvedModule(module), _, _) =>
        module
      }
      .foreach { exp =>
        moduleScope.addExport(
          asScope(
            exp
              .unsafeAsModule()
          )
        )
      }
    val importDefs = module.imports
    val methodDefs = module.bindings.collect {
      case method: definition.Method.Explicit => method
    }

    bindingsMap.resolvedImports.foreach { imp =>
      imp.target match {
        case BindingsMap.ResolvedType(_, _) =>
        case ResolvedModule(module) =>
          val mod = module
            .unsafeAsModule()
          val scope: ModuleScope = imp.importDef.onlyNames
            .map(only => {
              val requestedTypes = only.map(_.name).asJava
              if (requestedTypes.isEmpty()) {
                asScope(mod)
              } else {
                asScope(mod).withTypes(requestedTypes)
              }
            })
            .getOrElse(asScope(mod))
          moduleScope.addImport(scope)
      }
    }

    // Register the imports in scope
    importDefs.foreach {
      case poly @ imports.Polyglot(i: imports.Polyglot.Java, _, _, _, _) =>
        val hostSymbol = context.lookupJavaClass(i.getJavaName)
        if (hostSymbol != null) {
          this.moduleScope.registerPolyglotSymbol(
            poly.getVisibleName,
            hostSymbol
          )
        } else {
          throw new CompilerError(
            s"Incorrect polyglot import: Cannot find host symbol (Java class) '${i.getJavaName}'"
          )
        }
      case _: Import.Module =>
      case _: Error         =>
    }

    val typeDefs = module.bindings.collect { case tp: Definition.Type => tp }
    typeDefs.foreach { tpDef =>
      // Register the atoms and their constructors in scope
      val atomDefs = tpDef.members
      val asType   = moduleScope.getTypes.get(tpDef.name.name)
      val atomConstructors =
        atomDefs.map(cons => asType.getConstructors.get(cons.name.name))
      atomConstructors
        .zip(atomDefs)
        .foreach { case (atomCons, atomDefn) =>
          val scopeInfo = atomDefn
            .unsafeGetMetadata(
              AliasAnalysis,
              "No root scope on an atom definition."
            )
            .unsafeAs[AliasAnalysis.Info.Scope.Root]

          val dataflowInfo = atomDefn.unsafeGetMetadata(
            DataflowAnalysis,
            "No dataflow information associated with an atom."
          )
          val localScope = new LocalScope(
            None,
            scopeInfo.graph,
            scopeInfo.graph.rootScope,
            dataflowInfo
          )

          val argFactory =
            new DefinitionArgumentProcessor(
              scope = localScope
            )
          val argDefs =
            new Array[ArgumentDefinition](atomDefn.arguments.size)
          val argumentExpressions =
            new ArrayBuffer[(RuntimeExpression, RuntimeExpression)]

          for (idx <- atomDefn.arguments.indices) {
            val unprocessedArg = atomDefn.arguments(idx)
            val checkNode      = checkAsTypes(unprocessedArg)
            val arg            = argFactory.run(unprocessedArg, idx, checkNode)
            val occInfo = unprocessedArg
              .unsafeGetMetadata(
                AliasAnalysis,
                "No occurrence on an argument definition."
              )
              .unsafeAs[AliasAnalysis.Info.Occurrence]
            val slotIdx = localScope.getVarSlotIdx(occInfo.id)
            argDefs(idx) = arg
            val readArg =
              ReadArgumentNode.build(
                idx,
                arg.getDefaultValue.orElse(null),
                checkNode
              )
            val assignmentArg = AssignmentNode.build(readArg, slotIdx)
            val argRead =
              ReadLocalVariableNode.build(new FramePointer(0, slotIdx))
            argumentExpressions.append((assignmentArg, argRead))
          }

          val (assignments, reads) = argumentExpressions.unzip
          // build annotations
          val annotations = atomDefn.annotations.map { annotation =>
            val scopeElements = Seq(
              tpDef.name.name,
              atomDefn.name.name,
              annotation.name
            )
            val scopeName =
              scopeElements.mkString(Constants.SCOPE_SEPARATOR)
            val expressionProcessor = new ExpressionProcessor(
              scopeName,
              scopeInfo.graph,
              scopeInfo.graph.rootScope,
              dataflowInfo
            )
            val expressionNode =
              expressionProcessor.run(annotation.expression, true)
            val closureName = s"<default::$scopeName>"
            val closureRootNode = ClosureRootNode.build(
              language,
              expressionProcessor.scope,
              moduleScope,
              expressionNode,
              makeSection(moduleScope, annotation.location),
              closureName,
              true,
              false
            )
            new RuntimeAnnotation(annotation.name, closureRootNode)
          }
          if (!atomCons.isInitialized) {
            atomCons.initializeFields(
              language,
              makeSection(moduleScope, atomDefn.location),
              localScope,
              assignments.toArray,
              reads.toArray,
              annotations.toArray,
              argDefs: _*
            )
          }
        }
      asType.generateGetters(language)
    }

    // Register the method definitions in scope
    methodDefs.foreach(methodDef => {
      val scopeInfo = methodDef
        .unsafeGetMetadata(
          AliasAnalysis,
          s"Missing scope information for method " +
          s"`${methodDef.typeName.map(_.name + ".").getOrElse("")}${methodDef.methodName.name}`."
        )
        .unsafeAs[AliasAnalysis.Info.Scope.Root]
      val dataflowInfo = methodDef.unsafeGetMetadata(
        DataflowAnalysis,
        "Method definition missing dataflow information."
      )

      @tailrec
      def getContext(tp: Expression): Option[String] = tp match {
        case fn: Tpe.Function => getContext(fn.result)
        case ctx: Tpe.Context =>
          ctx.context match {
            case lit: Name.Literal => Some(lit.name)
            case _                 => None
          }
        case _ => None
      }

      val effectContext = methodDef
        .getMetadata(TypeSignatures)
        .flatMap(sig => getContext(sig.signature))

      val declaredConsOpt =
        methodDef.methodReference.typePointer match {
          case None =>
            Some(moduleScope.getAssociatedType)
          case Some(tpePointer) =>
            tpePointer
              .getMetadata(MethodDefinitions)
              .map { res =>
                res.target match {
                  case BindingsMap.ResolvedType(module, tp) =>
                    asScope(
                      module
                        .unsafeAsModule()
                    ).getTypes
                      .get(tp.name)
                  case BindingsMap.ResolvedModule(module) =>
                    asScope(
                      module
                        .unsafeAsModule()
                    ).getAssociatedType
                  case BindingsMap.ResolvedConstructor(_, _) =>
                    throw new CompilerError(
                      "Impossible, should be caught by MethodDefinitions pass"
                    )
                  case BindingsMap.ResolvedPolyglotSymbol(_, _) =>
                    throw new CompilerError(
                      "Impossible polyglot symbol, should be caught by MethodDefinitions pass."
                    )
                  case BindingsMap.ResolvedPolyglotField(_, _) =>
                    throw new CompilerError(
                      "Impossible polyglot field, should be caught by MethodDefinitions pass."
                    )
                  case _: BindingsMap.ResolvedMethod =>
                    throw new CompilerError(
                      "Impossible here, should be caught by MethodDefinitions pass."
                    )
                }
              }
        }

      val consOpt = declaredConsOpt.map { c =>
        if (methodDef.isStatic) {
          c.getEigentype
        } else { c }
      }

      consOpt.foreach { cons =>
        val fullMethodDefName =
          cons.getName ++ Constants.SCOPE_SEPARATOR ++ methodDef.methodName.name
        val expressionProcessor = new ExpressionProcessor(
          fullMethodDefName,
          scopeInfo.graph,
          scopeInfo.graph.rootScope,
          dataflowInfo
        )

        moduleScope.registerMethod(
          cons,
          methodDef.methodName.name,
          () => {
            val function = methodDef.body match {
              case fn: Function if isBuiltinMethod(fn.body) =>
                // For builtin types that own the builtin method we only check that
                // the method has been registered during the initialization of builtins
                // and not attempt to register it in the scope (can't redefined methods).
                // For non-builtin types (or modules) that own the builtin method
                // we have to look up the function and register it in the scope.
                // Static wrappers for instance methods have to be registered always.
                val fullMethodName = methodDef.body
                  .asInstanceOf[Function.Lambda]
                  .body
                  .asInstanceOf[Literal.Text]

                val builtinNameElements = fullMethodName.text.split('.')
                if (builtinNameElements.length != 2) {
                  throw new CompilerError(
                    s"Unknown builtin method ${fullMethodName.text}, probably should be '$fullMethodDefName?'"
                  )
                }
                val methodName      = builtinNameElements(1)
                val methodOwnerName = builtinNameElements(0)

                val staticWrapper = methodDef.isStaticWrapperForInstanceMethod

                val builtinFunction = context.getBuiltins
                  .getBuiltinFunction(
                    methodOwnerName,
                    methodName,
                    language,
                    staticWrapper
                  )
                builtinFunction.toScala
                  .map(Some(_))
                  .toRight(
                    new CompilerError(
                      s"Unable to find Truffle Node for method ${cons.getName}.${methodDef.methodName.name}"
                    )
                  )
                  .left
                  .flatMap { l =>
                    // Builtin Types Number and Integer have methods only for documentation purposes
                    val number = context.getBuiltins.number()
                    val ok =
                      staticWrapper && (cons == number.getNumber.getEigentype || cons == number.getInteger.getEigentype) ||
                      !staticWrapper && (cons == number.getNumber             || cons == number.getInteger)
                    if (ok) Right(None)
                    else Left(l)
                  }
                  .map(fOpt =>
                    fOpt.map { m =>
                      if (m.isAutoRegister) {
                        val irFunctionArgumentsCount = fn.arguments.length
                        val builtinArgumentsCount =
                          m.getFunction.getSchema.getArgumentsCount
                        if (irFunctionArgumentsCount != builtinArgumentsCount) {
                          val irFunctionArguments =
                            fn.arguments.map(_.name.name).mkString(",")
                          val builtinArguments =
                            m.getFunction.getSchema.getArgumentInfos
                              .map(_.getName)
                              .mkString(",")
                          throw new CompilerError(
                            s"Wrong number of arguments provided in the definition of builtin function ${cons.getName}.${methodDef.methodName.name}. " +
                            s"[$irFunctionArguments] vs [$builtinArguments]"
                          )
                        }
                        val bodyBuilder =
                          new expressionProcessor.BuildFunctionBody(
                            fn.arguments,
                            fn.body,
                            effectContext,
                            true
                          )
                        val builtinRootNode =
                          m.getFunction.getCallTarget.getRootNode
                            .asInstanceOf[BuiltinRootNode]
                        builtinRootNode
                          .setModuleName(moduleScope.getModule.getName)
                        builtinRootNode.setTypeName(cons.getQualifiedName)
                        new RuntimeFunction(
                          m.getFunction.getCallTarget,
                          null,
                          new FunctionSchema(
                            new Array[RuntimeAnnotation](0),
                            bodyBuilder.args(): _*
                          )
                        )
                      } else {
                        m.getFunction
                      }
                    }
                  )
              case fn: Function =>
                val bodyBuilder =
                  new expressionProcessor.BuildFunctionBody(
                    fn.arguments,
                    fn.body,
                    effectContext,
                    true
                  )
                val rootNode = MethodRootNode.build(
                  language,
                  expressionProcessor.scope,
                  moduleScope,
                  () => bodyBuilder.bodyNode(),
                  makeSection(moduleScope, methodDef.location),
                  cons,
                  methodDef.methodName.name
                )
                val callTarget = rootNode.getCallTarget
                val arguments  = bodyBuilder.args()
                // build annotations
                val annotations =
                  methodDef.getMetadata(GenericAnnotations).toVector.flatMap {
                    meta =>
                      meta.annotations
                        .collect { case annotation: Name.GenericAnnotation =>
                          val scopeElements = Seq(
                            cons.getName,
                            methodDef.methodName.name,
                            annotation.name
                          )
                          val scopeName =
                            scopeElements.mkString(Constants.SCOPE_SEPARATOR)
                          val scopeInfo = annotation
                            .unsafeGetMetadata(
                              AliasAnalysis,
                              s"Missing scope information for annotation " +
                              s"${annotation.name} of method " +
                              scopeElements.init
                                .mkString(Constants.SCOPE_SEPARATOR)
                            )
                            .unsafeAs[AliasAnalysis.Info.Scope.Root]
                          val dataflowInfo = annotation.unsafeGetMetadata(
                            DataflowAnalysis,
                            "Missing dataflow information for annotation " +
                            s"${annotation.name} of method " +
                            scopeElements.init
                              .mkString(Constants.SCOPE_SEPARATOR)
                          )
                          val expressionProcessor = new ExpressionProcessor(
                            scopeName,
                            scopeInfo.graph,
                            scopeInfo.graph.rootScope,
                            dataflowInfo
                          )
                          val expressionNode =
                            expressionProcessor.run(annotation.expression, true)
                          val closureName =
                            s"<default::${expressionProcessor.scopeName}>"
                          val closureRootNode = ClosureRootNode.build(
                            language,
                            expressionProcessor.scope,
                            moduleScope,
                            expressionNode,
                            makeSection(moduleScope, annotation.location),
                            closureName,
                            true,
                            false
                          )
                          new RuntimeAnnotation(
                            annotation.name,
                            closureRootNode
                          )
                        }
                  }

                Right(
                  Some(
                    new RuntimeFunction(
                      callTarget,
                      null,
                      new FunctionSchema(annotations.toArray, arguments: _*)
                    )
                  )
                )
              case _ =>
                Left(
                  new CompilerError(
                    "Method bodies must be functions at the point of codegen."
                  )
                )
            }
            function match {
              case Left(failure) =>
                throw failure
              case Right(Some(fun)) =>
                fun
              case x =>
                throw new IllegalStateException("Wrong state: " + x)
            }
          }
        )
      }
    })

    val conversionDefs = module.bindings.collect {
      case conversion: definition.Method.Conversion =>
        conversion
    }

    // Register the conversion definitions in scope
    conversionDefs.foreach(methodDef => {
      val scopeInfo = methodDef
        .unsafeGetMetadata(
          AliasAnalysis,
          s"Missing scope information for conversion " +
          s"`${methodDef.typeName.map(_.name + ".").getOrElse("")}${methodDef.methodName.name}`."
        )
        .unsafeAs[AliasAnalysis.Info.Scope.Root]
      val dataflowInfo = methodDef.unsafeGetMetadata(
        DataflowAnalysis,
        "Method definition missing dataflow information."
      )

      val toOpt =
        methodDef.methodReference.typePointer match {
          case Some(tpePointer) =>
            getTypeResolution(tpePointer)
          case None =>
            Some(moduleScope.getAssociatedType)
        }
      val fromOpt = getTypeResolution(methodDef.sourceTypeName)
      toOpt.zip(fromOpt).foreach { case (toType, fromType) =>
        val expressionProcessor = new ExpressionProcessor(
          toType.getName ++ Constants.SCOPE_SEPARATOR ++ methodDef.methodName.name,
          scopeInfo.graph,
          scopeInfo.graph.rootScope,
          dataflowInfo
        )

        val function = methodDef.body match {
          case fn: Function =>
            val bodyBuilder =
              new expressionProcessor.BuildFunctionBody(
                fn.arguments,
                fn.body,
                None,
                true
              )
            val rootNode = MethodRootNode.build(
              language,
              expressionProcessor.scope,
              moduleScope,
              () => bodyBuilder.bodyNode(),
              makeSection(moduleScope, methodDef.location),
              toType,
              methodDef.methodName.name
            )
            val callTarget = rootNode.getCallTarget
            val arguments  = bodyBuilder.args()
            new RuntimeFunction(
              callTarget,
              null,
              new FunctionSchema(arguments: _*)
            )
          case _ =>
            throw new CompilerError(
              "Conversion bodies must be functions at the point of codegen."
            )
        }
        moduleScope.registerConversionMethod(toType, fromType, function)
      }
    })
  }

  // ==========================================================================
  // === Utility Functions ====================================================
  // ==========================================================================

  private def extractAscribedType(
    name: Name,
    t: Expression
  ): ReadArgumentCheckNode = t match {
    case u: `type`.Set.Union =>
      ReadArgumentCheckNode.oneOf(
        name,
        u.operands.map(extractAscribedType(name, _)).asJava
      )
    case i: `type`.Set.Intersection =>
      ReadArgumentCheckNode.allOf(
        name,
        extractAscribedType(name, i.left),
        extractAscribedType(name, i.right)
      )
    case p: Application.Prefix => extractAscribedType(name, p.function)
    case _: Tpe.Function =>
      ReadArgumentCheckNode.build(
        name,
        context.getTopScope().getBuiltins().function()
      )
    case t => {
      t.getMetadata(TypeNames) match {
        case Some(
              BindingsMap
                .Resolution(BindingsMap.ResolvedType(mod, tpe))
            ) =>
          ReadArgumentCheckNode.build(
            name,
            asScope(
              mod
                .unsafeAsModule()
                .asInstanceOf[TruffleCompilerContext.Module]
            ).getTypes
              .get(tpe.name)
          )
        case Some(
              BindingsMap
                .Resolution(BindingsMap.ResolvedPolyglotSymbol(mod, symbol))
            ) =>
          ReadArgumentCheckNode.meta(
            name,
            asScope(
              mod
                .unsafeAsModule()
                .asInstanceOf[TruffleCompilerContext.Module]
            ).getPolyglotSymbol(symbol.name)
          )
        case _ => null
      }
    }
  }

  private def checkAsTypes(
    arg: DefinitionArgument
  ): ReadArgumentCheckNode = {
    arg.ascribedType.map(extractAscribedType(arg.name, _)).getOrElse(null)
  }

  /** Checks if the expression has a @Builtin_Method annotation
    *
    * @param expression the expression to check
    * @return 'true' if 'expression' has @Builtin_Method annotation, otherwise 'false'
    */
  private def isBuiltinMethod(expression: Expression): Boolean = {
    expression
      .getMetadata(ExpressionAnnotations)
      .exists(
        _.annotations.exists(_.name == ExpressionAnnotations.builtinMethodName)
      )
  }

  /** Creates a source section from a given location in the module's code.
    *
    * @param module the module that owns/provides the source code
    * @param location the location to turn into a section
    * @return the source section corresponding to `location`
    */
  private def makeSection(
    module: ModuleScope,
    location: Option[IdentifiedLocation]
  ): SourceSection = {
    location
      .map(loc => {
        val m = module.getModule()
        if (m.isModuleSource(source)) {
          module.getModule().createSection(loc.start, loc.length)
        } else {
          source.createSection(loc.start, loc.length)
        }
      })
      .getOrElse(source.createUnavailableSection())
  }

  private def getTypeResolution(expr: IR): Option[Type] =
    expr.getMetadata(MethodDefinitions).map { res =>
      res.target match {
        case BindingsMap.ResolvedType(definitionModule, tp) =>
          asScope(
            definitionModule
              .unsafeAsModule()
          ).getTypes
            .get(tp.name)
        case BindingsMap.ResolvedModule(module) =>
          asScope(
            module
              .unsafeAsModule()
          ).getAssociatedType
        case BindingsMap.ResolvedConstructor(_, _) =>
          throw new CompilerError(
            "Impossible here, should be caught by MethodDefinitions pass."
          )
        case BindingsMap.ResolvedPolyglotSymbol(_, _) =>
          throw new CompilerError(
            "Impossible polyglot symbol, should be caught by MethodDefinitions pass."
          )
        case BindingsMap.ResolvedPolyglotField(_, _) =>
          throw new CompilerError(
            "Impossible polyglot field, should be caught by MethodDefinitions pass."
          )
        case _: BindingsMap.ResolvedMethod =>
          throw new CompilerError(
            "Impossible here, should be caught by MethodDefinitions pass."
          )
      }
    }

  private def getTailStatus(
    expression: Expression
  ): BaseNode.TailStatus = {
    val isTailPosition =
      expression.getMetadata(TailCall).contains(TailCall.TailPosition.Tail)
    val isTailAnnotated = TailCall.isTailAnnotated(expression)
    if (isTailPosition) {
      if (isTailAnnotated) {
        BaseNode.TailStatus.TAIL_LOOP
      } else {
        BaseNode.TailStatus.TAIL_DIRECT
      }
    } else {
      BaseNode.TailStatus.NOT_TAIL
    }
  }

  /** Sets the source section for a given expression node to the provided
    * location.
    *
    * @param expr     the expression to set the location for
    * @param location the location to assign to `expr`
    * @tparam T the type of `expr`
    * @return `expr` with its location set to `location`
    */
  private def setLocation[T <: RuntimeExpression](
    expr: T,
    location: Option[IdentifiedLocation]
  ): T = {
    location.foreach { loc =>
      expr.setSourceLocation(loc.start, loc.length)
      loc.id.foreach { id => expr.setId(id) }
    }
    expr
  }

  private def generateReExportBindings(module: Module): Unit = {
    def mkConsGetter(constructor: AtomConstructor): RuntimeFunction = {
      new RuntimeFunction(
        new QualifiedAccessorNode(language, constructor).getCallTarget,
        null,
        new FunctionSchema(
          new ArgumentDefinition(
            0,
            ConstantsNames.SELF_ARGUMENT,
            null,
            null,
            ArgumentDefinition.ExecutionMode.EXECUTE
          )
        )
      )
    }

    def mkTypeGetter(tp: Type): RuntimeFunction = {
      new RuntimeFunction(
        new ConstantNode(language, tp).getCallTarget,
        null,
        new FunctionSchema(
          new ArgumentDefinition(
            0,
            ConstantsNames.SELF_ARGUMENT,
            null,
            null,
            ArgumentDefinition.ExecutionMode.EXECUTE
          )
        )
      )
    }

    val bindingsMap = module.unsafeGetMetadata(
      BindingAnalysis,
      "No binding analysis at the point of codegen."
    )
    bindingsMap.exportedSymbols.foreach {
      case (name, resolution :: _) =>
        if (
          resolution.isInstanceOf[ResolvedConstructor] || !resolution.module
            .unsafeAsModule()
            .equals(moduleScope.getModule.asCompilerModule)
        ) {
          resolution match {
            case BindingsMap.ResolvedType(module, tp) =>
              val runtimeTp =
                asScope(
                  module
                    .unsafeAsModule()
                ).getTypes
                  .get(tp.name)
              val fun = mkTypeGetter(runtimeTp)
              moduleScope.registerMethod(
                moduleScope.getAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedConstructor(definitionType, cons) =>
              val runtimeCons = asType(definitionType).getConstructors
                .get(cons.name)
              val fun = mkConsGetter(runtimeCons)
              moduleScope.registerMethod(
                moduleScope.getAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedModule(module) =>
              val runtimeCons =
                asScope(
                  module
                    .unsafeAsModule()
                ).getAssociatedType
              val fun = mkTypeGetter(runtimeCons)
              moduleScope.registerMethod(
                moduleScope.getAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedMethod(module, method) =>
              val actualModule = module.unsafeAsModule()
              val fun = asScope(actualModule).getMethodForType(
                asScope(actualModule).getAssociatedType,
                method.name
              )
              assert(
                fun != null,
                s"exported symbol `${method.name}` needs to be registered first in the module "
              )
              moduleScope.registerMethod(
                moduleScope.getAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedPolyglotSymbol(_, _) =>
            case BindingsMap.ResolvedPolyglotField(_, _)  =>
          }
        }
      case _ => throw new CompilerError("Unreachable")
    }
  }

  // ==========================================================================
  // === Expression Processor =================================================
  // ==========================================================================

  /** This class is responsible for performing codegen of [[IR]] constructs that
    * are Enso program expressions.
    *
    * @param scope     the scope in which the code generation is occurring
    * @param scopeName the name of `scope`
    */
  sealed private class ExpressionProcessor(
    val scope: LocalScope,
    val scopeName: String
  ) {

    private var currentVarName = "<anonymous>"

    // === Construction =======================================================

    /** Constructs an [[ExpressionProcessor]] instance with a defaulted local
      * scope.
      *
      * @param scopeName the name to attribute to the default local scope.
      */
    def this(
      scopeName: String,
      graph: AliasGraph,
      scope: AliasScope,
      dataflowInfo: DataflowAnalysis.Metadata
    ) = {
      this(
        new LocalScope(None, graph, scope, dataflowInfo),
        scopeName
      )
    }

    /** Creates an instance of [[ExpressionProcessor]] that operates in a child
      * scope of `this`.
      *
      * @param name the name of the child scope
      * @return an expression processor operating on a child scope
      */
    def createChild(
      name: String,
      scope: AliasScope
    ): ExpressionProcessor = {
      new ExpressionProcessor(this.scope.createChild(scope), name)
    }

    // === Runner =============================================================

    /** Runs the code generation process on the provided piece of [[IR]].
      *
      * @param ir the IR to generate code for
      * @param subjectToInstrumentation value of subject to instrumentation
      * @return a truffle expression that represents the same program as `ir`
      */
    def run(
      ir: Expression,
      subjectToInstrumentation: Boolean
    ): RuntimeExpression = run(ir, false, subjectToInstrumentation)

    private def run(
      ir: Expression,
      binding: Boolean,
      subjectToInstrumentation: Boolean
    ): RuntimeExpression = {
      var runtimeExpression = ir match {
        case block: Expression.Block => processBlock(block)
        case literal: Literal        => processLiteral(literal)
        case app: Application =>
          processApplication(app, subjectToInstrumentation)
        case name: Name                  => processName(name)
        case function: Function          => processFunction(function, binding)
        case binding: Expression.Binding => processBinding(binding)
        case caseExpr: Case =>
          processCase(caseExpr, subjectToInstrumentation)
        case typ: Tpe => processType(typ)
        case _: Empty =>
          throw new CompilerError(
            "Empty IR nodes should not exist during code generation."
          )
        case _: Comment =>
          throw new CompilerError(
            "Comments should not be present during codegen."
          )
        case err: Error => processError(err)
        case Foreign.Definition(_, _, _, _, _) =>
          throw new CompilerError(
            s"Foreign expressions not yet implemented: $ir."
          )
      }
      runtimeExpression.setTailStatus(getTailStatus(ir))

      ir match {
        case _: Expression.Binding =>
        case _ =>
          val types = ir.getMetadata(TypeSignatures)
          types.foreach { tpe =>
            val checkNode = extractAscribedType(null, tpe.signature);
            if (checkNode != null) {
              runtimeExpression =
                ReadArgumentCheckNode.wrap(runtimeExpression, checkNode)
            }
          }
      }
      runtimeExpression
    }

    /** Executes the expression processor on a piece of code that has been
      * written inline.
      *
      * @param ir the IR to generate code for
      * @return a truffle expression that represents the same program as `ir`
      */
    def runInline(ir: Expression): RuntimeExpression = {
      val expression = run(ir, false)
      expression
    }

    // === Processing =========================================================

    /** Performs code generation for an Enso block expression.
      *
      * @param block the block to generate code for
      * @return the truffle nodes corresponding to `block`
      */
    private def processBlock(block: Expression.Block): RuntimeExpression = {
      if (block.suspended) {
        val scopeInfo = block
          .unsafeGetMetadata(
            AliasAnalysis,
            "Missing scope information on block."
          )
          .unsafeAs[AliasAnalysis.Info.Scope.Child]

        val childFactory = this.createChild("suspended-block", scopeInfo.scope)
        val childScope   = childFactory.scope

        val blockNode = childFactory.processBlock(block.copy(suspended = false))

        val defaultRootNode = ClosureRootNode.build(
          language,
          childScope,
          moduleScope,
          blockNode,
          makeSection(moduleScope, block.location),
          currentVarName,
          false,
          false
        )

        val callTarget = defaultRootNode.getCallTarget
        setLocation(CreateThunkNode.build(callTarget), block.location)
      } else {
        val statementExprs = block.expressions.map(this.run(_, true)).toArray
        val retExpr        = this.run(block.returnValue, true)

        val blockNode = BlockNode.build(statementExprs, retExpr)
        setLocation(blockNode, block.location)
      }
    }

    /** Performs code generation for an Enso type operator.
      *
      * @param value the type operation to generate code for
      * @return the truffle nodes corresponding to `value`
      */
    def processType(value: Tpe): RuntimeExpression = {
      setLocation(
        ErrorNode.build(
          context.getBuiltins
            .error()
            .makeSyntaxError(
              Text.create(
                "Type operators are not currently supported at runtime"
              )
            )
        ),
        value.location
      )
    }

    /** Performs code generation for an Enso case expression.
      *
      * @param caseExpr the case expression to generate code for
      * @return the truffle nodes corresponding to `caseExpr`
      */
    def processCase(
      caseExpr: Case,
      subjectToInstrumentation: Boolean
    ): RuntimeExpression =
      caseExpr match {
        case Case.Expr(scrutinee, branches, isNested, location, _, _) =>
          val scrutineeNode = this.run(scrutinee, subjectToInstrumentation)

          val maybeCases    = branches.map(processCaseBranch)
          val allCasesValid = maybeCases.forall(_.isRight)

          if (allCasesValid) {
            val cases = maybeCases
              .collect { case Right(x) =>
                x
              }
              .toArray[BranchNode]

            // Note [Pattern Match Fallbacks]
            val matchExpr = CaseNode.build(
              scrutineeNode,
              cases,
              isNested
            )
            setLocation(matchExpr, location)
          } else {
            val invalidBranches = maybeCases.collect { case Left(x) =>
              x
            }

            val message = invalidBranches.map(_.message).mkString(", ")

            val error = context.getBuiltins
              .error()
              .makeCompileError(Text.create(message))

            setLocation(ErrorNode.build(error), caseExpr.location)
          }
        case _: Case.Branch =>
          throw new CompilerError("A CaseBranch should never occur here.")
      }

    /** Performs code generation for an Enso case branch.
      *
      * @param branch the case branch to generate code for
      * @return the truffle nodes correspondingg to `caseBranch` or an error if
      *         the match is invalid
      */
    def processCaseBranch(
      branch: Case.Branch
    ): Either[BadPatternMatch, BranchNode] = {
      val scopeInfo = branch
        .unsafeGetMetadata(
          AliasAnalysis,
          "No scope information on a case branch."
        )
        .unsafeAs[AliasAnalysis.Info.Scope.Child]

      val childProcessor = this.createChild("case_branch", scopeInfo.scope)

      branch.pattern match {
        case named @ Pattern.Name(_, _, _, _) =>
          val arg = List(genArgFromMatchField(named))

          val branchCodeNode = childProcessor.processFunctionBody(
            arg,
            branch.expression,
            branch.location
          )

          val branchNode =
            CatchAllBranchNode.build(branchCodeNode.getCallTarget, true)

          Right(branchNode)
        case cons @ Pattern.Constructor(constructor, _, _, _, _) =>
          if (!cons.isDesugared) {
            throw new CompilerError(
              "Nested patterns desugaring must have taken place by the " +
              "point of code generation."
            )
          }

          val fieldNames   = cons.unsafeFieldsAsNamed
          val fieldsAsArgs = fieldNames.map(genArgFromMatchField)

          val branchCodeNode = childProcessor.processFunctionBody(
            fieldsAsArgs,
            branch.expression,
            branch.location
          )

          constructor match {
            case err: errors.Resolution =>
              Left(BadPatternMatch.NonVisibleConstructor(err.name))
            case _ =>
              constructor.getMetadata(Patterns) match {
                case None =>
                  Left(BadPatternMatch.NonVisibleConstructor(constructor.name))
                case Some(
                      BindingsMap.Resolution(BindingsMap.ResolvedModule(mod))
                    ) =>
                  Right(
                    ObjectEqualityBranchNode.build(
                      branchCodeNode.getCallTarget,
                      asScope(
                        mod
                          .unsafeAsModule()
                      ).getAssociatedType,
                      branch.terminalBranch
                    )
                  )
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedConstructor(tp, cons)
                      )
                    ) =>
                  val atomCons =
                    asType(tp).getConstructors.get(cons.name)
                  val r = if (atomCons == context.getBuiltins.bool().getTrue) {
                    BooleanBranchNode.build(
                      true,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  } else if (atomCons == context.getBuiltins.bool().getFalse) {
                    BooleanBranchNode.build(
                      false,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  } else {
                    ConstructorBranchNode.build(
                      atomCons,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  }
                  Right(r)
                case Some(
                      BindingsMap.Resolution(BindingsMap.ResolvedType(mod, tp))
                    ) =>
                  val tpe =
                    asScope(
                      mod
                        .unsafeAsModule()
                    ).getTypes
                      .get(tp.name)
                  val polyglot = context.getBuiltins.polyglot
                  val branchNode = if (tpe == polyglot) {
                    PolyglotBranchNode.build(
                      tpe,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  } else {
                    ObjectEqualityBranchNode.build(
                      branchCodeNode.getCallTarget,
                      tpe,
                      branch.terminalBranch
                    )
                  }
                  Right(branchNode)
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedPolyglotSymbol(mod, symbol)
                      )
                    ) =>
                  val polyglotSymbol = asScope(
                    mod
                      .unsafeAsModule()
                  ).getPolyglotSymbol(symbol.name)
                  Either.cond(
                    polyglotSymbol != null,
                    ObjectEqualityBranchNode
                      .build(
                        branchCodeNode.getCallTarget,
                        polyglotSymbol,
                        branch.terminalBranch
                      ),
                    BadPatternMatch.NonVisiblePolyglotSymbol(symbol.name)
                  )
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedPolyglotField(typ, symbol)
                      )
                    ) =>
                  val mod = typ.module
                  val polyClass = asScope(
                    mod
                      .unsafeAsModule()
                  ).getPolyglotSymbol(typ.symbol.name)

                  val polyValueOrError =
                    if (polyClass == null)
                      Left(
                        BadPatternMatch.NonVisiblePolyglotSymbol(
                          typ.symbol.name
                        )
                      )
                    else
                      try {
                        val iop = InteropLibrary.getUncached()
                        if (!iop.isMemberReadable(polyClass, symbol)) {
                          Left(BadPatternMatch.NonVisiblePolyglotSymbol(symbol))
                        } else {
                          if (iop.isMemberModifiable(polyClass, symbol)) {
                            Left(
                              BadPatternMatch.NonConstantPolyglotSymbol(symbol)
                            )
                          } else {
                            val value = iop.readMember(polyClass, symbol);
                            val ensoValue =
                              HostValueToEnsoNode.getUncached().execute(value)
                            Right(ensoValue)
                          }
                        }
                      } catch {
                        case _: Throwable =>
                          Left(BadPatternMatch.NonVisiblePolyglotSymbol(symbol))
                      }
                  polyValueOrError.map(polyValue => {
                    ObjectEqualityBranchNode
                      .build(
                        branchCodeNode.getCallTarget,
                        polyValue,
                        branch.terminalBranch
                      )
                  })
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedMethod(_, _)
                      )
                    ) =>
                  throw new CompilerError(
                    "Impossible method here, should be caught by Patterns resolution pass."
                  )
              }
          }
        case literalPattern: Pattern.Literal =>
          val branchCodeNode = childProcessor.processFunctionBody(
            Nil,
            branch.expression,
            branch.location
          )

          literalPattern.literal match {
            case num: Literal.Number =>
              num.numericValue match {
                case doubleVal: Double =>
                  Right(
                    NumericLiteralBranchNode.build(
                      doubleVal,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  )
                case longVal: Long =>
                  Right(
                    NumericLiteralBranchNode.build(
                      longVal,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  )
                case bigIntVal: BigInteger =>
                  Right(
                    NumericLiteralBranchNode.build(
                      bigIntVal,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  )
                case _ =>
                  throw new CompilerError(
                    "Invalid literal numeric value"
                  )
              }
            case text: Literal.Text =>
              Right(
                StringLiteralBranchNode.build(
                  text.text,
                  branchCodeNode.getCallTarget,
                  branch.terminalBranch
                )
              )
          }
        case Pattern.Type(varName, tpeName, location, _, _) =>
          tpeName.getMetadata(Patterns) match {
            case None =>
              Left(BadPatternMatch.NonVisibleType(tpeName.name))
            case Some(
                  BindingsMap.Resolution(BindingsMap.ResolvedType(mod, tpe))
                ) =>
              // Using .getTypes because .getType may return an associated type
              Option(
                asScope(
                  mod
                    .unsafeAsModule()
                ).getTypes
                  .get(tpe.name)
              ) match {
                case Some(tpe) =>
                  val argOfType = List(
                    DefinitionArgument.Specified(
                      varName,
                      None,
                      None,
                      suspended = false,
                      location,
                      passData    = varName.passData,
                      diagnostics = varName.diagnostics
                    )
                  )

                  val branchCodeNode = childProcessor.processFunctionBody(
                    argOfType,
                    branch.expression,
                    branch.location
                  )
                  Right(
                    CatchTypeBranchNode.build(
                      tpe,
                      branchCodeNode.getCallTarget,
                      branch.terminalBranch
                    )
                  )
                case None => Left(BadPatternMatch.NonVisibleType(tpeName.name))
              }
            case Some(
                  BindingsMap.Resolution(
                    BindingsMap.ResolvedPolyglotSymbol(mod, symbol)
                  )
                ) =>
              val polySymbol =
                asScope(
                  mod
                    .unsafeAsModule()
                ).getPolyglotSymbol(symbol.name)
              if (polySymbol != null) {
                val argOfType = List(
                  DefinitionArgument.Specified(
                    varName,
                    None,
                    None,
                    suspended = false,
                    location,
                    passData    = varName.passData,
                    diagnostics = varName.diagnostics
                  )
                )

                val branchCodeNode = childProcessor.processFunctionBody(
                  argOfType,
                  branch.expression,
                  branch.location
                )
                Right(
                  PolyglotSymbolTypeBranchNode.build(
                    polySymbol,
                    branchCodeNode.getCallTarget,
                    branch.terminalBranch
                  )
                )
              } else {
                Left(BadPatternMatch.NonVisiblePolyglotSymbol(tpeName.name))
              }
            case Some(BindingsMap.Resolution(resolved)) =>
              throw new CompilerError(
                s"Impossible ${resolved} here, should be caught by Patterns resolution pass."
              )
          }
        case _: Pattern.Documentation =>
          throw new CompilerError(
            "Branch documentation should be desugared at an earlier stage."
          )
        case errors.Pattern(
              _,
              errors.Pattern.WrongArity(name, expected, actual),
              _,
              _
            ) =>
          Left(BadPatternMatch.WrongArgCount(name, expected, actual))

      }
    }

    /* Note [Pattern Match Fallbacks]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * Enso in its current state has no coverage checking for constructors on
     * pattern matches as it has no sense of what constructors contribute to
     * make a 'type'. This means that, in absence of a user-provided fallback or
     * catch-all case in a pattern match, the interpreter has to ensure that
     * it has one to catch that error.
     */

    /** Generates an argument from a field of a pattern match.
      *
      * @param name the pattern field to generate from
      * @return `name` as a function definition argument.
      */
    def genArgFromMatchField(name: Pattern.Name): DefinitionArgument = {
      DefinitionArgument.Specified(
        name.name,
        None,
        None,
        suspended = false,
        name.location,
        passData    = name.name.passData,
        diagnostics = name.name.diagnostics
      )
    }

    /** Generates code for an Enso binding expression.
      *
      * @param binding the binding to generate code for
      * @return the truffle nodes corresponding to `binding`
      */
    private def processBinding(
      binding: Expression.Binding
    ): RuntimeExpression = {
      val occInfo = binding
        .unsafeGetMetadata(
          AliasAnalysis,
          "Binding with missing occurrence information."
        )
        .unsafeAs[AliasAnalysis.Info.Occurrence]

      currentVarName = binding.name.name

      val slotIdx = scope.getVarSlotIdx(occInfo.id)

      setLocation(
        AssignmentNode.build(this.run(binding.expression, true, true), slotIdx),
        binding.location
      )
    }

    /** Generates code for an Enso function.
      *
      * @param function the function to generate code for
      * @param binding whether the function is right inside a binding
      * @return the truffle nodes corresponding to `function`
      */
    private def processFunction(
      function: Function,
      binding: Boolean
    ): RuntimeExpression = {
      val scopeInfo = function
        .unsafeGetMetadata(AliasAnalysis, "No scope info on a function.")
        .unsafeAs[AliasAnalysis.Info.Scope.Child]

      if (function.body.isInstanceOf[Function]) {
        throw new CompilerError(
          "Lambda found directly as function body. It looks like Lambda " +
          "Consolidation hasn't run."
        )
      }

      val scopeName = if (function.canBeTCO) {
        currentVarName
      } else {
        "case_expression"
      }

      val child = this.createChild(scopeName, scopeInfo.scope)

      val fn = child.processFunctionBody(
        function.arguments,
        function.body,
        function.location,
        binding
      )

      fn
    }

    /** Generates code for an Enso name.
      *
      * @param name the name to generate code for
      * @return the truffle nodes corresponding to `name`
      */
    def processName(name: Name): RuntimeExpression = {
      val nameExpr = name match {
        case Name.Literal(nameStr, _, _, _, _, _) =>
          val useInfo = name
            .unsafeGetMetadata(
              AliasAnalysis,
              "No occurrence on variable usage."
            )
            .unsafeAs[AliasAnalysis.Info.Occurrence]

          val framePointer = scope.getFramePointer(useInfo.id)
          val global       = name.getMetadata(GlobalNames)
          if (framePointer.isDefined) {
            ReadLocalVariableNode.build(framePointer.get)
          } else if (global.isDefined) {
            val resolution = global.get.target
            nodeForResolution(resolution)
          } else if (nameStr == ConstantsNames.FROM_MEMBER) {
            ConstantObjectNode.build(UnresolvedConversion.build(moduleScope))
          } else {
            DynamicSymbolNode.build(
              UnresolvedSymbol.build(nameStr, moduleScope)
            )
          }
        case Name.Self(location, _, passData, _) =>
          processName(
            Name.Literal(
              ConstantsNames.SELF_ARGUMENT,
              isMethod = false,
              location,
              None,
              passData
            )
          )
        case n: Name.SelfType =>
          nodeForResolution(
            n.unsafeGetMetadata(
              GlobalNames,
              "a Self occurence must be resolved"
            ).target
          )
        case Name.Special(name, _, _, _) =>
          val fun = name match {
            case Special.NewRef    => context.getBuiltins.special().getNewRef
            case Special.ReadRef   => context.getBuiltins.special().getReadRef
            case Special.WriteRef  => context.getBuiltins.special().getWriteRef
            case Special.RunThread => context.getBuiltins.special().getRunThread
            case Special.JoinThread =>
              context.getBuiltins.special().getJoinThread
          }
          ConstantObjectNode.build(fun)
        case _: Name.Annotation =>
          throw new CompilerError(
            "Annotation should not be present at codegen time."
          )
        case _: Name.Blank =>
          throw new CompilerError(
            "Blanks should not be present at codegen time."
          )
        case _: Name.MethodReference =>
          throw new CompilerError(
            "Method references should not be present at codegen time."
          )
        case _: Name.Qualified =>
          throw new CompilerError(
            "Qualified names should not be present at codegen time."
          )
        case err: errors.Resolution => processError(err)
        case err: errors.Conversion => processError(err)
      }

      setLocation(nameExpr, name.location)
    }

    private def nodeForResolution(
      resolution: BindingsMap.ResolvedName
    ): RuntimeExpression = {
      resolution match {
        case tp: BindingsMap.ResolvedType =>
          ConstantObjectNode.build(
            asScope(
              tp.module
                .unsafeAsModule()
            ).getTypes
              .get(tp.tp.name)
          )
        case BindingsMap.ResolvedConstructor(definitionType, cons) =>
          val c = asType(definitionType).getConstructors
            .get(cons.name)
          if (c == null) {
            throw new CompilerError(s"Constructor for $cons is null")
          }
          ConstructorNode.build(c)
        case BindingsMap.ResolvedModule(module) =>
          ConstantObjectNode.build(
            asScope(
              module
                .unsafeAsModule()
            ).getAssociatedType
          )
        case BindingsMap.ResolvedPolyglotSymbol(module, symbol) =>
          val s = asScope(
            module
              .unsafeAsModule()
          ).getPolyglotSymbol(symbol.name)
          if (s == null) {
            throw new CompilerError(
              s"No polyglot symbol for ${symbol.name}"
            )
          }
          ConstantObjectNode.build(s)
        case BindingsMap.ResolvedPolyglotField(symbol, name) =>
          val s = asScope(
            symbol.module
              .unsafeAsModule()
          ).getPolyglotSymbol(name)
          if (s == null) {
            throw new CompilerError(
              s"No polyglot field for ${name}"
            )
          }

          ConstantObjectNode.build(s)
        case BindingsMap.ResolvedMethod(_, method) =>
          throw new CompilerError(
            s"Impossible here, ${method.name} should be caught when translating application"
          )
      }
    }

    /** Generates code for an Enso literal.
      *
      * @param literal the literal to generate code for
      * @return the truffle nodes corresponding to `literal`
      */
    @throws[CompilerError]
    def processLiteral(literal: Literal): RuntimeExpression =
      literal match {
        case lit @ Literal.Number(_, _, location, _, _) =>
          val node = lit.numericValue match {
            case l: Long       => LiteralNode.build(l)
            case d: Double     => LiteralNode.build(d)
            case b: BigInteger => LiteralNode.build(b)
          }
          setLocation(node, location)
        case Literal.Text(text, location, _, _) =>
          setLocation(LiteralNode.build(text), location)
      }

    /** Generates a runtime implementation for compile error nodes.
      *
      * @param error the IR representing a compile error.
      * @return a runtime node representing the error.
      */
    def processError(error: Error): RuntimeExpression = {
      val payload: Atom = error match {
        case Error.InvalidIR(_, _, _) =>
          throw new CompilerError("Unexpected Invalid IR during codegen.")
        case err: errors.Syntax =>
          context.getBuiltins
            .error()
            .makeSyntaxError(Text.create(err.message(source)))
        case err: errors.Redefined.Binding =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Redefined.Method =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Redefined.MethodClashWithAtom =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Redefined.Conversion =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Redefined.Type =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Redefined.SelfArg =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Unexpected.TypeSignature =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Resolution =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case err: errors.Conversion =>
          context.getBuiltins
            .error()
            .makeCompileError(Text.create(err.message(source)))
        case _: errors.Pattern =>
          throw new CompilerError(
            "Impossible here, should be handled in the pattern match."
          )
        case _: errors.ImportExport =>
          throw new CompilerError(
            "Impossible here, should be handled in import/export processing"
          )
      }
      setLocation(ErrorNode.build(payload), error.location)
    }

    /** Processes function arguments, generates arguments reads and creates
      * a node to represent the whole method body.
      *
      * @param arguments the argument definitions
      * @param body      the body definition
      * @return a node for the final shape of function body and pre-processed
      *         argument definitions.
      */
    class BuildFunctionBody(
      val arguments: List[DefinitionArgument],
      val body: Expression,
      val effectContext: Option[String],
      val subjectToInstrumentation: Boolean
    ) {
      private val argFactory = new DefinitionArgumentProcessor(scopeName, scope)
      private lazy val slots = computeSlots()
      private lazy val bodyN = computeBodyNode()

      def args(): Array[ArgumentDefinition] = slots._2
      def bodyNode(): RuntimeExpression     = bodyN

      private def computeBodyNode(): RuntimeExpression = {
        val (argSlotIdxs, _, argExpressions) = slots

        val bodyExpr = body match {
          case Foreign.Definition(lang, code, _, _, _) =>
            buildForeignBody(
              ForeignLanguage.getBySyntacticTag(lang),
              code,
              arguments.map(_.name.name),
              argSlotIdxs
            )
          case _ =>
            ExpressionProcessor.this.run(body, false, subjectToInstrumentation)
        }
        BlockNode.build(argExpressions.toArray, bodyExpr)
      }

      private def computeSlots(): (
        List[Int],
        Array[ArgumentDefinition],
        ArrayBuffer[RuntimeExpression]
      ) = {
        val seenArgNames   = mutable.Set[String]()
        val argDefinitions = new Array[ArgumentDefinition](arguments.size)
        val argExpressions = new ArrayBuffer[RuntimeExpression]
        // Note [Rewriting Arguments]
        val argSlots = arguments.zipWithIndex.map {
          case (unprocessedArg, idx) =>
            val checkNode = checkAsTypes(unprocessedArg)
            val arg       = argFactory.run(unprocessedArg, idx, checkNode)
            argDefinitions(idx) = arg
            val occInfo = unprocessedArg
              .unsafeGetMetadata(
                AliasAnalysis,
                "No occurrence on an argument definition."
              )
              .unsafeAs[AliasAnalysis.Info.Occurrence]

            val slotIdx = scope.getVarSlotIdx(occInfo.id)
            val readArg =
              ReadArgumentNode.build(
                idx,
                arg.getDefaultValue.orElse(null),
                checkNode
              )
            val assignArg = AssignmentNode.build(readArg, slotIdx)

            argExpressions.append(assignArg)

            val argName = arg.getName

            if (
              argName != ConstantsNames.SELF_ARGUMENT && seenArgNames.contains(
                argName
              )
            ) {
              throw new IllegalStateException(
                s"A duplicate argument name, $argName, was found during codegen."
              )
            } else seenArgNames.add(argName)
            slotIdx
        }
        (argSlots, argDefinitions, argExpressions)
      }
    }

    private def buildForeignBody(
      language: ForeignLanguage,
      code: String,
      argumentNames: List[String],
      argumentSlotIdxs: List[Int]
    ): RuntimeExpression = {
      val src       = language.buildSource(code, scopeName)
      val foreignCt = context.parseInternal(src, argumentNames: _*)
      val argumentReaders = argumentSlotIdxs
        .map(slotIdx =>
          ReadLocalVariableNode.build(new FramePointer(0, slotIdx))
        )
        .toArray[RuntimeExpression]
      ForeignMethodCallNode.build(argumentReaders, foreignCt)
    }

    /** Generates code for an Enso function body.
      *
      * @param arguments the arguments to the function
      * @param body      the body of the function
      * @param location  the location at which the function exists in the source
      * @param binding whether the function is right inside a binding
      * @return a truffle node representing the described function
      */
    private def processFunctionBody(
      arguments: List[DefinitionArgument],
      body: Expression,
      location: Option[IdentifiedLocation],
      binding: Boolean = false
    ): CreateFunctionNode = {
      val bodyBuilder = new BuildFunctionBody(arguments, body, None, false)
      val fnRootNode = ClosureRootNode.build(
        language,
        scope,
        moduleScope,
        bodyBuilder.bodyNode(),
        makeSection(moduleScope, location),
        scopeName,
        false,
        binding
      )
      val callTarget = fnRootNode.getCallTarget

      val expr = CreateFunctionNode.build(callTarget, bodyBuilder.args())

      setLocation(expr, location)
    }

    /* Note [Rewriting Arguments]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~
     * While it would be tempting to handle function arguments as a special case
     * of a lookup, it is instead far simpler to rewrite them such that they
     * just become bindings in the function local scope. This occurs for both
     * explicitly passed argument values, and those that have been defaulted.
     *
     * For each argument, the following algorithm is executed:
     *
     * 1. Argument Conversion: Arguments are converted into their definitions so
     *    as to provide a compact representation of all known information about
     *    that argument.
     * 2. Frame Conversion: A variable framePointer is created in the function's local
     *    frame to contain the value of the function argument.
     * 3. Read Provision: A `ReadArgumentNode` is generated to allow that
     *    function argument to be treated purely as a local variable access. See
     *    Note [Handling Argument Defaults] for more information on how this
     *    works.
     * 4. Value Assignment: A `AssignmentNode` is created to connect the
     *    argument value to the framePointer created in Step 2.
     * 5. Body Rewriting: The expression representing the argument is written
     *    into the function body, thus allowing it to be read simply.
     */

    /** Generates code for an Enso function application.
      *
      * @param application the function application to generate code for
      * @return the truffle nodes corresponding to `application`
      */
    private def processApplication(
      application: Application,
      subjectToInstrumentation: Boolean
    ): RuntimeExpression =
      application match {
        case Application.Prefix(fn, Nil, true, _, _, _) =>
          run(fn, subjectToInstrumentation)
        case app: Application.Prefix =>
          processApplicationWithArgs(app, subjectToInstrumentation)
        case Application.Force(expr, location, _, _) =>
          setLocation(
            ForceNode.build(this.run(expr, subjectToInstrumentation)),
            location
          )
        case Application.Sequence(items, location, _, _) =>
          val itemNodes = items.map(run(_, subjectToInstrumentation)).toArray
          setLocation(SequenceLiteralNode.build(itemNodes), location)
        case _: Application.Typeset =>
          setLocation(
            ErrorNode.build(
              context.getBuiltins
                .error()
                .makeSyntaxError(
                  Text.create(
                    "Typeset literals are not yet supported at runtime"
                  )
                )
            ),
            application.location
          )
        case op: Operator.Binary =>
          throw new CompilerError(
            s"Explicit operators not supported during codegen but $op found"
          )
        case sec: Section =>
          throw new CompilerError(
            s"Explicit operator sections not supported during codegen but " +
            s"$sec found"
          )
      }

    private def processApplicationWithArgs(
      application: Application.Prefix,
      subjectToInstrumentation: Boolean
    ): RuntimeExpression = {
      val Application.Prefix(fn, args, hasDefaultsSuspended, loc, _, _) =
        application
      val callArgFactory = new CallArgumentProcessor(scope, scopeName)

      val arguments = args
      val callArgs  = new ArrayBuffer[callable.argument.CallArgument]()

      for ((unprocessedArg, position) <- arguments.view.zipWithIndex) {
        val arg =
          callArgFactory.run(unprocessedArg, position, subjectToInstrumentation)
        callArgs.append(arg)
      }

      val defaultsExecutionMode = if (hasDefaultsSuspended) {
        InvokeCallableNode.DefaultsExecutionMode.IGNORE
      } else {
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE
      }

      val appNode = ApplicationNode.build(
        this.run(fn, subjectToInstrumentation),
        callArgs.toArray,
        defaultsExecutionMode
      )

      setLocation(appNode, loc)
    }

  }

  // ==========================================================================
  // === Call Argument Processor ==============================================
  // ==========================================================================

  /** Performs codegen for call-site arguments in Enso.
    *
    * @param scope     the scope in which the function call exists
    * @param scopeName the name of `scope`
    */
  sealed private class CallArgumentProcessor(
    val scope: LocalScope,
    val scopeName: String
  ) {

    // === Runner =============================================================

    /** Executes codegen on the call-site argument.
      *
      * @param arg      the argument definition
      * @param position the position of the argument at the call site
      * @return a truffle construct corresponding to the argument definition
      *         `arg`
      */
    def run(
      arg: CallArgument,
      position: Int,
      subjectToInstrumentation: Boolean
    ): callable.argument.CallArgument =
      arg match {
        case CallArgument.Specified(
              name,
              value,
              _,
              _,
              _
            ) =>
          val scopeInfo = arg
            .unsafeGetMetadata(
              AliasAnalysis,
              "No scope attached to a call argument."
            )
            .unsafeAs[AliasAnalysis.Info.Scope.Child]

          val shouldCreateClosureRootNode = value match {
            case _: Name           => false
            case _: Literal.Text   => false
            case _: Literal.Number => false
            case _                 => true
          }

          val childScope = if (shouldCreateClosureRootNode) {
            scope.createChild(scopeInfo.scope)
          } else {
            // Note [Scope Flattening]
            scope.createChild(scopeInfo.scope, flattenToParent = true)
          }
          val argumentExpression =
            new ExpressionProcessor(childScope, scopeName)
              .run(value, subjectToInstrumentation)

          val result = if (!shouldCreateClosureRootNode) {
            argumentExpression
          } else {
            argumentExpression.setTailStatus(getTailStatus(value))

            val displayName =
              s"${scopeName}<arg-${name.map(_.name).getOrElse(String.valueOf(position))}>"

            val section = value.location
              .map(loc => source.createSection(loc.start, loc.length))
              .orNull

            val closureRootNode = ClosureRootNode.build(
              language,
              childScope,
              moduleScope,
              argumentExpression,
              section,
              displayName,
              subjectToInstrumentation,
              false
            )
            val callTarget = closureRootNode.getCallTarget

            CreateThunkNode.build(callTarget)
          }

          new callable.argument.CallArgument(name.map(_.name).orNull, result)
      }
  }

  /* Note [Scope Flattening]
   * ~~~~~~~~~~~~~~~~~~~~~~~
   * Given that we represent _all_ function arguments as thunks at runtime, we
   * account for this during alias analysis by allocating new scopes for the
   * function arguments as they are passed. However, in the case of an argument
   * that is _already_ suspended, we want to pass this directly. However, we do
   * not have demand information at the point of alias analysis, and so we have
   * allocated a new scope for it regardless.
   *
   * As a result, we flatten that scope back into the parent during codegen to
   * work around the differences between the semantic meaning of the language
   * and the runtime representation of function arguments.
   */

  // ==========================================================================
  // === Definition Argument Processor ========================================
  // ==========================================================================

  /** Performs codegen for definition-site arguments in Enso.
    *
    * @param scope     the scope in which the function is defined
    * @param scopeName the name of `scope`
    */
  sealed private class DefinitionArgumentProcessor(
    val scopeName: String = "<root>",
    val scope: LocalScope
  ) {

    // === Runner =============================================================

    /* Note [Handling Suspended Defaults]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * Suspended defaults need to be wrapped in a thunk to ensure that they
     * behave properly with regards to the expected semantics of lazy arguments.
     *
     * Were they not wrapped in a thunk, they would be evaluated eagerly, and
     * hence the point at which the default would be evaluated would differ from
     * the point at which a passed-in argument would be evaluated.
     */

    /** Executes the code generator on the provided definition-site argument.
      *
      * @param inputArg the argument to generate code for
      * @param position the position of `arg` at the function definition site
      * @param checkNode null or node to check the argument type for
      * @return a truffle entity corresponding to the definition of `arg` for a
      *         given function
      */
    def run(
      inputArg: DefinitionArgument,
      position: Int,
      types: ReadArgumentCheckNode
    ): ArgumentDefinition =
      inputArg match {
        case arg: DefinitionArgument.Specified =>
          val defaultExpression = arg.defaultValue
            .map(new ExpressionProcessor(scope, scopeName).run(_, false))
            .orNull

          // Note [Handling Suspended Defaults]
          val defaultedValue = if (arg.suspended && defaultExpression != null) {
            val defaultRootNode = ClosureRootNode.build(
              language,
              scope,
              moduleScope,
              defaultExpression,
              null,
              s"<default::$scopeName::${arg.name}>",
              false,
              false
            )

            CreateThunkNode.build(
              defaultRootNode.getCallTarget
            )
          } else {
            defaultExpression
          }

          val executionMode = if (arg.suspended) {
            ArgumentDefinition.ExecutionMode.PASS_THUNK
          } else {
            ArgumentDefinition.ExecutionMode.EXECUTE
          }

          new ArgumentDefinition(
            position,
            arg.name.name,
            types,
            defaultedValue,
            executionMode
          )
      }
  }

  private def asScope(module: CompilerContext.Module): ModuleScope = {
    val m = org.enso.interpreter.runtime.Module.fromCompilerModule(module)
    m.getScope()
  }

  private def asType(typ: BindingsMap.ResolvedType): Type = {
    asScope(typ.module.unsafeAsModule()).getTypes().get(typ.tp.name)
  }
}
