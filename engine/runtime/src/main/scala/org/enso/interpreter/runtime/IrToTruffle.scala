package org.enso.interpreter.runtime

import java.util.logging.Level
import com.oracle.truffle.api.source.{Source, SourceSection}
import com.oracle.truffle.api.interop.InteropLibrary
import org.enso.compiler.pass.analyse.FramePointer
import org.enso.compiler.pass.analyse.FrameVariableNames
import org.enso.compiler.context.{
  CompilerContext,
  LocalScope,
  NameResolutionAlgorithm
}
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
import org.enso.compiler.core.ir.module.scope.definition.Method
import org.enso.compiler.data.BindingsMap.{ResolvedConstructor, ResolvedModule}
import org.enso.compiler.data.{BindingsMap, CompilerConfig}
import org.enso.compiler.exception.BadPatternMatch
import org.enso.compiler.pass.analyse.alias.graph.Graph.{Scope => AliasScope}
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  BindingAnalysis,
  DataflowAnalysis,
  FramePointerAnalysis,
  TailCall
}
import org.enso.compiler.pass.analyse.alias.AliasMetadata
import org.enso.compiler.pass.analyse.alias.graph.{Graph => AliasGraph}
import org.enso.compiler.pass.resolve.{
  ExpressionAnnotations,
  GenericAnnotations,
  GlobalNames,
  MethodDefinitions,
  Patterns,
  TypeNames,
  TypeSignatures
}
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
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode
import org.enso.interpreter.node.expression.constant._
import org.enso.interpreter.node.expression.foreign.ForeignMethodCallNode
import org.enso.interpreter.node.expression.literal.LiteralNode
import org.enso.interpreter.node.scope.{AssignmentNode, ReadLocalVariableNode}
import org.enso.interpreter.node.{
  BaseNode,
  ClosureRootNode,
  ConstantNode,
  MethodRootNode,
  ExpressionNode => RuntimeExpression
}
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition
import org.enso.interpreter.runtime.data.atom.{Atom, AtomConstructor}
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
import org.enso.interpreter.runtime.scope.{ImportExportScope, ModuleScope}
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
  * @param scopeBuilder   the scope's builder of the module for which code is being generated
  * @param compilerConfig the configuration for the compiler
  */
class IrToTruffle(
  val context: EnsoContext,
  val source: Source,
  val scopeBuilder: ModuleScope.Builder,
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
    new ExpressionProcessor(
      localScope,
      scopeName,
      scopeBuilder.getModule().getName().toString()
    ).runInline(ir)
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

    registerModuleExports(bindingsMap)
    registerModuleImports(bindingsMap)
    registerPolyglotImports(module)

    registerTypeDefinitions(module)
    registerMethodDefinitions(module)
    registerConversions(module)

    scopeBuilder.build()
  }

  private def registerModuleExports(bindingsMap: BindingsMap): Unit =
    bindingsMap.getDirectlyExportedModules.foreach { exportedMod =>
      val exportedRuntimeMod = exportedMod.module.module.unsafeAsModule()
      scopeBuilder.addExport(
        new ImportExportScope(exportedRuntimeMod)
      )
    }

  private def registerModuleImports(bindingsMap: BindingsMap): Unit =
    bindingsMap.resolvedImports.foreach { imp =>
      imp.targets.foreach {
        case _: BindingsMap.ResolvedType             =>
        case _: BindingsMap.ResolvedConstructor      =>
        case _: BindingsMap.ResolvedModuleMethod     =>
        case _: BindingsMap.ResolvedExtensionMethod  =>
        case _: BindingsMap.ResolvedConversionMethod =>
        case ResolvedModule(module) =>
          val mod = module
            .unsafeAsModule()
          val scope: ImportExportScope = imp.importDef.onlyNames
            .map(only => new ImportExportScope(mod, only.map(_.name).asJava))
            .getOrElse(new ImportExportScope(mod))
          scopeBuilder.addImport(scope)
      }
    }

  private def registerPolyglotImports(module: Module): Unit =
    module.imports.foreach {
      case poly @ imports.Polyglot(i: imports.Polyglot.Java, _, _, _) =>
        this.scopeBuilder.registerPolyglotSymbol(
          poly.getVisibleName,
          () => {
            val hostSymbol = context.lookupJavaClass(i.getJavaName)
            hostSymbol
          }
        )
      case _: Import.Module =>
      case _: Error         =>
    }

  private def registerTypeDefinitions(module: Module): Unit = {
    val typeDefs = module.bindings.collect { case tp: Definition.Type => tp }
    typeDefs.foreach { tpDef =>
      // Register the atoms and their constructors in scope
      val atomDefs = tpDef.members
      val asType   = scopeBuilder.asModuleScope().getType(tpDef.name.name, true)
      val atomConstructors =
        atomDefs.map(cons => asType.getConstructors.get(cons.name.name))
      atomConstructors
        .zip(atomDefs)
        .foreach { case (atomCons, atomDefn) =>
          registerAtomConstructor(tpDef, atomCons, atomDefn)
        }
      asType.generateGetters(language)
    }
  }

  private def registerAtomConstructor(
    tpDef: Definition.Type,
    atomCons: AtomConstructor,
    atomDefn: Definition.Data
  ): Unit = {
    val scopeInfo = rootScopeInfo("atom definition", atomDefn)

    def dataflowInfo() = atomDefn.unsafeGetMetadata(
      DataflowAnalysis,
      "No dataflow information associated with an atom."
    )
    def frameInfo() = atomDefn.unsafeGetMetadata(
      FramePointerAnalysis,
      "Method definition missing frame information."
    )
    val localScope = new LocalScope(
      None,
      () => scopeInfo().graph,
      () => scopeInfo().graph.rootScope,
      dataflowInfo,
      frameInfo
    )

    val argFactory =
      new DefinitionArgumentProcessor(
        scope       = localScope,
        initialName = "Type " + tpDef.name.name
      )
    val argDefs =
      new Array[ArgumentDefinition](atomDefn.arguments.size)
    val argumentExpressions =
      new ArrayBuffer[(RuntimeExpression, RuntimeExpression)]

    for (idx <- atomDefn.arguments.indices) {
      val unprocessedArg = atomDefn.arguments(idx)
      val checkNode      = checkAsTypes(unprocessedArg)
      val arg            = argFactory.run(unprocessedArg, idx, checkNode)
      val fp = unprocessedArg
        .unsafeGetMetadata(
          FramePointerAnalysis,
          "No frame pointer on an argument definition."
        )
        .asInstanceOf[FramePointer]
      val slotIdx = fp.frameSlotIdx()
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
        () => scopeInfo().graph,
        () => scopeInfo().graph.rootScope,
        dataflowInfo,
        atomDefn.name.name,
        frameInfo
      )
      val expressionNode =
        expressionProcessor.run(annotation.expression, true)
      val closureName = s"<default::$scopeName>"
      val closureRootNode = ClosureRootNode.build(
        language,
        expressionProcessor.scope,
        scopeBuilder.asModuleScope(),
        expressionNode,
        makeSection(scopeBuilder.getModule, annotation.location),
        closureName,
        true,
        false
      )
      new RuntimeAnnotation(annotation.name, closureRootNode)
    }
    if (!atomCons.isInitialized) {
      atomCons.initializeFields(
        language,
        makeSection(scopeBuilder.getModule, atomDefn.location),
        localScope,
        scopeBuilder,
        assignments.toArray,
        reads.toArray,
        annotations.toArray,
        argDefs: _*
      )
    }
  }

  private def registerMethodDefinitions(module: Module): Unit = {
    val methodDefs = module.bindings.collect {
      case method: definition.Method.Explicit => method
    }

    methodDefs.foreach(methodDef => {
      lazy val where =
        s"`method ${methodDef.typeName.map(_.name + ".").getOrElse("")}${methodDef.methodName.name}`."
      val scopeInfo = rootScopeInfo(where, methodDef)
      def dataflowInfo() = methodDef.unsafeGetMetadata(
        DataflowAnalysis,
        "Method definition missing dataflow information."
      )
      def frameInfo() = methodDef.unsafeGetMetadata(
        FramePointerAnalysis,
        "Method definition missing frame information."
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
        getTypeAssociatedWithMethodDefinition(methodDef)

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
          () => scopeInfo().graph,
          () => scopeInfo().graph.rootScope,
          dataflowInfo,
          fullMethodDefName,
          frameInfo
        )

        scopeBuilder.registerMethod(
          cons,
          methodDef.methodName.name,
          () => {
            buildFunction(
              methodDef,
              effectContext,
              cons,
              fullMethodDefName,
              expressionProcessor
            )
          }
        )
      }
    })
  }

  private def buildFunction(
    methodDef: Method.Explicit,
    effectContext: Option[String],
    cons: Type,
    fullMethodDefName: String,
    expressionProcessor: ExpressionProcessor
  ): RuntimeFunction = {
    val function = methodDef.body match {
      case fn: Function if isBuiltinMethod(fn.body) =>
        buildBuiltinFunction(
          fn,
          expressionProcessor,
          methodDef,
          effectContext,
          cons,
          fullMethodDefName
        )
      case fn: Function =>
        Right(
          Some(
            buildRegularFunction(
              methodDef,
              effectContext,
              cons,
              fullMethodDefName,
              expressionProcessor,
              fn
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

  private def buildRegularFunction(
    methodDef: Method.Explicit,
    effectContext: Option[String],
    cons: Type,
    fullMethodDefName: String,
    expressionProcessor: ExpressionProcessor,
    fn: Function
  ): RuntimeFunction = {
    val bodyBuilder =
      new expressionProcessor.BuildFunctionBody(
        fullMethodDefName,
        fn.arguments,
        fn.body,
        null,
        effectContext,
        true
      )

    val operators = ".!$%&*+-/<>?^~\\="

    def isOperator(n: Name): Boolean = {
      n.name
        .chars()
        .allMatch(operators.indexOf(_) >= 0)
    }

    val arguments = bodyBuilder.args()
    val rootNode =
      if (arguments.size == 2 && isOperator(methodDef.methodName)) {
        MethodRootNode.buildOperator(
          language,
          expressionProcessor.scope,
          scopeBuilder.asModuleScope(),
          () => bodyBuilder.argsExpr._1(0),
          () => bodyBuilder.argsExpr._1(1),
          () => bodyBuilder.argsExpr._2,
          makeSection(scopeBuilder.getModule, methodDef.location),
          cons,
          methodDef.methodName.name
        )
      } else {
        MethodRootNode.build(
          language,
          expressionProcessor.scope,
          scopeBuilder.asModuleScope(),
          () => bodyBuilder.bodyNode(),
          makeSection(scopeBuilder.getModule, methodDef.location),
          cons,
          methodDef.methodName.name
        )
      }
    val callTarget = rootNode.getCallTarget
    // build annotations
    val annotations =
      methodDef.getMetadata(GenericAnnotations).toVector.flatMap { meta =>
        meta.annotations
          .collect { case annotation: Name.GenericAnnotation =>
            val scopeElements = Seq(
              cons.getName,
              methodDef.methodName.name,
              annotation.name
            )
            val scopeName =
              scopeElements.mkString(Constants.SCOPE_SEPARATOR)

            lazy val where =
              s"annotation ${annotation.name} of method ${scopeElements.init
                .mkString(Constants.SCOPE_SEPARATOR)}"
            val scopeInfo = rootScopeInfo(where, annotation)

            def dataflowInfo() = annotation.unsafeGetMetadata(
              DataflowAnalysis,
              "Missing dataflow information for annotation " +
              s"${annotation.name} of method " +
              scopeElements.init
                .mkString(Constants.SCOPE_SEPARATOR)
            )
            def frameInfo() = annotation.unsafeGetMetadata(
              FramePointerAnalysis,
              "Method definition missing frame information."
            )
            val expressionProcessor = new ExpressionProcessor(
              scopeName,
              () => scopeInfo().graph,
              () => scopeInfo().graph.rootScope,
              dataflowInfo,
              methodDef.methodName.name,
              frameInfo
            )
            val expressionNode =
              expressionProcessor.run(annotation.expression, true)
            val closureName =
              s"<default::${expressionProcessor.scopeName}>"
            val closureRootNode = ClosureRootNode.build(
              language,
              expressionProcessor.scope,
              scopeBuilder.asModuleScope(),
              expressionNode,
              makeSection(
                scopeBuilder.getModule,
                annotation.location
              ),
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
    val funcSchemaBldr = FunctionSchema
      .newBuilder()
      .annotations(annotations: _*)
      .argumentDefinitions(arguments: _*)
    if (methodDef.isPrivate) {
      funcSchemaBldr.projectPrivate()
    }
    val funcSchema = funcSchemaBldr.build()
    new RuntimeFunction(
      callTarget,
      null,
      funcSchema
    )
  }

  private def buildBuiltinFunction(
    fn: Function,
    expressionProcessor: ExpressionProcessor,
    methodDef: Method.Explicit,
    effectContext: Option[String],
    cons: Type,
    fullMethodDefName: String
  ): Either[CompilerError, Option[RuntimeFunction]] = {
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
                m.getFunction.getName,
                fn.arguments,
                fn.body,
                null,
                effectContext,
                true
              )
            val builtinRootNode =
              m.getFunction.getCallTarget.getRootNode
                .asInstanceOf[BuiltinRootNode]
            builtinRootNode
              .setModuleName(scopeBuilder.getModule.getName)
            builtinRootNode.setTypeName(cons.getQualifiedName)
            val funcSchemaBldr = FunctionSchema
              .newBuilder()
              .argumentDefinitions(bodyBuilder.args(): _*)
            if (methodDef.isPrivate) {
              funcSchemaBldr.projectPrivate()
            }
            val funcSchema = funcSchemaBldr.build()
            new RuntimeFunction(
              m.getFunction.getCallTarget,
              null,
              funcSchema
            )
          } else {
            m.getFunction
          }
        }
      )
  }

  private def getTypeAssociatedWithMethodDefinition(
    methodDef: Method.Explicit
  ): Option[Type] = {
    methodDef.methodReference.typePointer match {
      case None =>
        Some(scopeAssociatedType)
      case Some(tpePointer) =>
        tpePointer
          .getMetadata(MethodDefinitions)
          .map { res =>
            res.target match {
              case binding @ BindingsMap.ResolvedType(_, _) =>
                asType(binding)
              case BindingsMap.ResolvedModule(module) =>
                asAssociatedType(module.unsafeAsModule())
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
              case _: BindingsMap.ResolvedModuleMethod =>
                throw new CompilerError(
                  "Impossible module method here, should be caught by MethodDefinitions pass."
                )
              case _: BindingsMap.ResolvedExtensionMethod =>
                throw new CompilerError(
                  "Impossible static method here, should be caught by MethodDefinitions pass."
                )
              case _: BindingsMap.ResolvedConversionMethod =>
                throw new CompilerError(
                  "Impossible conversion method here, should be caught by MethodDefinitions pass."
                )
            }
          }
    }
  }

  private def registerConversions(module: Module): Unit = {
    val conversionDefs = module.bindings.collect {
      case conversion: definition.Method.Conversion =>
        conversion
    }

    // Register the conversion definitions in scope
    conversionDefs.foreach(methodDef => {
      lazy val where =
        s"conversion `${methodDef.typeName.map(_.name + ".").getOrElse("")}${methodDef.methodName.name}`."
      val scopeInfo = rootScopeInfo(where, methodDef)

      def dataflowInfo() = methodDef.unsafeGetMetadata(
        DataflowAnalysis,
        "Method definition missing dataflow information."
      )
      def frameInfo() = methodDef.unsafeGetMetadata(
        FramePointerAnalysis,
        "Method definition missing frame information."
      )

      val toOpt =
        methodDef.methodReference.typePointer match {
          case Some(tpePointer) =>
            getTypeResolution(tpePointer)
          case None =>
            Some(scopeAssociatedType)
        }
      val fromOpt = getTypeResolution(methodDef.sourceTypeName)
      toOpt.zip(fromOpt).foreach { case (toType, fromType) =>
        val expressionProcessor = new ExpressionProcessor(
          toType.getName ++ Constants.SCOPE_SEPARATOR ++ methodDef.methodName.name,
          () => scopeInfo().graph,
          () => scopeInfo().graph.rootScope,
          dataflowInfo,
          methodDef.methodName.name,
          frameInfo
        )

        val function = methodDef.body match {
          case fn: Function =>
            val bodyBuilder =
              new expressionProcessor.BuildFunctionBody(
                methodDef.methodName.name,
                fn.arguments,
                fn.body,
                ReadArgumentCheckNode.build(context, "conversion", toType),
                None,
                true
              )
            val rootNode = MethodRootNode.build(
              language,
              expressionProcessor.scope,
              scopeBuilder.asModuleScope(),
              () => bodyBuilder.bodyNode(),
              makeSection(scopeBuilder.getModule, methodDef.location),
              toType,
              methodDef.methodName.name
            )
            val callTarget = rootNode.getCallTarget
            val arguments  = bodyBuilder.args()
            val funcSchema = FunctionSchema
              .newBuilder()
              .argumentDefinitions(arguments: _*)
              .build()
            new RuntimeFunction(
              callTarget,
              null,
              funcSchema
            )
          case _ =>
            throw new CompilerError(
              "Conversion bodies must be functions at the point of codegen."
            )
        }
        scopeBuilder.registerConversionMethod(toType, fromType, function)
      }
    })
  }

  // ==========================================================================
  // === Utility Functions ====================================================
  // ==========================================================================

  private def extractAscribedType(
    comment: String,
    t: Expression
  ): ReadArgumentCheckNode = t match {
    case u: `type`.Set.Union =>
      val oneOf = u.operands.map(extractAscribedType(comment, _))
      if (oneOf.contains(null)) {
        null
      } else {
        ReadArgumentCheckNode.oneOf(
          comment,
          oneOf.asJava
        )
      }
    case i: `type`.Set.Intersection =>
      ReadArgumentCheckNode.allOf(
        comment,
        extractAscribedType(comment, i.left),
        extractAscribedType(comment, i.right)
      )
    case p: Application.Prefix => extractAscribedType(comment, p.function)
    case _: Tpe.Function =>
      ReadArgumentCheckNode.build(
        context,
        comment,
        context.getTopScope().getBuiltins().function()
      )
    case typeWithError: Tpe.Error =>
      // When checking a `a ! b` type, we ignore the error part as it is only used for documentation purposes and is not checked.
      extractAscribedType(comment, typeWithError.typed)
    case typeInContext: Tpe.Context =>
      // Type contexts aren't currently really used. But we should still check the base type.
      extractAscribedType(comment, typeInContext.typed)
    case t => {
      val res = t.getMetadata(TypeNames)
      res match {
        case Some(
              BindingsMap
                .Resolution(binding @ BindingsMap.ResolvedType(_, _))
            ) =>
          val typeOrAny = asType(binding)
          if (context.getBuiltins().any() == typeOrAny) {
            null
          } else {
            ReadArgumentCheckNode.build(context, comment, typeOrAny)
          }
        case Some(
              BindingsMap
                .Resolution(BindingsMap.ResolvedPolyglotSymbol(mod, symbol))
            ) =>
          ReadArgumentCheckNode.meta(
            comment,
            asScope(
              mod.unsafeAsModule().asInstanceOf[TruffleCompilerContext.Module]
            ).getPolyglotSymbolSupplier(symbol.name)
          )
        case _ => null
      }
    }
  }

  private def checkAsTypes(
    arg: DefinitionArgument
  ): ReadArgumentCheckNode = {
    val comment = "`" + arg.name.name + "`"
    arg.ascribedType.map(extractAscribedType(comment, _)).getOrElse(null)
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
    module: org.enso.interpreter.runtime.Module,
    location: Option[IdentifiedLocation]
  ): SourceSection = {
    location
      .map(loc => {
        val m = module
        if (m.isModuleSource(source)) {
          module.createSection(loc.start, loc.length)
        } else {
          source.createSection(loc.start, loc.length)
        }
      })
      .getOrElse(source.createUnavailableSection())
  }

  private def getTypeResolution(expr: IR): Option[Type] =
    expr.getMetadata(MethodDefinitions).map { res =>
      res.target match {
        case binding @ BindingsMap.ResolvedType(_, _) =>
          asType(binding)
        case BindingsMap.ResolvedModule(module) =>
          asAssociatedType(module.unsafeAsModule())
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
        case _: BindingsMap.ResolvedModuleMethod =>
          throw new CompilerError(
            "Impossible module method here, should be caught by MethodDefinitions pass."
          )
        case _: BindingsMap.ResolvedExtensionMethod =>
          throw new CompilerError(
            "Impossible static method here, should be caught by MethodDefinitions pass."
          )
        case _: BindingsMap.ResolvedConversionMethod =>
          throw new CompilerError(
            "Impossible conversion method here, should be caught by MethodDefinitions pass."
          )
      }
    }

  private def getTailStatus(
    expression: Expression
  ): BaseNode.TailStatus = {
    val isTailPosition =
      expression.getMetadata(TailCall.INSTANCE).isDefined
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

  /** Sets the source section for a given expression node to the provided
    * location.
    *
    * @param expr the expression to set the location for
    * @param location the location to assign to `expr`
    * @tparam T the type of `expr`
    * @return `expr` with its location set to `location`
    */
  private def setLocation[T <: RuntimeExpression](
    expr: T,
    location: IdentifiedLocation
  ): T = {
    if (location ne null) {
      expr.setSourceLocation(location.start, location.length)
      location.id.foreach { id => expr.setId(id) }
    }
    expr
  }

  private def generateReExportBindings(module: Module): Unit = {
    def mkConsGetter(constructor: AtomConstructor): RuntimeFunction =
      constructor.getAccessorFunction()

    def mkTypeGetter(tp: Type): RuntimeFunction = {
      val funcSchema = FunctionSchema
        .newBuilder()
        .argumentDefinitions(
          new ArgumentDefinition(
            0,
            ConstantsNames.SELF_ARGUMENT,
            null,
            null,
            ArgumentDefinition.ExecutionMode.EXECUTE
          )
        )
        .build()
      new RuntimeFunction(
        new ConstantNode(language, tp).getCallTarget,
        null,
        funcSchema
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
            .equals(scopeBuilder.getModule.asCompilerModule)
        ) {
          resolution match {
            case binding @ BindingsMap.ResolvedType(_, _) =>
              val runtimeTp =
                asType(binding)
              val fun = mkTypeGetter(runtimeTp)
              scopeBuilder.registerMethod(
                scopeAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedConstructor(definitionType, cons) =>
              val tpe = asType(definitionType)
              val runtimeCons =
                tpe.getConstructors
                  .get(cons.name)
              val fun = mkConsGetter(runtimeCons)
              scopeBuilder.registerMethod(
                scopeAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedModule(module) =>
              val runtimeCons =
                asAssociatedType(module.unsafeAsModule())
              val fun = mkTypeGetter(runtimeCons)
              scopeBuilder.registerMethod(
                scopeAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedModuleMethod(module, method) =>
              val actualModule = module.unsafeAsModule()
              val fun = asScope(actualModule)
                .getMethodForType(
                  asAssociatedType(actualModule),
                  method.name
                )
              org.enso.common.Asserts.assertInJvm(
                fun != null,
                s"exported symbol `${method.name}` needs to be registered first in the module "
              )
              scopeBuilder.registerMethod(
                scopeAssociatedType,
                name,
                fun
              )
            case BindingsMap.ResolvedExtensionMethod(module, staticMethod) =>
              val actualModule = module.unsafeAsModule()
              val currentScope = asScope(actualModule)
              actualModule.getBindingsMap.resolveName(
                staticMethod.tpName
              ) match {
                case Right(List(BindingsMap.ResolvedType(modWithTp, _))) =>
                  val tpScope = asScope(modWithTp.unsafeAsModule())
                  val tp      = tpScope.getType(staticMethod.tpName, true)
                  org.enso.common.Asserts.assertInJvm(
                    tp != null,
                    s"Type should be defined in module ${modWithTp.getName}"
                  )
                  // We have to search for the method on eigen type, because it is a static method.
                  // Static methods are always defined on eigen types
                  val eigenTp = tp.getEigentype
                  val fun =
                    currentScope.getMethodForType(
                      eigenTp,
                      staticMethod.methodName
                    )
                  org.enso.common.Asserts.assertInJvm(
                    fun != null,
                    s"exported symbol (static method) `${staticMethod.name}` needs to be registered first in the module "
                  )
                  scopeBuilder.registerMethod(
                    scopeAssociatedType,
                    name,
                    fun
                  )
                case _ =>
                  throw new CompilerError(
                    s"Type ${staticMethod.tpName} should be resolvable in module ${module.getName}"
                  )
              }
            case BindingsMap.ResolvedConversionMethod(
                  module,
                  conversionMethod
                ) =>
              val actualModule = module.unsafeAsModule()
              val actualScope  = asScope(actualModule)
              actualModule.getBindingsMap.resolveName(
                conversionMethod.targetTpName
              ) match {
                case Right(
                      List(BindingsMap.ResolvedType(modWithTargetTp, _))
                    ) =>
                  val targetTpScope = asScope(modWithTargetTp.unsafeAsModule())
                  val targetTp =
                    targetTpScope.getType(conversionMethod.targetTpName, true)
                  org.enso.common.Asserts.assertInJvm(
                    targetTp != null,
                    s"Target type should be defined in module ${module.getName}"
                  )
                  actualModule.getBindingsMap.resolveName(
                    conversionMethod.sourceTpName
                  ) match {
                    case Right(
                          List(BindingsMap.ResolvedType(modWithSourceTp, _))
                        ) =>
                      val sourceTpScope =
                        asScope(modWithSourceTp.unsafeAsModule())
                      val sourceTp = sourceTpScope.getType(
                        conversionMethod.sourceTpName,
                        true
                      )
                      org.enso.common.Asserts.assertInJvm(
                        sourceTp != null,
                        s"Source type should be defined in module ${module.getName}"
                      )
                      val conversionFun =
                        actualScope.lookupConversionDefinition(
                          sourceTp,
                          targetTp
                        )
                      org.enso.common.Asserts.assertInJvm(
                        conversionFun != null,
                        s"Conversion method `$conversionMethod` should be defined in module ${module.getName}"
                      )
                      scopeBuilder.registerConversionMethod(
                        targetTp,
                        sourceTp,
                        conversionFun
                      )
                    case _ =>
                      throw new CompilerError(
                        s"Source type ${conversionMethod.sourceTpName} should be resolvable in module ${module.getName}"
                      )
                  }
                case _ =>
                  throw new CompilerError(
                    s"Target type ${conversionMethod.targetTpName} should be resolvable in module ${module.getName}"
                  )
              }
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
    * @param initialName suggested name for a first closure
    */
  sealed private class ExpressionProcessor(
    val scope: LocalScope,
    val scopeName: String,
    private val initialName: String
  ) {

    private var currentVarName = initialName

    // === Construction =======================================================

    /** Constructs an [[ExpressionProcessor]] instance with a defaulted local
      * scope.
      *
      * @param scopeName the name to attribute to the default local scope.
      * @param initialName suggested name for a first closure
      */
    def this(
      scopeName: String,
      graph: () => AliasGraph,
      scope: () => AliasScope,
      dataflowInfo: () => DataflowAnalysis.Metadata,
      initialName: String,
      frameInfo: () => FramePointerAnalysis.Metadata = null
    ) = {
      this(
        new LocalScope(None, graph, scope, dataflowInfo, frameInfo),
        scopeName,
        initialName
      )
    }

    /** Creates an instance of [[ExpressionProcessor]] that operates in a child
      * scope of `this`.
      *
      * @param name the name of the child scope
      * @param initialName suggested name for a first closure
      * @return an expression processor operating on a child scope
      */
    def createChild(
      name: String,
      scope: () => AliasScope,
      initialName: String,
      symbolsProvider: () => FrameVariableNames = null
    ): ExpressionProcessor = {
      val childScope =
        this.scope.createChild(scope, symbolsProvider = symbolsProvider)
      new ExpressionProcessor(childScope, name, initialName)
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
        case asc: Tpe.Ascription =>
          val checkNode =
            extractAscribedType(asc.comment.orNull, asc.signature)
          if (checkNode != null) {
            val body = run(asc.typed, binding, subjectToInstrumentation)
            ReadArgumentCheckNode.wrap(body, checkNode)
          } else {
            processType(asc)
          }
        case typ: Tpe => processType(typ)
        case _: Empty =>
          processEmpty()
        case _: Comment =>
          throw new CompilerError(
            "Comments should not be present during codegen."
          )
        case err: Error => processError(err)
        case Foreign.Definition(_, _, _, _) =>
          throw new CompilerError(
            s"Foreign expressions not yet implemented: $ir."
          )
      }
      runtimeExpression.setTailStatus(getTailStatus(ir))

      ir match {
        case _: Expression.Binding =>
        case _ =>
          val types: Option[TypeSignatures.Signature] =
            ir.getMetadata(TypeSignatures)
          types.foreach { tpe =>
            val checkNode =
              extractAscribedType(tpe.comment.orNull, tpe.signature)
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
        val scopeInfo = childScopeInfo("block", block)
        def frameInfo() = block
          .unsafeGetMetadata(
            FramePointerAnalysis,
            "Method definition missing frame information."
          )
          .asInstanceOf[FrameVariableNames]

        val childFactory = this.createChild(
          "suspended-block",
          () => scopeInfo().scope,
          "suspended " + currentVarName,
          frameInfo
        )
        val childScope = childFactory.scope

        val blockNode = childFactory.processBlock(block.copy(suspended = false))

        val defaultRootNode = ClosureRootNode.build(
          language,
          childScope,
          scopeBuilder.asModuleScope(),
          blockNode,
          makeSection(scopeBuilder.getModule, block.location),
          currentVarName,
          false,
          false
        )

        val callTarget = defaultRootNode.getCallTarget
        setLocation(CreateThunkNode.build(callTarget), block.location)
      } else {
        val statementExprs = block.expressions.map(this.run(_, true)).toArray
        val retExpr        = this.run(block.returnValue, true)

        val blockNode = BlockNode.buildSilent(statementExprs, retExpr)
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
              "Type operators are not currently supported at runtime"
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
        case caseExpr @ Case.Expr(scrutinee, branches, isNested, location, _) =>
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
              .makeCompileError(message)

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
      val scopeInfo = childScopeInfo("case branch", branch)
      def frameInfo() = branch
        .unsafeGetMetadata(
          FramePointerAnalysis,
          "Method definition missing frame information."
        )
        .asInstanceOf[FrameVariableNames]
      val childProcessor =
        this.createChild(
          "case_branch",
          () => scopeInfo().scope,
          "case " + currentVarName,
          frameInfo
        )

      branch.pattern match {
        case named: Pattern.Name =>
          val arg = List(genArgFromMatchField(named))

          val branchCodeNode = childProcessor.processFunctionBody(
            arg,
            branch.expression,
            branch.location
          )

          val branchNode =
            CatchAllBranchNode.build(branchCodeNode.getCallTarget, true)

          Right(branchNode)
        case cons @ Pattern.Constructor(constructor, _, _, _) =>
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
                      LiteralNode.build(asAssociatedType(mod.unsafeAsModule())),
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
                      BindingsMap.Resolution(
                        binding @ BindingsMap.ResolvedType(_, _)
                      )
                    ) =>
                  val tpe =
                    asType(binding)
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
                      LiteralNode.build(tpe),
                      branch.terminalBranch
                    )
                  }
                  Right(branchNode)
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedPolyglotSymbol(mod, symbol)
                      )
                    ) =>
                  val polyglotSymbol =
                    asScope(mod.unsafeAsModule())
                      .getPolyglotSymbolSupplier(symbol.name)
                  Either.cond(
                    polyglotSymbol != null,
                    ObjectEqualityBranchNode
                      .build(
                        branchCodeNode.getCallTarget,
                        LazyObjectNode.build(symbol.name, polyglotSymbol),
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
                  val polyClass = asScope(mod.unsafeAsModule())
                    .getPolyglotSymbolSupplier(typ.symbol.name)
                    .get()

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
                            val value = iop.readMember(polyClass, symbol)
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
                        ConstantObjectNode.build(polyValue),
                        branch.terminalBranch
                      )
                  })
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedModuleMethod(_, _)
                      )
                    ) =>
                  throw new CompilerError(
                    "Impossible module method here, should be caught by Patterns resolution pass."
                  )
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedExtensionMethod(_, _)
                      )
                    ) =>
                  throw new CompilerError(
                    "Impossible static method here, should be caught by Patterns resolution pass."
                  )
                case Some(
                      BindingsMap.Resolution(
                        BindingsMap.ResolvedConversionMethod(_, _)
                      )
                    ) =>
                  throw new CompilerError(
                    "Impossible conversion method here, should be caught by Patterns resolution pass."
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
        case typePattern: Pattern.Type =>
          typePattern.tpe.getMetadata(Patterns) match {
            case None =>
              Left(BadPatternMatch.NonVisibleType(typePattern.tpe.name))
            case Some(
                  BindingsMap.Resolution(
                    binding @ BindingsMap.ResolvedType(_, _)
                  )
                ) =>
              // Using .getTypes because .getType may return an associated type
              Option(asType(binding)) match {
                case Some(tpe) =>
                  val argOfType = List(
                    new DefinitionArgument.Specified(
                      typePattern.name,
                      None,
                      None,
                      suspended = false,
                      typePattern.identifiedLocation,
                      passData    = typePattern.name.passData,
                      diagnostics = typePattern.name.diagnostics
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
                case None =>
                  Left(BadPatternMatch.NonVisibleType(typePattern.tpe.name))
              }
            case Some(
                  BindingsMap.Resolution(
                    BindingsMap.ResolvedPolyglotSymbol(mod, symbol)
                  )
                ) =>
              val polySymbol =
                asScope(mod.unsafeAsModule())
                  .getPolyglotSymbolSupplier(symbol.name)
                  .get()
              if (polySymbol != null) {
                val argOfType = List(
                  new DefinitionArgument.Specified(
                    typePattern.name,
                    None,
                    None,
                    suspended = false,
                    typePattern.identifiedLocation,
                    passData    = typePattern.name.passData,
                    diagnostics = typePattern.name.diagnostics
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
                Left(
                  BadPatternMatch.NonVisiblePolyglotSymbol(
                    typePattern.name.name
                  )
                )
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
    private def genArgFromMatchField(name: Pattern.Name): DefinitionArgument = {
      new DefinitionArgument.Specified(
        name.name,
        None,
        None,
        suspended = false,
        name.identifiedLocation,
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
      val fp = binding
        .unsafeGetMetadata(
          FramePointerAnalysis,
          "Binding with missing frame pointer."
        )
        .asInstanceOf[FramePointer]

      currentVarName = binding.name.name
      val slotIdx = fp.frameSlotIdx()
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
      val scopeInfo = childScopeInfo("function", function)
      if (function.body.isInstanceOf[Function]) {
        throw new CompilerError(
          "Lambda found directly as function body. It looks like Lambda " +
          "Consolidation hasn't run."
        )
      }
      def frameInfo() = function
        .unsafeGetMetadata(
          FramePointerAnalysis,
          "Method definition missing frame information."
        )
        .asInstanceOf[FrameVariableNames]

      val scopeName = if (function.canBeTCO) {
        this.scopeName + "." + currentVarName
      } else {
        "case_expression"
      }

      val child =
        this.createChild(
          scopeName,
          () => scopeInfo().scope,
          "case " + currentVarName,
          frameInfo
        )

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
        case literalName: Name.Literal =>
          val resolver = new RuntimeNameResolution()
          val fpMeta = literalName.passData.get(FramePointerAnalysis) match {
            case Some(meta: FramePointer) => meta
            case _                        => null
          }
          resolver.resolveName(literalName, fpMeta)
        case Name.MethodReference(
              None,
              Name.Literal(nameStr, _, _, _, _),
              _,
              _
            ) =>
          DynamicSymbolNode.buildUnresolvedConstructor(nameStr)
        case Name.Self(location, _, passData) =>
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
        case Name.Special(name, _, _) =>
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
        case _: Name.Qualified =>
          throw new CompilerError(
            "Qualified names should not be present at codegen time."
          )
        case err: errors.Resolution => processError(err)
        case err: errors.Conversion => processError(err)
      }

      setLocation(nameExpr, name.location)
    }

    private class RuntimeNameResolution
        extends NameResolutionAlgorithm[
          RuntimeExpression,
          FramePointer,
          FramePointer
        ] {
      override protected def findLocalLink(
        fpMeta: FramePointer
      ): Option[FramePointer] = {
        if (scope.flattenToParent && fpMeta.parentLevel() > 0) {
          Some(
            new FramePointer(fpMeta.parentLevel() - 1, fpMeta.frameSlotIdx())
          )
        } else {
          Some(fpMeta)
        }
      }

      override protected def resolveLocalName(
        localLink: FramePointer
      ): RuntimeExpression =
        ReadLocalVariableNode.build(localLink)

      override protected def resolveGlobalName(
        resolvedName: BindingsMap.ResolvedName
      ): RuntimeExpression =
        nodeForResolution(resolvedName)

      override protected def resolveFromConversion(): RuntimeExpression =
        ConstantObjectNode.build(
          UnresolvedConversion.build(scopeBuilder.asModuleScope())
        )

      override protected def resolveUnresolvedSymbol(
        symbolName: String
      ): RuntimeExpression =
        DynamicSymbolNode.build(
          UnresolvedSymbol.build(symbolName, scopeBuilder.asModuleScope())
        )
    }

    private def nodeForResolution(
      resolution: BindingsMap.ResolvedName
    ): RuntimeExpression = {
      resolution match {
        case tp: BindingsMap.ResolvedType =>
          ConstantObjectNode.build(asType(tp))
        case BindingsMap.ResolvedConstructor(definitionType, cons) =>
          val c = asType(definitionType).getConstructors
            .get(cons.name)
          if (c == null) {
            throw new CompilerError(s"Constructor for $cons is null")
          }
          ConstructorNode.build(c)
        case BindingsMap.ResolvedModule(module) =>
          ConstantObjectNode.build(
            asAssociatedType(module.unsafeAsModule())
          )
        case BindingsMap.ResolvedPolyglotSymbol(module, symbol) =>
          val s =
            asScope(module.unsafeAsModule())
              .getPolyglotSymbolSupplier(symbol.name)
          LazyObjectNode.build(symbol.name, s)
        case BindingsMap.ResolvedPolyglotField(symbol, name) =>
          val s =
            asScope(symbol.module.unsafeAsModule())
              .getPolyglotSymbolSupplier(name)
          LazyObjectNode.build(name, s)
        case BindingsMap.ResolvedModuleMethod(_, method) =>
          throw new CompilerError(
            s"Impossible here, module method ${method.name} should be caught when translating application"
          )
        case BindingsMap.ResolvedExtensionMethod(_, staticMethod) =>
          throw new CompilerError(
            s"Impossible here, static method ${staticMethod.name} should be caught when translating application"
          )
        case BindingsMap.ResolvedConversionMethod(_, conversionMethod) =>
          throw new CompilerError(
            s"Impossible here, conversion method ${conversionMethod.targetTpName}.${conversionMethod.methodName} should be caught when translating application"
          )
      }
    }

    /** Generates code for an Enso literal.
      *
      * @param literal the literal to generate code for
      * @return the truffle nodes corresponding to `literal`
      */
    @throws[CompilerError]
    private def processLiteral(literal: Literal): RuntimeExpression =
      literal match {
        case lit: Literal.Number =>
          val node = lit.numericValue match {
            case l: Long       => LiteralNode.build(l)
            case d: Double     => LiteralNode.build(d)
            case b: BigInteger => LiteralNode.build(b)
          }
          setLocation(node, lit.location)
        case lit: Literal.Text =>
          setLocation(LiteralNode.build(lit.text), lit.location)
      }

    private def fileLocationFromSection(loc: IdentifiedLocation) = {
      val section =
        source.createSection(loc.location().start(), loc.location().length())
      val locStr = "" + section.getStartLine() + ":" + section
        .getStartColumn() + "-" + section.getEndLine() + ":" + section
        .getEndColumn()
      source.getName() + "[" + locStr + "]"
    }

    /** Generates a runtime implementation for compile error nodes.
      *
      * @param error the IR representing a compile error.
      * @return a runtime node representing the error.
      */
    private def processError(error: Error): RuntimeExpression = {
      val payload: Atom = error match {
        case Error.InvalidIR(_, _) =>
          throw new CompilerError("Unexpected Invalid IR during codegen.")
        case err: errors.Syntax =>
          context.getBuiltins
            .error()
            .makeSyntaxError(err.message(fileLocationFromSection))
        case err: errors.Redefined.Binding =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Redefined.Method =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Redefined.MethodClashWithAtom =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Redefined.Conversion =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Redefined.Type =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Redefined.SelfArg =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Redefined.Arg =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Unexpected.TypeSignature =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Resolution =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
        case err: errors.Conversion =>
          context.getBuiltins
            .error()
            .makeCompileError(err.message(fileLocationFromSection))
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

    /** Processes an empty expression.
      *
      * @return the Nothing builtin
      */
    private def processEmpty(): RuntimeExpression = {
      LiteralNode.build(context.getBuiltins.nothing())
    }

    /** Processes function arguments, generates arguments reads and creates
      * a node to represent the whole method body.
      *
      * @param arguments the argument definitions
      * @param body      the body definition
      * @param initialName suggested name for a first closure
      * @return a node for the final shape of function body and pre-processed
      *         argument definitions.
      */
    class BuildFunctionBody(
      val initialName: String,
      val arguments: List[DefinitionArgument],
      val body: Expression,
      val typeCheck: ReadArgumentCheckNode,
      val effectContext: Option[String],
      val subjectToInstrumentation: Boolean
    ) {
      private val argFactory =
        new DefinitionArgumentProcessor(scopeName, scope, initialName)
      private lazy val slots = computeSlots()
      lazy val argsExpr      = computeArgsAndExpression()

      def args(): Array[ArgumentDefinition] = slots._2
      def bodyNode(): RuntimeExpression = {
        val body = BlockNode.buildRoot(Array(), argsExpr._2)
        val initVariablesAndThenBody =
          BlockNode.buildSilent(argsExpr._1.toArray, body)
        initVariablesAndThenBody
      }

      private def computeArgsAndExpression()
        : (Array[RuntimeExpression], RuntimeExpression) = {
        val (argSlotIdxs, _, argExpressions) = slots

        val bodyExpr = body match {
          case Foreign.Definition(lang, code, _, _) =>
            buildForeignBody(
              lang,
              body.location,
              code,
              arguments.map(_.name.name),
              argSlotIdxs
            )
          case _ =>
            ExpressionProcessor.this.run(body, false, subjectToInstrumentation)
        }

        if (typeCheck == null) {
          (argExpressions.toArray, bodyExpr)
        } else {
          val bodyWithCheck = ReadArgumentCheckNode.wrap(bodyExpr, typeCheck)
          (argExpressions.toArray, bodyWithCheck)
        }
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
            val fp = unprocessedArg
              .unsafeGetMetadata(
                FramePointerAnalysis,
                "No frame pointer on an argument definition."
              )
              .asInstanceOf[FramePointer]
            val slotIdx = fp.frameSlotIdx()
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
      language: String,
      location: Option[IdentifiedLocation],
      code: String,
      argumentNames: List[String],
      argumentSlotIdxs: List[Int]
    ): RuntimeExpression = {
      val line = location
        .map(l => source.createSection(l.start, l.length).getStartLine())
        .getOrElse(0)
      val name = scopeName.replace('.', '_') + "." + language
      val b    = Source.newBuilder("epb", language + ":" + line + "#" + code, name)
      b.uri(source.getURI())
      val src       = b.build()
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
      val bodyBuilder =
        new BuildFunctionBody(scopeName, arguments, body, null, None, false)
      val fnRootNode = ClosureRootNode.build(
        language,
        scope,
        scopeBuilder.asModuleScope(),
        bodyBuilder.bodyNode(),
        makeSection(scopeBuilder.getModule, location),
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
        case Application.Prefix(fn, Nil, true, _, _) =>
          run(fn, subjectToInstrumentation)
        case app: Application.Prefix =>
          processApplicationWithArgs(app, subjectToInstrumentation)
        case Application.Force(expr, location, _) =>
          setLocation(
            ForceNode.build(this.run(expr, subjectToInstrumentation)),
            location
          )
        case Application.Sequence(items, location, _) =>
          val itemNodes = items.map(run(_, subjectToInstrumentation)).toArray
          setLocation(SequenceLiteralNode.build(itemNodes), location)
        case _: Application.Typeset =>
          setLocation(
            ErrorNode.build(
              context.getBuiltins
                .error()
                .makeSyntaxError(
                  "Typeset literals are not yet supported at runtime"
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
      val Application.Prefix(fn, args, hasDefaultsSuspended, loc, _) =
        application
      val callArgFactory =
        new CallArgumentProcessor(scope, scopeName, currentVarName)

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
    * @param initialName suggested name for a first closure
    */
  sealed private class CallArgumentProcessor(
    val scope: LocalScope,
    val scopeName: String,
    private val initialName: String
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
        case CallArgument.Specified(name, value, _, _) =>
          val scopeInfo = childScopeInfo("call argument", arg)

          def valueHasSomeTypeCheck() =
            value.getMetadata(TypeSignatures).isDefined

          val shouldCreateClosureRootNode = value match {
            case _: Name           => valueHasSomeTypeCheck()
            case _: Literal.Text   => valueHasSomeTypeCheck()
            case _: Literal.Number => valueHasSomeTypeCheck()
            case _                 => true
          }

          val childScope = if (shouldCreateClosureRootNode) {
            def frameInfo() = arg
              .unsafeGetMetadata(
                FramePointerAnalysis,
                "Method definition missing frame information."
              )
              .asInstanceOf[FrameVariableNames]

            scope.createChild(
              () => scopeInfo().scope,
              symbolsProvider = frameInfo
            )
          } else {
            // Note [Scope Flattening]
            scope.createChild(() => scopeInfo().scope, flattenToParent = true)
          }
          val argumentExpression =
            new ExpressionProcessor(childScope, scopeName, initialName)
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
              scopeBuilder.asModuleScope(),
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
    * @param initialName suggested name for a first closure
    */
  sealed private class DefinitionArgumentProcessor(
    val scopeName: String = "<root>",
    val scope: LocalScope,
    private val initialName: String
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
      * @param types null or node to check the argument type for
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
            .map(
              new ExpressionProcessor(scope, scopeName, initialName)
                .run(_, false)
            )
            .orNull

          // Note [Handling Suspended Defaults]
          val defaultedValue = if (arg.suspended && defaultExpression != null) {
            org.enso.common.Asserts.assertInJvm(arg.defaultValue.isDefined)
            val defaultRootNode = ClosureRootNode.build(
              language,
              scope,
              scopeBuilder.asModuleScope(),
              defaultExpression,
              makeSection(
                scopeBuilder.getModule,
                arg.defaultValue.get.location()
              ),
              s"<default::$scopeName::${arg.name.showCode()}>",
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

  private def asType(
    typ: BindingsMap.ResolvedType
  ): Type = {
    val m = org.enso.interpreter.runtime.Module
      .fromCompilerModule(typ.module.unsafeAsModule())
    m.getScope().getType(typ.tp.name, true)
  }

  private def asAssociatedType(
    module: CompilerContext.Module
  ): Type = {
    val m = org.enso.interpreter.runtime.Module.fromCompilerModule(module)
    m.getScope().getAssociatedType()
  }

  private def scopeAssociatedType =
    scopeBuilder.asModuleScope().getAssociatedType

  private def rootScopeInfo(
    where: => String,
    ir: IR
  ): () => AliasMetadata.RootScope = {
    def readScopeInfo() = {
      val raw =
        ir.unsafeGetMetadata(AliasAnalysis, s"No root scope for ${where}.")
      val scope = raw.unsafeAs[AliasMetadata.RootScope]

      val log = context.getLogger()
      if (log.isLoggable(Level.FINEST)) {
        val allDefs = scope.graph.rootScope.allDefinitions
        log.log(Level.FINEST, s"Scope for ${where} loaded with {0}", allDefs)
      }
      scope
    }
    readScopeInfo
  }

  private def childScopeInfo(
    where: => String,
    ir: IR
  ): () => AliasMetadata.ChildScope = {
    def readScopeInfo() = {
      val raw =
        ir.unsafeGetMetadata(AliasAnalysis, s"No root scope for ${where}.")
      val scope = raw.unsafeAs[AliasMetadata.ChildScope]

      val log = context.getLogger()
      if (log.isLoggable(Level.FINEST)) {
        val allDefs = scope.graph.rootScope.allDefinitions
        log.log(Level.FINEST, s"Scope for ${where} loaded with {0}", allDefs)
      }
      scope
    }
    readScopeInfo
  }

}
