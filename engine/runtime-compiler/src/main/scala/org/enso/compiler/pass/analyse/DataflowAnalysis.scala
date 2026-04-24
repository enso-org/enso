package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.Implicits.AsMetadata
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.{
  errors,
  Application,
  Case,
  Comment,
  Error,
  Foreign,
  Operator
}
import org.enso.compiler.core.ir.{
  `type`,
  CallArgument,
  DefinitionArgument,
  Empty,
  Expression,
  Function,
  Literal,
  Module,
  Name,
  Pattern,
  Type
}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRProcessingPass
import org.enso.compiler.pass.analyse.DependencyInfo.Type.asStatic
import org.enso.compiler.pass.analyse.alias.graph.GraphOccurrence
import org.enso.persist.Persistance
import org.enso.compiler.pass.analyse.DependencyInfo

/** This pass implements dataflow analysis for Enso.
  *
  * Dataflow analysis is the processes of determining the dependencies between
  * program expressions.
  *
  * This pass requires the context to provide:
  *
  * - A [[org.enso.compiler.context.LocalScope]], where relevant.
  *
  * It requires that all members of [[org.enso.compiler.core.ir.IRKind.Primitive]] have been removed
  * from the IR by the time it runs.
  */
//noinspection DuplicatedCode
case object DataflowAnalysis extends IRPass {
  override type Metadata = DependencyInfo
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    AliasAnalysis,
    DemandAnalysis,
    TailCall.INSTANCE
  )

  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List()

  /** Executes the dataflow analysis process on an Enso module.
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
    val builders = (DependencyMapping.newBuilder, DependencyMapping.newBuilder)
    ir.copyWithBindings(
      ir.bindings.map(analyseModuleDefinition(_, builders))
    ).updateMetadata(toMetadata(builders))
  }

  /** Performs dataflow analysis on an inline expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    val localScope = inlineContext.localScope.getOrElse(
      throw new CompilerError(
        "A valid local scope is required for the inline flow."
      )
    )
    val info = localScope.dataflowInfo
    val builders = (
      DependencyMapping.newBuilder(info.dependents),
      DependencyMapping.newBuilder(info.dependencies)
    )
    analyseExpression(ir, builders)
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    sourceIr: T,
    copyOfIr: T
  ): T = {
    (sourceIr, copyOfIr) match {
      case (sourceIr: Module, copyOfIr: Module) =>
        val sourceMeta =
          sourceIr.unsafeGetMetadata[DataflowAnalysis.Metadata](
            this,
            "Dataflow Analysis must have run."
          )
        val copyMeta = new DependencyInfo(
          sourceMeta.dependents.deepCopy,
          sourceMeta.dependencies.deepCopy
        )

        val sourceNodes = sourceIr.preorder
        val copyNodes   = copyOfIr.preorder

        sourceNodes.lazyZip(copyNodes).foreach { case (src, copy) =>
          src
            .getMetadata(this, classOf[DataflowAnalysis.Metadata])
            .foreach(_ => copy.updateMetadata(new MetadataPair(this, copyMeta)))
        }

        copyOfIr.asInstanceOf[T]
      case _ => copyOfIr
    }
    copyOfIr
  }

  // === Pass Internals =======================================================

  /** Performs dataflow analysis on a module definition.
    *
    * Atoms are dependent on the definitions of their arguments, while methods
    * are dependent on the definitions of their bodies.
    *
    * @param binding the binding to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `binding`, with attached dependency information
    */
  // TODO [AA] Can I abstract the pattern here?
  private def analyseModuleDefinition(
    binding: Definition,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Definition = {
    binding match {
      case m: definition.Method.Conversion =>
        val bodyDep       = asStatic(m.body)
        val methodDep     = asStatic(m)
        val sourceTypeDep = asStatic(m.sourceTypeName)
        info._1.updateAt(sourceTypeDep, Set(methodDep))
        info._1.updateAt(bodyDep, Set(methodDep))
        info._2.updateAt(methodDep, Set(bodyDep, sourceTypeDep))

        m.copyBuilder()
          .body(analyseExpression(m.body(), info))
          .sourceTypeName(
            m.sourceTypeName().updateMetadata(toMetadata(info))
          )
          .build()
          .updateMetadata(toMetadata(info))
      case method: definition.Method.Explicit =>
        val body      = method.body()
        val bodyDep   = asStatic(body)
        val methodDep = asStatic(method)
        info._1.updateAt(bodyDep, Set(methodDep))
        info._2.update(methodDep, Set(bodyDep))
        method
          .copyBuilder()
          .bodyReference(
            Persistance.Reference.of(analyseExpression(body, info))
          )
          .build()
          .updateMetadata(toMetadata(info))
      case tp: Definition.Type =>
        val params  = tp.params()
        val members = tp.members()
        val tpDep   = asStatic(tp)
        val newParams = params.map { param =>
          val paramDep = asStatic(param)
          info._1.updateAt(paramDep, Set(tpDep))
          info._2.updateAt(tpDep, Set(paramDep))
          analyseDefinitionArgument(param, info)
        }
        val newMembers = members.map { data =>
          val dataDep = asStatic(data)
          info._1.updateAt(dataDep, Set(tpDep))
          info._2.updateAt(tpDep, Set(dataDep))
          data.arguments.foreach(arg => {
            val argDep = asStatic(arg)
            info._1.updateAt(argDep, Set(dataDep))
            info._2.updateAt(dataDep, Set(argDep))
          })

          data
            .copyBuilder()
            .arguments(
              data.arguments.map(analyseDefinitionArgument(_, info))
            )
            .build()
            .updateMetadata(toMetadata(info))
        }
        tp.copyBuilder()
          .params(newParams)
          .members(newMembers)
          .build()
          .updateMetadata(toMetadata(info))
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Sugared method definitions should not occur during dataflow " +
          "analysis."
        )
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present during " +
          "dataflow analysis."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation should not exist as an entity during dataflow analysis."
        )
      case _: Type.Ascription =>
        throw new CompilerError(
          "Type signatures should not exist at the top level during " +
          "dataflow analysis."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "dataflow analysis."
        )
      case ann: Name.GenericAnnotation =>
        ann
          .copyBuilder()
          .expression(analyseExpression(ann.expression, info))
          .build()
          .updateMetadata(toMetadata(info))
      case err: Error => err
    }
  }

  /** Performs dependency analysis on an arbitrary expression.
    *
    * The value of a block depends on its return value, while the value of a
    * binding depends on the expression being bound and the name being bound to.
    *
    * @param expression the expression to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `expression`, with attached dependency information
    */
  private def analyseExpression(
    expression: Expression,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Expression = {
    expression match {
      case empty: Empty       => empty.updateMetadata(toMetadata(info))
      case function: Function => analyseFunction(function, info)
      case app: Application   => analyseApplication(app, info)
      case typ: Type          => analyseType(typ, info)
      case name: Name         => analyseName(name, info)
      case cse: Case          => analyseCase(cse, info)
      case literal: Literal =>
        literal.updateMetadata(toMetadata(info))
      case foreign: Foreign =>
        foreign.updateMetadata(toMetadata(info))

      case block: Expression.Block =>
        val retValDep = asStatic(block.returnValue)
        val blockDep  = asStatic(block)
        info._1.updateAt(retValDep, Set(blockDep))
        info._2.updateAt(blockDep, Set(retValDep))

        val newExprs  = block.expressions().map(analyseExpression(_, info))
        val newRetVal = analyseExpression(block.returnValue, info)
        block
          .copyBuilder()
          .expressions(newExprs)
          .returnValue(newRetVal)
          .build()
          .updateMetadata(toMetadata(info))
      case binding: Expression.Binding =>
        val expressionDep = asStatic(binding.expression)
        val nameDep       = asStatic(binding.name)
        val bindingDep    = asStatic(binding)
        info._1.updateAt(expressionDep, Set(bindingDep))
        info._1.updateAt(nameDep, Set(bindingDep))
        info._2.updateAt(bindingDep, Set(expressionDep, nameDep))

        binding
          .copyBuilder()
          .name(binding.name().updateMetadata(toMetadata(info)))
          .expression(analyseExpression(binding.expression(), info))
          .build()
          .updateMetadata(toMetadata(info))

      case error: Error => error
      case _: Comment =>
        throw new CompilerError(
          "Comments should not be present during dataflow analysis."
        )
    }
  }

  /** Performs dataflow analysis on a function.
    *
    * The result of a function is dependent on the result from its body, as well
    * as the definitions of any defaults for its arguments.
    *
    * @param function the function to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `function`, with attached dependency information
    */
  private def analyseFunction(
    function: Function,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Function = {
    function match {
      case lam: Function.Lambda =>
        val body      = lam.body()
        val arguments = lam.arguments()
        val bodyDep   = asStatic(body)
        val lamDep    = asStatic(lam)
        info._1.updateAt(bodyDep, Set(lamDep))
        info._2.updateAt(lamDep, Set(bodyDep))

        lam
          .copyWithArgumentsAndBody(
            arguments.map(analyseDefinitionArgument(_, info)),
            analyseExpression(body, info)
          )
          .updateMetadata(toMetadata(info))
      case _: Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during dataflow analysis."
        )
    }
  }

  /** Performs dependency analysis on an application.
    *
    * A prefix application depends on the values of the function and arguments,
    * while a force depends purely on the term being forced.
    *
    * @param application the application to perform dependency analysis on
    * @param info the dependency information for the module
    * @return `application`, with attached dependency information
    */
  private def analyseApplication(
    application: Application,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Application = {
    application match {
      case prefix: Application.Prefix =>
        val fnDep     = asStatic(prefix.function)
        val prefixDep = asStatic(prefix)
        info._1.updateAt(fnDep, Set(prefixDep))
        prefix.arguments().headOption.map(_.value()).foreach {
          case literalSelfArg: Name.Literal =>
            // Self arguments are cached, so whenever the type (or method in it)
            // changes, then it should be invalidated as well.
            // Tracking a dependency between the application
            // and the self argument ensures correct invalidation.
            val selfArgDep = asStatic(literalSelfArg)
            info._1.updateAt(prefixDep, Set(selfArgDep))
          case _ =>
        }
        info._2.updateAt(prefixDep, Set(fnDep))
        prefix.arguments.foreach(arg => {
          val argDep = asStatic(arg)
          info._1.updateAt(argDep, Set(prefixDep))
          info._2.updateAt(prefixDep, Set(argDep))
        })

        prefix
          .copy(
            analyseExpression(prefix.function, info),
            prefix.arguments.map(analyseCallArgument(_, info))
          )
          .updateMetadata(toMetadata(info))
      case force: Application.Force =>
        val targetDep = asStatic(force.target)
        val forceDep  = asStatic(force)
        info._1.updateAt(targetDep, Set(forceDep))
        info._2.updateAt(forceDep, Set(targetDep))

        force
          .copyWithTarget(analyseExpression(force.target, info))
          .updateMetadata(toMetadata(info))
      case vector: Application.Sequence =>
        val vectorDep = asStatic(vector)
        vector.items.foreach(it => {
          val itemDep = asStatic(it)
          info._1.updateAt(itemDep, Set(vectorDep))
          info._2.updateAt(vectorDep, Set(itemDep))
        })

        vector
          .copyWithItems(vector.items.map(analyseExpression(_, info)))
          .updateMetadata(toMetadata(info))
      case tSet: Application.Typeset =>
        val tSetDep = asStatic(tSet)
        tSet.expression.foreach(exp => {
          val exprDep = asStatic(exp)
          info._1.updateAt(exprDep, Set(tSetDep))
          info._2.updateAt(tSetDep, Set(exprDep))
        })

        tSet
          .copyWithExpression(tSet.expression.map(analyseExpression(_, info)))
          .updateMetadata(toMetadata(info))
      case _: Operator =>
        throw new CompilerError("Unexpected operator during Dataflow Analysis.")
    }
  }

  /** Performs dataflow analysis on a typing expression.
    *
    * Dataflow for typing expressions is a simple dependency on their parts.
    *
    * @param typ the type expression to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `typ`, with attached dependency information
    */
  private def analyseType(
    typ: Type,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Type = {
    typ match {
      case asc: Type.Ascription =>
        val typed     = asc.typed()
        val signature = asc.signature()
        val ascrDep   = asStatic(asc)
        val typedDep  = asStatic(typed)
        val sigDep    = asStatic(signature)
        info._1.updateAt(typedDep, Set(ascrDep))
        info._1.updateAt(sigDep, Set(ascrDep))
        info._2.updateAt(ascrDep, Set(typedDep, sigDep))

        asc
          .copyBuilder()
          .typed(analyseExpression(typed, info))
          .signature(analyseExpression(signature, info))
          .build()
          .updateMetadata(toMetadata(info))

      case fun: Type.Function =>
        val args    = fun.args()
        val result  = fun.result()
        val funDep  = asStatic(fun)
        val argDeps = args.map(asStatic)
        val resDep  = asStatic(result)
        argDeps.foreach(info._1.updateAt(_, Set(funDep)))
        info._1.updateAt(resDep, Set(funDep))
        info._2.updateAt(funDep, Set(resDep :: argDeps: _*))

        fun
          .copyBuilder()
          .args(args.map(analyseExpression(_, info)))
          .result(analyseExpression(result, info))
          .build()
          .updateMetadata(toMetadata(info))

      case ctx: Type.Context =>
        val typed      = ctx.typed()
        val context    = ctx.context()
        val ctxDep     = asStatic(ctx)
        val typedDep   = asStatic(typed)
        val contextDep = asStatic(context)
        info._1.updateAt(typedDep, Set(ctxDep))
        info._1.updateAt(contextDep, Set(ctxDep))
        info._2.updateAt(ctxDep, Set(typedDep, contextDep))

        ctx
          .copyBuilder()
          .typed(analyseExpression(typed, info))
          .context(analyseExpression(context, info))
          .build()
          .updateMetadata(toMetadata(info))

      case err: Type.Error =>
        val typed    = err.typed()
        val error    = err.error()
        val errDep   = asStatic(err)
        val typedDep = asStatic(typed)
        val errorDep = asStatic(error)
        info._1.updateAt(typedDep, Set(errDep))
        info._1.updateAt(errorDep, Set(errDep))
        info._2.updateAt(errDep, Set(typedDep, errorDep))

        err
          .copyBuilder()
          .typed(analyseExpression(typed, info))
          .error(analyseExpression(error, info))
          .build()
          .updateMetadata(toMetadata(info))

      case member: `type`.Set.Member =>
        val memberType    = member.memberType()
        val value         = member.value()
        val memberDep     = asStatic(member)
        val memberTypeDep = asStatic(memberType)
        val valueDep      = asStatic(value)
        info._1.updateAt(memberTypeDep, Set(memberDep))
        info._1.updateAt(valueDep, Set(memberDep))
        info._2.updateAt(memberDep, Set(memberTypeDep, valueDep))

        member
          .copyBuilder()
          .memberType(analyseExpression(memberType, info))
          .value(analyseExpression(value, info))
          .build()
          .updateMetadata(toMetadata(info))
      case intersect: `type`.Set.Intersection =>
        val intersectDep = asStatic(intersect)
        val leftDep      = asStatic(intersect.left)
        val rightDep     = asStatic(intersect.right)
        info._1.updateAt(leftDep, Set(intersectDep))
        info._1.updateAt(rightDep, Set(intersectDep))
        info._2.updateAt(intersectDep, Set(leftDep, rightDep))

        intersect
          .copyBuilder()
          .left(analyseExpression(intersect.left, info))
          .right(analyseExpression(intersect.right, info))
          .build()
          .updateMetadata(toMetadata(info))
      case union: `type`.Set.Union =>
        val operands = union.operands()
        val unionDep = asStatic(union)
        val opDeps   = operands.map(asStatic)
        opDeps.foreach(info._1.updateAt(_, Set(unionDep)))
        info._2.updateAt(unionDep, opDeps.toSet)
        union
          .copyWithOperands(operands.map(analyseExpression(_, info)))
          .updateMetadata(toMetadata(info))
    }
  }

  /** Performs dataflow analysis for a name usage.
    *
    * Name usages are dependent on the definition positions for those names.
    * These names can either be dynamic symbols (in which case all usages of
    * that symbol should be invalidated when the symbol changes), or static
    * symbols, which can be resolved into a direct dependency.
    *
    * @param name the name to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `name`, with attached dependency information
    */
  private def analyseName(
    name: Name,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Name = {
    val aliasInfo = name.passData
      .get(AliasAnalysis)
      .getOrElse(
        throw new CompilerError(
          "Name occurrence with missing aliasing information."
        )
      )
      .asInstanceOf[alias.AliasMetadata.Occurrence]

    name match {
      case _: Name.Blank =>
        throw new CompilerError(
          "Blanks should not be present during dataflow analysis."
        )
      case _ =>
        val defIdForName = aliasInfo.graph.defLinkFor(aliasInfo.id)
        val key: DependencyInfo.Type = defIdForName match {
          case Some(defLink) =>
            aliasInfo.graph.getOccurrence(defLink.target) match {
              case Some(GraphOccurrence.Def(_, _, id, ext, _)) =>
                new DependencyInfo.Type.Static(id, ext)
              case _ =>
                new DependencyInfo.Type.Dynamic(name.name, None)
            }

          case None =>
            new DependencyInfo.Type.Dynamic(name.name, None)
        }

        val nameDep = asStatic(name)
        info._1.updateAt(key, Set(nameDep))
        info._2.updateAt(nameDep, Set(key))

        name.updateMetadata(toMetadata(info))
    }
  }

  /** Performs dependency analysis on a case expression.
    *
    * The value of a case expression is dependent on both its scrutinee and the
    * definitions of its branches. The computation of the branches also depends
    * on the scrutinee.
    *
    * @param cse the case expression to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `cse`, with attached dependency information
    */
  private def analyseCase(
    cse: Case,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Case = {
    cse match {
      case expr: Case.Expr =>
        val exprDep  = asStatic(expr)
        val scrutDep = asStatic(expr.scrutinee)
        info._1.updateAt(scrutDep, Set(exprDep))
        info._2.updateAt(exprDep, Set(scrutDep))
        expr.branches.foreach(branch => {
          val branchDep = asStatic(branch)
          info._1.updateAt(branchDep, Set(exprDep))
          info._2.updateAt(exprDep, Set(branchDep))
        })

        expr
          .copy(
            analyseExpression(expr.scrutinee, info),
            expr.branches.map(analyseCaseBranch(_, info))
          )
          .updateMetadata(toMetadata(info))
      case _: Case.Branch =>
        throw new CompilerError("Unexpected case branch.")
    }
  }

  /** Performs dataflow analysis on a case branch.
    *
    * A case branch is dependent on both its pattern expression and the branch
    * expression.
    *
    * @param branch the case branch to perform dataflow analysis on.
    * @param info the dependency information for the module
    * @return `branch`, with attached dependency information
    */
  private def analyseCaseBranch(
    branch: Case.Branch,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Case.Branch = {
    val pattern    = branch.pattern
    val expression = branch.expression

    val branchDep  = asStatic(branch)
    val patternDep = asStatic(pattern)
    val exprDep    = asStatic(expression)
    info._1.updateAt(patternDep, Set(branchDep))
    info._1.updateAt(exprDep, Set(branchDep))
    info._2.updateAt(branchDep, Set(patternDep, exprDep))

    branch
      .copy(
        analysePattern(pattern, info),
        analyseExpression(expression, info),
        branch.terminalBranch()
      )
      .updateMetadata(toMetadata(info))
  }

  /** Performs dataflow analysis on a case branch.
    *
    * A case pattern is dependent on its subexpressions only.
    *
    * @param pattern the pattern to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `pattern`, with attached dependency information
    */
  private def analysePattern(
    pattern: Pattern,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): Pattern = {
    val patternDep = asStatic(pattern)
    pattern match {
      case named: Pattern.Name =>
        val nameDep = asStatic(named.name)
        info._1.updateAt(nameDep, Set(patternDep))
        info._2.updateAt(patternDep, Set(nameDep))

        named.updateMetadata(toMetadata(info))
      case cons: Pattern.Constructor =>
        val constructor = cons.constructor()
        val fields      = cons.fields()
        val consDep     = asStatic(constructor)
        info._1.updateAt(consDep, Set(patternDep))
        info._2.updateAt(patternDep, Set(consDep))
        fields.foreach(field => {
          val fieldDep = asStatic(field)
          info._1.updateAt(fieldDep, Set(patternDep))
          info._2.updateAt(patternDep, Set(fieldDep))
        })

        cons
          .copyBuilder()
          .constructor(analyseName(constructor, info))
          .fields(fields.map(analysePattern(_, info)))
          .build()
          .updateMetadata(toMetadata(info))
      case literal: Pattern.Literal =>
        literal.updateMetadata(toMetadata(info))
      case bool: Pattern.Bool =>
        bool.updateMetadata(toMetadata(info))
      case tp: Pattern.Type =>
        val nameDep = asStatic(tp.name)
        info._1.updateAt(nameDep, Set(patternDep))
        info._2.updateAt(patternDep, Set(nameDep))
        val tpeDep = asStatic(tp.tpe)
        info._1.updateAt(tpeDep, Set(patternDep))
        info._2.updateAt(patternDep, Set(tpeDep))

        pattern.updateMetadata(toMetadata(info))
      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
      case err: errors.Pattern =>
        err.updateMetadata(toMetadata(info))
    }
  }

  /** Performs dataflow analysis on a function definition argument.
    *
    * A function definition argument is dependent purely on its default, if said
    * default is present.
    *
    * @param argument the definition argument to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `argument`, with attached dependency information
    */
  private def analyseDefinitionArgument(
    argument: DefinitionArgument,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): DefinitionArgument = {
    argument match {
      case spec: DefinitionArgument.Specified =>
        val defValue = spec.defaultValue
        val specDep  = asStatic(spec)
        defValue.foreach(expr => {
          val exprDep = asStatic(expr)
          info._1.updateAt(exprDep, Set(specDep))
          info._2.updateAt(specDep, Set(exprDep))
        })

        spec
          .copyWithDefaultValue(
            defValue.map(analyseExpression(_, info))
          )
          .updateMetadata(toMetadata(info))
    }
  }

  /** Performs dataflow analysis on a function call argument.
    *
    * A function call argument is dependent both on the expression value that it
    * is wrapping, as well as the name of the argument, if it exists.
    *
    * @param argument the call argument to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `argument`, with attached dependency information
    */
  private def analyseCallArgument(
    argument: CallArgument,
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): CallArgument = {
    argument match {
      case spec: CallArgument.Specified =>
        val specDep  = asStatic(spec)
        val valueDep = asStatic(spec.value)
        info._1.updateAt(valueDep, Set(specDep))
        info._2.updateAt(specDep, Set(valueDep))
        spec.name.foreach(name => {
          val nameDep = asStatic(name)
          info._1.updateAt(nameDep, Set(specDep))
          info._2.updateAt(specDep, Set(nameDep))
        })

        spec
          .copy(
            analyseExpression(spec.value, info)
          )
          .updateMetadata(toMetadata(info))
    }
  }

  private def toMetadata(
    info: (DependencyMapping.Builder, DependencyMapping.Builder)
  ): MetadataPair[DataflowAnalysis.type] = {
    val dependents   = info._1.build
    val dependencies = info._2.build

    val res = new DependencyInfo(dependents, dependencies)
    new MetadataPair(this, res)
  }
}
