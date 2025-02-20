package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.expression.errors.Redefined
import org.enso.compiler.core.ir.expression.{
  errors,
  Application,
  Case,
  Comment,
  Error,
  Operator,
  Section
}
import org.enso.compiler.core.ir.module.scope.{definition, Definition}
import org.enso.compiler.core.ir.{
  `type`,
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Literal,
  Module,
  Name,
  Pattern,
  Type
}
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRProcessingPass
import org.enso.compiler.pass.analyse.alias.graph.GraphBuilder
import org.enso.compiler.pass.analyse.alias.graph.Graph.Scope
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.lint.UnusedBindings

import scala.collection.mutable

/** This pass performs scope identification and analysis, as well as symbol
  * resolution where it is possible to do so statically.
  *
  * It attaches the following information to the IR:
  *
  * - Top-level constructs are annotated with an aliasing graph.
  * - Scopes within each top-level construct are annotated with the
  *   corresponding child scope.
  * - Occurrences of symbols are annotated with occurrence information that
  *   points into the graph.
  *
  * The analysis process explicitly compensates for some deficiencies in our
  * underlying IR representation by collapsing certain sets of scopes into each
  * other. The collapsing takes place under the following circumstances:
  *
  * - A lambda whose body is a block does not allocate an additional scope for
  *   the block.
  * - A method whose body is a block does not allocate an additional scope for
  *   the block.
  * - A method whose body is a lambda does not allocate an additional scope for
  *   the lambda.
  * - A method whose body is a lambda containing a block as its body allocates
  *   no additional scope for the lambda or the block.
  *
  * Alias analysis requires its configuration to be in the configuration object.
  *
  * This pass requires the context to provide:
  *
  * - A [[org.enso.compiler.pass.PassConfiguration]] containing an instance of
  *   [[AliasAnalysis.Configuration]].
  * - A [[org.enso.compiler.context.LocalScope]], where relevant.
  */
case object AliasAnalysis extends IRPass {

  /** Alias information for the IR. */
  override type Metadata = alias.AliasMetadata
  override type Config   = Configuration

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    FunctionBinding,
    GenerateMethodBodies,
    SectionsToBinOp.INSTANCE,
    OperatorToFunction,
    LambdaShorthandToLambda
  )

  override lazy val invalidatedPasses: Seq[IRProcessingPass] =
    List(DataflowAnalysis, UnusedBindings)

  /** Performs alias analysis on a module.
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
    ir.copy(bindings = ir.bindings.map(analyseModuleDefinition))
  }

  /** Performs alias analysis on an inline expression, starting from the
    * provided scope.
    *
    * @param ir            the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    val shouldWriteState =
      inlineContext.passConfiguration
        .flatMap(config => config.get(this))
        .getOrElse(
          throw new CompilerError(
            "Alias analysis execution missing configuration."
          )
        )
        .shouldWriteToContext

    inlineContext.localScope
      .map { localScope =>
        val scope =
          if (shouldWriteState) localScope.scope
          else
            localScope.scope
              .deepCopy(mutable.Map())
              .withParent(localScope.scope)

        val ag = localScope.aliasingGraph()
        val graph =
          if (shouldWriteState) ag
          else {
            val mapping = mutable.Map(localScope.scope -> scope)
            ag.deepCopy(mapping)
          }
        val result = analyseExpression(ir, GraphBuilder.create(graph, scope))

        result
      }
      .getOrElse(
        throw new CompilerError(
          "Local scope must be provided for alias analysis."
        )
      )
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    sourceIr: T,
    copyOfIr: T
  ): T = {
    def doCopy(sourceBinding: IR, copyBinding: IR): Unit = {
      val sourceRootScopeGraphOpt = sourceBinding
        .getMetadata(this)

      sourceRootScopeGraphOpt.foreach { sourceRootScopeGraphScope =>
        val sourceRootScopeGraph =
          sourceRootScopeGraphScope
            .asInstanceOf[alias.AliasMetadata.RootScope]
            .graph

        val scopeMapping = mutable.Map[Scope, Scope]()
        val copyRootScopeGraph =
          sourceRootScopeGraph.deepCopy(scopeMapping)

        val sourceNodes = sourceBinding.preorder
        val copyNodes   = copyBinding.preorder

        val matchedNodes = sourceNodes.lazyZip(copyNodes)

        matchedNodes.foreach { case (sourceNode, copyNode) =>
          sourceNode.getMetadata(this) match {
            case Some(meta) =>
              val newMeta = meta match {
                case root: alias.AliasMetadata.RootScope =>
                  root.copy(graph = copyRootScopeGraph)
                case child: alias.AliasMetadata.ChildScope =>
                  child.copy(
                    graph = copyRootScopeGraph,
                    scope = child.scope.deepCopy(scopeMapping)
                  )
                case occ: alias.AliasMetadata.Occurrence =>
                  occ.copy(graph = copyRootScopeGraph)
              }
              copyNode.updateMetadata(new MetadataPair(this, newMeta))
            case None =>
          }
        }
      }
    }

    (sourceIr, copyOfIr) match {
      case (sourceIr: Module, copyOfIr: Module) =>
        val sourceBindings = sourceIr.bindings
        val copyBindings   = copyOfIr.bindings
        val zippedBindings = sourceBindings.lazyZip(copyBindings)

        zippedBindings.foreach {
          case (
                source: Definition.Type,
                copy: Definition.Type
              ) =>
            doCopy(source, copy)
            source.members.lazyZip(copy.members).foreach {
              case (source, copy) =>
                doCopy(source, copy)
            }
          case (sourceBinding, copyBinding) =>
            doCopy(sourceBinding, copyBinding)
        }
        copyOfIr.asInstanceOf[T]
      case _ => copyOfIr
    }
  }

  /** Performs alias analysis on the module-level definitions.
    *
    * Each module level definition is assigned its own aliasing graph, as under
    * the current dynamic semantics of the language we cannot statically resolve
    * aliasing between top-level constructs within or between modules.
    *
    * @param ir the module-level definition to perform alias analysis on
    * @return `ir`, with the results of alias analysis attached
    */
  def analyseModuleDefinition(
    ir: Definition
  ): Definition = {
    val builder = GraphBuilder.create()

    ir match {
      case m: definition.Method.Conversion =>
        m.body match {
          case _: Function =>
            m.copy(
              body = analyseExpression(
                m.body,
                builder,
                lambdaReuseScope = true
              )
            ).updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.RootScope(builder.toGraph())
              )
            )
          case _ =>
            throw new CompilerError(
              "The body of a method should always be a function."
            )
        }
      case m: definition.Method.Explicit =>
        m.body match {
          case _: Function =>
            m.copy(
              body = analyseExpression(
                m.body,
                builder,
                lambdaReuseScope = true
              )
            ).updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.RootScope(builder.toGraph())
              )
            )
          case _ =>
            throw new CompilerError(
              "The body of a method should always be a function."
            )
        }
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Method definition sugar should not occur during alias analysis."
        )
      case t: Definition.Type =>
        t.copy(
          params = analyseArgumentDefs(
            t.params,
            builder
          ),
          members = t.members.map(d => {
            val graph = GraphBuilder.create()
            d.copy(
              arguments = analyseArgumentDefs(
                d.arguments,
                graph
              ),
              annotations = d.annotations.map { ann =>
                ann
                  .copy(
                    expression = analyseExpression(
                      ann.expression,
                      builder
                    )
                  )
                  .updateMetadata(
                    new MetadataPair(
                      this,
                      alias.AliasMetadata.RootScope(builder.toGraph())
                    )
                  )
              }
            ).updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.RootScope(graph.toGraph())
              )
            )
          })
        ).updateMetadata(
          new MetadataPair(
            this,
            alias.AliasMetadata.RootScope(builder.toGraph())
          )
        )
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present during " +
          "alias analysis."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation should not exist as an entity during alias analysis."
        )
      case _: Type.Ascription =>
        throw new CompilerError(
          "Type signatures should not exist at the top level during " +
          "alias analysis."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of alias " +
          "analysis."
        )
      case ann: Name.GenericAnnotation =>
        ann
          .copy(expression =
            analyseExpression(
              ann.expression,
              builder
            )
          )
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.RootScope(builder.toGraph())
            )
          )
      case err: Error => err
    }
  }

  /** Performs alias analysis on an expression.
    *
    * An expression is assumed to be a child of an aliasing `graph`, and the
    * analysis takes place in the context of said graph.
    *
    * It should be noted that not _all_ expressions are annotated with aliasing
    * information. Please see the pass header documentation for more details.
    *
    * @param expression       the expression to perform alias analysis on
    * @param builder          the scope builder
    * @param lambdaReuseScope whether to reuse the parent scope for a lambda
    *                         instead of creating a new scope
    * @return `expression`, potentially with aliasing information attached
    */
  private def analyseExpression(
    expression: Expression,
    builder: GraphBuilder,
    lambdaReuseScope: Boolean = false
  ): Expression = {
    expression match {
      case fn: Function =>
        analyseFunction(fn, builder, lambdaReuseScope)
      case name: Name =>
        analyseName(
          name,
          isInPatternContext                = false,
          isConstructorNameInPatternContext = false,
          builder
        )
      case cse: Case => analyseCase(cse, builder)
      case block: Expression.Block =>
        val currentScope =
          if (!block.suspended) builder else builder.addChild()

        block
          .copy(
            expressions = block.expressions.map((expression: Expression) =>
              analyseExpression(
                expression,
                currentScope
              )
            ),
            returnValue = analyseExpression(
              block.returnValue,
              currentScope
            )
          )
          .updateMetadata(
            new MetadataPair(
              this,
              new alias.AliasMetadata.ChildScope(currentScope)
            )
          )
      case binding @ Expression.Binding(name, expression, _, _) =>
        if (builder.findDef(name.name) == -1) {
          val isSuspended = expression match {
            case Expression.Block(_, _, _, isSuspended, _) => isSuspended
            case _                                         => false
          }
          val occurrence = builder.newDef(
            name.name,
            binding.getId(),
            binding.getExternalId,
            isSuspended
          )

          builder.add(occurrence)
          builder.addDefinition(occurrence)

          binding
            .copy(
              expression = analyseExpression(
                expression,
                builder
              )
            )
            .updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.Occurrence(builder.toGraph(), occurrence.id)
              )
            )
        } else {
          errors.Redefined.Binding(binding)
        }
      case app: Application =>
        analyseApplication(app, builder)
      case tpe: Type => analyseType(tpe, builder)
      case x =>
        x.mapExpressions((expression: Expression) =>
          analyseExpression(
            expression,
            builder
          )
        )
    }
  }

  /** Performs alias analysis on a type-related expression.
    *
    * @param value       the ir to analyse
    * @param builder       the graph builder
    * @return `value`, annotated with aliasing information
    */
  def analyseType(
    value: Type,
    builder: GraphBuilder
  ): Type = {
    value match {
      case member @ `type`.Set.Member(label, memberType, value, _, _) =>
        val memberTypeScope = memberType match {
          case _: Literal => builder
          case _          => builder.addChild()
        }

        val valueScope = value match {
          case _: Literal => builder
          case _          => builder.addChild()
        }

        val definition = builder.newDef(
          label.name,
          label.getId,
          label.getExternalId
        )
        builder.add(definition)
        builder.addDefinition(definition)

        member
          .copy(
            memberType = analyseExpression(memberType, memberTypeScope),
            value      = analyseExpression(value, valueScope)
          )
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.Occurrence(builder.toGraph(), definition.id)
            )
          )
      case x =>
        x.mapExpressions(analyseExpression(_, builder))
    }
  }

  private def isSyntheticSelf(name: Name): Boolean = {
    name match {
      case Name.Self(_, true, _) => true
      case _                     => false
    }
  }

  /** Performs alias analysis on the argument definitions for a function.
    *
    * Care is taken during this analysis to ensure that spurious resolutions do
    * not happen regarding the ordering of arguments. Only the arguments
    * declared _earlier_ in the arguments list are considered to be in scope for
    * later arguments. This also means that the default for an argument must be
    * resolved _before_ the argument name is defined, lest recursive definitions
    * occur (e.g. `x = x + 1`, the `x` on the RHS must refer to an `x` outside
    * the argument's scope).
    *
    * @param args  the list of arguments to perform analysis on
    * @param builder the graph builder
    * @return `args`, potentially
    */
  private def analyseArgumentDefs(
    args: List[DefinitionArgument],
    builder: GraphBuilder
  ): List[DefinitionArgument] = {
    args.map {
      case arg: DefinitionArgument.Specified if isSyntheticSelf(arg.name) =>
        // Synthetic `self` must not be added to the scope, but it has to be added as a
        // definition for frame index metadata
        val definition = builder.newDef(
          arg.name.name,
          arg.getId(),
          arg.getExternalId
        )
        builder.addDefinition(definition)
        arg
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.Occurrence(builder.toGraph(), definition.id)
            )
          )
          .copyWithAscribedType(
            arg.ascribedType.map(analyseExpression(_, builder))
          )

      case arg: DefinitionArgument.Specified =>
        val name      = arg.name
        val value     = arg.defaultValue
        val suspended = arg.suspended
        val nameOccursInScope =
          builder.findDef(
            name.name
          )
        if (nameOccursInScope == -1) {
          val argScope = if (suspended) builder.addChild() else builder
          val newDefault =
            value.map((ir: Expression) => analyseExpression(ir, argScope))

          val definition = builder.newDef(
            name.name,
            arg.getId(),
            arg.getExternalId,
            suspended
          )
          builder.add(definition)
          builder.addDefinition(definition)

          arg
            .copy(
              newDefault,
              arg.ascribedType.map(analyseExpression(_, builder))
            )
            .updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.Occurrence(builder.toGraph(), definition.id)
              )
            )
        } else {
          arg
            .copyWithAscribedType(
              Some(Redefined.Arg(name, arg.identifiedLocation))
            )
            .updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata
                  .Occurrence(builder.toGraph(), nameOccursInScope)
              )
            )
        }
    }
  }

  /** Performs alias analysis on a function application.
    *
    * @param application the function application to analyse
    * @param graph       the graph in which the analysis is taking place
    * @param scope       the scope in which the application is happening
    * @return `application`, possibly with aliasing information attached
    */
  def analyseApplication(
    application: Application,
    builder: GraphBuilder
  ): Application = {
    application match {
      case app: Application.Prefix =>
        app.copy(
          function  = analyseExpression(app.function, builder),
          arguments = analyseCallArguments(app.arguments, builder)
        )
      case app: Application.Force =>
        app.copyWithTarget(analyseExpression(app.target, builder))
      case app: Application.Sequence =>
        app.copyWithItems(app.items.map(analyseExpression(_, builder)))
      case tSet: Application.Typeset =>
        val newScope = builder.addChild()
        tSet
          .copyWithExpression(
            tSet.expression.map(analyseExpression(_, newScope))
          )
          .updateMetadata(
            new MetadataPair(
              this,
              new alias.AliasMetadata.ChildScope(newScope)
            )
          )
      case _: Operator.Binary =>
        throw new CompilerError(
          "Binary operator occurred during Alias Analysis."
        )
      case _: Section =>
        throw new CompilerError(
          "Operator section occurred during Alias Analysis."
        )
    }
  }

  /** Performs alias analysis on function call arguments.
    *
    * @param args        the list of arguments to analyse
    * @param builder     the graph builder
    * @return `args`, with aliasing information attached to each argument
    */
  private def analyseCallArguments(
    args: List[CallArgument],
    builder: GraphBuilder
  ): List[CallArgument] = {
    args.map { case arg: CallArgument.Specified =>
      val currentScope = arg.value match {
        case _: Literal => builder
        case _          => builder.addChild()
      }
      arg
        .copy(analyseExpression(arg.value, currentScope))
        .updateMetadata(
          new MetadataPair(
            this,
            new alias.AliasMetadata.ChildScope(currentScope)
          )
        )
    }
  }

  /** Performs alias analysis on a function definition.
    *
    * @param function         the function to analyse
    * @param builder            the graph builder
    * @param lambdaReuseScope whether or not the lambda should reuse the parent
    *                         scope or allocate a child of it
    * @return `function`, with alias analysis information attached
    */
  def analyseFunction(
    function: Function,
    builder: GraphBuilder,
    lambdaReuseScope: Boolean = false
  ): Function = {
    val currentScope =
      if (lambdaReuseScope) builder else builder.addChild()

    function match {
      case lambda: Function.Lambda =>
        lambda
          .copy(
            arguments = analyseArgumentDefs(lambda.arguments, currentScope),
            body = analyseExpression(
              lambda.body,
              currentScope
            )
          )
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata
                .ChildScope(currentScope.toGraph(), currentScope.toScope())
            )
          )
      case _: Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during alias analysis."
        )
    }
  }

  /** Performs alias analysis for a name.
    *
    * @param name the name to analyse
    * @param isInPatternContext whether or not the name is occurring in a
    *                           pattern context
    * @param isConstructorNameInPatternContext whether or not the name is
    *                           constructor name occurring in a pattern context
    * @param builder the graph builder
    * @return `name`, with alias analysis information attached
    */
  def analyseName(
    name: Name,
    isInPatternContext: Boolean,
    isConstructorNameInPatternContext: Boolean,
    builder: GraphBuilder
  ): Name = {
    val occurrenceId =
      if (isInPatternContext && !isConstructorNameInPatternContext) {
        val definition = builder.newDef(
          name.name,
          name.getId,
          name.getExternalId
        )
        builder.add(definition)
        builder.addDefinition(definition)
        definition.id
      } else {
        val occurrence =
          builder.newUse(
            name.name,
            name.getId,
            name.getExternalId
          )
        builder.add(occurrence)
        if (!isConstructorNameInPatternContext && !name.isMethod) {
          builder.resolveLocalUsage(occurrence)
        }
        occurrence.id
      }
    name.updateMetadata(
      new MetadataPair(
        this,
        alias.AliasMetadata.Occurrence(builder.toGraph(), occurrenceId)
      )
    )
  }

  /** Performs alias analysis on a case expression.
    *
    * @param ir          the case expression to analyse
    * @param graph       the graph in which the analysis is taking place
    * @param parentScope the scope in which the case expression occurs
    * @return `ir`, possibly with alias analysis information attached
    */
  def analyseCase(
    ir: Case,
    builder: GraphBuilder
  ): Case = {
    ir match {
      case caseExpr: Case.Expr =>
        caseExpr
          .copy(
            scrutinee = analyseExpression(caseExpr.scrutinee, builder),
            branches  = caseExpr.branches.map(analyseCaseBranch(_, builder))
          )
      case _: Case.Branch =>
        throw new CompilerError("Case branch in `analyseCase`.")
    }
  }

  /** Performs alias analysis on a case branch.
    *
    * @param branch the case branch to analyse
    * @param builder the graph builder
    * @return `branch`, possibly with alias analysis information attached
    */
  def analyseCaseBranch(
    branch: Case.Branch,
    builder: GraphBuilder
  ): Case.Branch = {
    val currentScope = builder.addChild()

    branch
      .copy(
        pattern = analysePattern(branch.pattern, currentScope),
        expression = analyseExpression(
          branch.expression,
          currentScope
        )
      )
      .updateMetadata(
        new MetadataPair(
          this,
          new alias.AliasMetadata.ChildScope(currentScope)
        )
      )
  }

  /** Performs alias analysis on a pattern.
    *
    * @param pattern the pattern to analyse
    * @param builder the graph builder
    * @return `pattern`, possibly with alias analysis information attached
    */
  def analysePattern(
    pattern: Pattern,
    builder: GraphBuilder
  ): Pattern = {
    pattern match {
      case named: Pattern.Name =>
        named.copy(
          name = analyseName(
            named.name,
            isInPatternContext                = true,
            isConstructorNameInPatternContext = false,
            builder
          )
        )
      case cons: Pattern.Constructor =>
        if (!cons.isDesugared) {
          throw new CompilerError(
            "Nested patterns should be desugared by the point of alias " +
            "analysis."
          )
        }

        cons.copy(
          constructor = analyseName(
            cons.constructor,
            isInPatternContext                = true,
            isConstructorNameInPatternContext = true,
            builder
          ),
          fields = cons.fields.map(analysePattern(_, builder))
        )
      case literalPattern: Pattern.Literal =>
        literalPattern
      case typePattern: Pattern.Type =>
        typePattern.copy(
          name = analyseName(
            typePattern.name,
            isInPatternContext                = true,
            isConstructorNameInPatternContext = false,
            builder
          ),
          tpe = analyseName(
            typePattern.tpe,
            isInPatternContext                = false,
            isConstructorNameInPatternContext = false,
            builder
          )
        )
      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
      case err: errors.Pattern => err
    }
  }

  // === Data Definitions =====================================================

  // === Pass Configuration ===================================================

  /** Configuration for the alias analysis pass.
    *
    * @param shouldWriteToContext whether the pass should write its results to
    *                             the context or not
    */
  sealed case class Configuration(
    override var shouldWriteToContext: Boolean = false
  ) extends IRPass.Configuration

}
