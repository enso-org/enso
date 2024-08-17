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
import org.enso.compiler.pass.analyse.alias.graph.Graph
import org.enso.compiler.pass.analyse.alias.graph.GraphOccurrence
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

  override lazy val precursorPasses: Seq[IRPass] = List(
    FunctionBinding,
    GenerateMethodBodies,
    SectionsToBinOp,
    OperatorToFunction,
    LambdaShorthandToLambda
  )

  override lazy val invalidatedPasses: Seq[IRPass] =
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
        val result = analyseExpression(ir, graph, scope)

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
    val topLevelGraph = new Graph

    ir match {
      case m: definition.Method.Conversion =>
        m.body match {
          case _: Function =>
            m.copy(
              body = analyseExpression(
                m.body,
                topLevelGraph,
                topLevelGraph.rootScope,
                lambdaReuseScope = true
              )
            ).updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.RootScope(topLevelGraph)
              )
            )
          case _ =>
            throw new CompilerError(
              "The body of a method should always be a function."
            )
        }
      case m @ definition.Method.Explicit(_, body, _, _, _) =>
        body match {
          case _: Function =>
            m.copy(
              body = analyseExpression(
                body,
                topLevelGraph,
                topLevelGraph.rootScope,
                lambdaReuseScope = true
              )
            ).updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.RootScope(topLevelGraph)
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
            topLevelGraph,
            topLevelGraph.rootScope
          ),
          members = t.members.map(d => {
            val graph = new Graph
            d.copy(
              arguments = analyseArgumentDefs(
                d.arguments,
                graph,
                graph.rootScope
              ),
              annotations = d.annotations.map { ann =>
                ann
                  .copy(
                    expression = analyseExpression(
                      ann.expression,
                      topLevelGraph,
                      topLevelGraph.rootScope
                    )
                  )
                  .updateMetadata(
                    new MetadataPair(
                      this,
                      alias.AliasMetadata.RootScope(topLevelGraph)
                    )
                  )
              }
            ).updateMetadata(
              new MetadataPair(this, alias.AliasMetadata.RootScope(graph))
            )
          })
        ).updateMetadata(
          new MetadataPair(this, alias.AliasMetadata.RootScope(topLevelGraph))
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
              topLevelGraph,
              topLevelGraph.rootScope
            )
          )
          .updateMetadata(
            new MetadataPair(this, alias.AliasMetadata.RootScope(topLevelGraph))
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
    * @param graph            the aliasing graph in which the analysis is being performed
    * @param parentScope      the parent scope for this expression
    * @param lambdaReuseScope whether to reuse the parent scope for a lambda
    *                         instead of creating a new scope
    * @return `expression`, potentially with aliasing information attached
    */
  private def analyseExpression(
    expression: Expression,
    graph: Graph,
    parentScope: Scope,
    lambdaReuseScope: Boolean = false
  ): Expression = {
    expression match {
      case fn: Function =>
        analyseFunction(fn, graph, parentScope, lambdaReuseScope)
      case name: Name =>
        analyseName(
          name,
          isInPatternContext                = false,
          isConstructorNameInPatternContext = false,
          graph,
          parentScope
        )
      case cse: Case => analyseCase(cse, graph, parentScope)
      case block @ Expression.Block(
            expressions,
            retVal,
            _,
            isSuspended,
            _,
            _
          ) =>
        val currentScope =
          if (!isSuspended) parentScope else parentScope.addChild()

        block
          .copy(
            expressions = expressions.map((expression: Expression) =>
              analyseExpression(
                expression,
                graph,
                currentScope
              )
            ),
            returnValue = analyseExpression(
              retVal,
              graph,
              currentScope
            )
          )
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.ChildScope(graph, currentScope)
            )
          )
      case binding @ Expression.Binding(name, expression, _, _, _) =>
        if (
          !parentScope.hasSymbolOccurrenceAs[GraphOccurrence.Def](name.name)
        ) {
          val isSuspended = expression match {
            case Expression.Block(_, _, _, isSuspended, _, _) => isSuspended
            case _                                            => false
          }
          val occurrenceId = graph.nextId()
          val occurrence =
            GraphOccurrence.Def(
              occurrenceId,
              name.name,
              binding.getId(),
              binding.getExternalId,
              isSuspended
            )

          parentScope.add(occurrence)
          parentScope.addDefinition(occurrence)

          binding
            .copy(
              expression = analyseExpression(
                expression,
                graph,
                parentScope
              )
            )
            .updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.Occurrence(graph, occurrenceId)
              )
            )
        } else {
          errors.Redefined.Binding(binding)
        }
      case app: Application =>
        analyseApplication(app, graph, parentScope)
      case tpe: Type => analyseType(tpe, graph, parentScope)
      case x =>
        x.mapExpressions((expression: Expression) =>
          analyseExpression(
            expression,
            graph,
            parentScope
          )
        )
    }
  }

  /** Performs alias analysis on a type-related expression.
    *
    * @param value       the ir to analyse
    * @param graph       the graph in which the analysis is taking place
    * @param parentScope the parent scope in which `value` occurs
    * @return `value`, annotated with aliasing information
    */
  def analyseType(
    value: Type,
    graph: Graph,
    parentScope: Scope
  ): Type = {
    value match {
      case member @ `type`.Set.Member(label, memberType, value, _, _, _) =>
        val memberTypeScope = memberType match {
          case _: Literal => parentScope
          case _          => parentScope.addChild()
        }

        val valueScope = value match {
          case _: Literal => parentScope
          case _          => parentScope.addChild()
        }

        val labelId = graph.nextId()
        val definition =
          GraphOccurrence.Def(
            labelId,
            label.name,
            label.getId,
            label.getExternalId
          )
        parentScope.add(definition)
        parentScope.addDefinition(definition)

        member
          .copy(
            memberType = analyseExpression(memberType, graph, memberTypeScope),
            value      = analyseExpression(value, graph, valueScope)
          )
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.Occurrence(graph, labelId)
            )
          )
      case x =>
        x.mapExpressions(analyseExpression(_, graph, parentScope))
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
    * @param graph the graph in which the analysis is taking place
    * @param scope the scope of the function for which `args` are being
    *              defined
    * @return `args`, potentially
    */
  private def analyseArgumentDefs(
    args: List[DefinitionArgument],
    graph: Graph,
    scope: Scope
  ): List[DefinitionArgument] = {
    args.map {
      case arg @ DefinitionArgument.Specified(
            selfName @ Name.Self(_, true, _, _),
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        // Synthetic `self` must not be added to the scope, but it has to be added as a
        // definition for frame index metadata
        val occurrenceId = graph.nextId()
        val definition = GraphOccurrence.Def(
          occurrenceId,
          selfName.name,
          arg.getId(),
          arg.getExternalId
        )
        scope.addDefinition(definition)
        arg
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.Occurrence(graph, occurrenceId)
            )
          )
          .copy(
            ascribedType =
              arg.ascribedType.map(analyseExpression(_, graph, scope))
          )

      case arg @ DefinitionArgument.Specified(
            name,
            _,
            value,
            suspended,
            _,
            _,
            _
          ) =>
        val nameOccursInScope =
          scope.hasSymbolOccurrenceAs[GraphOccurrence.Def](
            name.name
          )
        if (!nameOccursInScope) {
          val argScope = if (suspended) scope.addChild() else scope
          val newDefault =
            value.map((ir: Expression) =>
              analyseExpression(ir, graph, argScope)
            )

          val occurrenceId = graph.nextId()
          val definition = GraphOccurrence.Def(
            occurrenceId,
            name.name,
            arg.getId(),
            arg.getExternalId,
            suspended
          )
          scope.add(definition)
          scope.addDefinition(definition)

          arg
            .copy(
              defaultValue = newDefault,
              ascribedType =
                arg.ascribedType.map(analyseExpression(_, graph, scope))
            )
            .updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.Occurrence(graph, occurrenceId)
              )
            )
        } else {
          val f = scope.occurrences.values.collectFirst {
            case x if x.symbol == name.name => x
          }
          arg
            .copy(
              ascribedType = Some(Redefined.Arg(name, arg.location))
            )
            .updateMetadata(
              new MetadataPair(
                this,
                alias.AliasMetadata.Occurrence(graph, f.get.id)
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
    graph: Graph,
    scope: Graph.Scope
  ): Application = {
    application match {
      case app @ Application.Prefix(fun, arguments, _, _, _, _) =>
        app.copy(
          function  = analyseExpression(fun, graph, scope),
          arguments = analyseCallArguments(arguments, graph, scope)
        )
      case app @ Application.Force(expr, _, _, _) =>
        app.copy(target = analyseExpression(expr, graph, scope))
      case app @ Application.Sequence(items, _, _, _) =>
        app.copy(items = items.map(analyseExpression(_, graph, scope)))
      case tSet @ Application.Typeset(expr, _, _, _) =>
        val newScope = scope.addChild()
        tSet
          .copy(expression = expr.map(analyseExpression(_, graph, newScope)))
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.ChildScope(graph, newScope)
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
    * @param graph       the graph in which the analysis is taking place
    * @param parentScope the scope in which the arguments are defined
    * @return `args`, with aliasing information attached to each argument
    */
  private def analyseCallArguments(
    args: List[CallArgument],
    graph: Graph,
    parentScope: Graph.Scope
  ): List[CallArgument] = {
    args.map { case arg @ CallArgument.Specified(_, expr, _, _, _) =>
      val currentScope = expr match {
        case _: Literal => parentScope
        case _          => parentScope.addChild()
      }
      arg
        .copy(value = analyseExpression(expr, graph, currentScope))
        .updateMetadata(
          new MetadataPair(
            this,
            alias.AliasMetadata.ChildScope(graph, currentScope)
          )
        )
    }
  }

  /** Performs alias analysis on a function definition.
    *
    * @param function         the function to analyse
    * @param graph            the graph in which the analysis is taking place
    * @param parentScope      the scope in which the function is declared
    * @param lambdaReuseScope whether or not the lambda should reuse the parent
    *                         scope or allocate a child of it
    * @return `function`, with alias analysis information attached
    */
  def analyseFunction(
    function: Function,
    graph: Graph,
    parentScope: Scope,
    lambdaReuseScope: Boolean = false
  ): Function = {
    val currentScope =
      if (lambdaReuseScope) parentScope else parentScope.addChild()

    function match {
      case lambda @ Function.Lambda(arguments, body, _, _, _, _) =>
        lambda
          .copy(
            arguments = analyseArgumentDefs(arguments, graph, currentScope),
            body = analyseExpression(
              body,
              graph,
              currentScope
            )
          )
          .updateMetadata(
            new MetadataPair(
              this,
              alias.AliasMetadata.ChildScope(graph, currentScope)
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
    * @param graph the graph in which the analysis is taking place
    * @param parentScope the scope in which `name` is declared
    * @return `name`, with alias analysis information attached
    */
  def analyseName(
    name: Name,
    isInPatternContext: Boolean,
    isConstructorNameInPatternContext: Boolean,
    graph: Graph,
    parentScope: Scope
  ): Name = {
    val occurrenceId = graph.nextId()

    if (isInPatternContext && !isConstructorNameInPatternContext) {
      val definition =
        GraphOccurrence.Def(
          occurrenceId,
          name.name,
          name.getId,
          name.getExternalId
        )
      parentScope.add(definition)
      parentScope.addDefinition(definition)
    } else {
      val occurrence =
        GraphOccurrence.Use(
          occurrenceId,
          name.name,
          name.getId,
          name.getExternalId
        )
      parentScope.add(occurrence)
      if (!isConstructorNameInPatternContext && !name.isMethod) {
        graph.resolveLocalUsage(occurrence)
      } else {
        graph.resolveGlobalUsage(occurrence)
      }
    }
    name.updateMetadata(
      new MetadataPair(
        this,
        alias.AliasMetadata.Occurrence(graph, occurrenceId)
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
    graph: Graph,
    parentScope: Scope
  ): Case = {
    ir match {
      case caseExpr @ Case.Expr(scrutinee, branches, _, _, _, _) =>
        caseExpr
          .copy(
            scrutinee = analyseExpression(scrutinee, graph, parentScope),
            branches  = branches.map(analyseCaseBranch(_, graph, parentScope))
          )
      case _: Case.Branch =>
        throw new CompilerError("Case branch in `analyseCase`.")
    }
  }

  /** Performs alias analysis on a case branch.
    *
    * @param branch the case branch to analyse
    * @param graph the graph in which the analysis is taking place
    * @param parentScope the scope in which the case branch occurs
    * @return `branch`, possibly with alias analysis information attached
    */
  def analyseCaseBranch(
    branch: Case.Branch,
    graph: Graph,
    parentScope: Scope
  ): Case.Branch = {
    val currentScope = parentScope.addChild()

    branch
      .copy(
        pattern = analysePattern(branch.pattern, graph, currentScope),
        expression = analyseExpression(
          branch.expression,
          graph,
          currentScope
        )
      )
      .updateMetadata(
        new MetadataPair(
          this,
          alias.AliasMetadata.ChildScope(graph, currentScope)
        )
      )
  }

  /** Performs alias analysis on a pattern.
    *
    * @param pattern the pattern to analyse
    * @param graph the graph in which the analysis is taking place
    * @param parentScope the scope in which the case branch occurs
    * @return `pattern`, possibly with alias analysis information attached
    */
  def analysePattern(
    pattern: Pattern,
    graph: Graph,
    parentScope: Scope
  ): Pattern = {
    pattern match {
      case named @ Pattern.Name(name, _, _, _) =>
        named.copy(
          name = analyseName(
            name,
            isInPatternContext                = true,
            isConstructorNameInPatternContext = false,
            graph,
            parentScope
          )
        )
      case cons @ Pattern.Constructor(constructor, fields, _, _, _) =>
        if (!cons.isDesugared) {
          throw new CompilerError(
            "Nested patterns should be desugared by the point of alias " +
            "analysis."
          )
        }

        cons.copy(
          constructor = analyseName(
            constructor,
            isInPatternContext                = true,
            isConstructorNameInPatternContext = true,
            graph,
            parentScope
          ),
          fields = fields.map(analysePattern(_, graph, parentScope))
        )
      case literalPattern: Pattern.Literal =>
        literalPattern
      case typePattern @ Pattern.Type(name, tpe, _, _, _) =>
        typePattern.copy(
          name = analyseName(
            name,
            isInPatternContext                = true,
            isConstructorNameInPatternContext = false,
            graph,
            parentScope
          ),
          tpe = analyseName(
            tpe,
            isInPatternContext                = false,
            isConstructorNameInPatternContext = false,
            graph,
            parentScope
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
