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
import org.enso.compiler.pass.analyse.alias.Graph.{Occurrence, Scope}
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
  override type Metadata = alias.Info
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
    val bindings1 = ir.bindings.map(analyseModuleDefinition)
    if (bindings1 != ir.bindings)
      ir.copy(bindings = bindings1)
    else
      ir
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

        val graph =
          if (shouldWriteState) localScope.aliasingGraph
          else {
            val mapping = mutable.Map(localScope.scope -> scope)
            localScope.aliasingGraph.deepCopy(mapping)
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
            .asInstanceOf[alias.Info.Scope.Root]
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
                case root: alias.Info.Scope.Root =>
                  root.copy(graph = copyRootScopeGraph)
                case child: alias.Info.Scope.Child =>
                  child.copy(
                    graph = copyRootScopeGraph,
                    scope = child.scope.deepCopy(scopeMapping)
                  )
                case occ: alias.Info.Occurrence =>
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
    val topLevelGraph = new alias.Graph

    ir match {
      case m: definition.Method.Conversion =>
        m.body match {
          case _: Function =>
            val body1 = analyseExpression(
              m.body,
              topLevelGraph,
              topLevelGraph.rootScope,
              lambdaReuseScope = true
            )
            (if (body1 != m.body) m.copy(body = body1) else m).updateMetadata(
              new MetadataPair(
                this,
                alias.Info.Scope.Root(topLevelGraph)
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
            val body1 = analyseExpression(
              body,
              topLevelGraph,
              topLevelGraph.rootScope,
              lambdaReuseScope = true
            )
            (if (body1 != m.body) m.copy(body = body1) else m).updateMetadata(
              new MetadataPair(
                this,
                alias.Info.Scope.Root(topLevelGraph)
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
        val params1 = analyseArgumentDefs(
          t.params,
          topLevelGraph,
          topLevelGraph.rootScope
        )
        val members1 = t.members.map(d => {
          val graph = new alias.Graph
          val arguments1 = analyseArgumentDefs(
            d.arguments,
            graph,
            graph.rootScope
          )
          val annotations1 = d.annotations.map { ann =>
            val expression1 = analyseExpression(
              ann.expression,
              topLevelGraph,
              topLevelGraph.rootScope
            )
            (if (expression1 != ann.expression)
               ann.copy(expression = expression1)
             else ann)
              .updateMetadata(
                new MetadataPair(
                  this,
                  alias.Info.Scope.Root(topLevelGraph)
                )
              )
          }
          (if (arguments1 != d.arguments || annotations1 != d.annotations)
             d.copy(arguments = arguments1, annotations = annotations1)
           else d).updateMetadata(
            new MetadataPair(this, alias.Info.Scope.Root(graph))
          )
        })
        (if (params1 != t.params || members1 != t.members)
           t.copy(
             params  = params1,
             members = members1
           )
         else t).updateMetadata(
          new MetadataPair(this, alias.Info.Scope.Root(topLevelGraph))
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
        val expression1 = analyseExpression(
          ann.expression,
          topLevelGraph,
          topLevelGraph.rootScope
        )
        (if (expression1 != ann.expression) ann.copy(expression = expression1)
         else ann).updateMetadata(
          new MetadataPair(this, alias.Info.Scope.Root(topLevelGraph))
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
    graph: alias.Graph,
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

        val expressions1 = expressions.map((expression: Expression) =>
          analyseExpression(
            expression,
            graph,
            currentScope
          )
        )
        val returnValue1 = analyseExpression(
          retVal,
          graph,
          currentScope
        )

        (if (expressions1 != expression || returnValue1 != retVal)
           block
             .copy(
               expressions = expressions1,
               returnValue = returnValue1
             )
         else block)
          .updateMetadata(
            new MetadataPair(
              this,
              alias.Info.Scope.Child(graph, currentScope)
            )
          )
      case binding @ Expression.Binding(name, expression, _, _, _) =>
        if (!parentScope.hasSymbolOccurrenceAs[Occurrence.Def](name.name)) {
          val isSuspended = expression match {
            case Expression.Block(_, _, _, isSuspended, _, _) => isSuspended
            case _                                            => false
          }
          val occurrenceId = graph.nextId()
          val occurrence =
            Occurrence.Def(
              occurrenceId,
              name.name,
              binding.getId(),
              binding.getExternalId,
              isSuspended
            )

          parentScope.add(occurrence)
          parentScope.addDefinition(occurrence)

          val expression1 = analyseExpression(
            expression,
            graph,
            parentScope
          )
          (if (expression1 != expression) binding.copy(expression = expression1)
           else binding)
            .updateMetadata(
              new MetadataPair(
                this,
                alias.Info.Occurrence(graph, occurrenceId)
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
    graph: alias.Graph,
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
          Occurrence.Def(labelId, label.name, label.getId, label.getExternalId)
        parentScope.add(definition)
        parentScope.addDefinition(definition)

        val memberType1 = analyseExpression(memberType, graph, memberTypeScope)
        val value1      = analyseExpression(value, graph, valueScope)

        (if (memberType1 != memberType || value1 != value)
           member
             .copy(
               memberType = memberType1,
               value      = value1
             )
         else member)
          .updateMetadata(
            new MetadataPair(this, alias.Info.Occurrence(graph, labelId))
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
    graph: alias.Graph,
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
        val definition = alias.Graph.Occurrence.Def(
          occurrenceId,
          selfName.name,
          arg.getId(),
          arg.getExternalId
        )
        scope.addDefinition(definition)
        val ascribedType1 =
          arg.ascribedType.map(analyseExpression(_, graph, scope))
        (if (ascribedType1 != arg.ascribedType)
           arg.copy(ascribedType = ascribedType1)
         else arg)
          .updateMetadata(
            new MetadataPair(
              this,
              alias.Info.Occurrence(graph, occurrenceId)
            )
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
          scope.hasSymbolOccurrenceAs[alias.Graph.Occurrence.Def](
            name.name
          )
        if (!nameOccursInScope) {
          val argScope = if (suspended) scope.addChild() else scope
          val newDefault =
            value.map((ir: Expression) =>
              analyseExpression(ir, graph, argScope)
            )

          val occurrenceId = graph.nextId()
          val definition = alias.Graph.Occurrence.Def(
            occurrenceId,
            name.name,
            arg.getId(),
            arg.getExternalId,
            suspended
          )
          scope.add(definition)
          scope.addDefinition(definition)

          val ascribedType1 =
            arg.ascribedType.map(analyseExpression(_, graph, scope))
          (if (newDefault != value || ascribedType1 != arg.ascribedType)
             arg
               .copy(
                 defaultValue = newDefault,
                 ascribedType = ascribedType1
               )
           else arg)
            .updateMetadata(
              new MetadataPair(
                this,
                alias.Info.Occurrence(graph, occurrenceId)
              )
            )
        } else {
          val f = scope.occurrences.values.collectFirst {
            case x if x.symbol == name.name => x
          }
          arg
            .copy(ascribedType = Some(Redefined.Arg(name, arg.location)))
            .updateMetadata(
              new MetadataPair(
                this,
                alias.Info.Occurrence(graph, f.get.id)
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
    graph: alias.Graph,
    scope: alias.Graph.Scope
  ): Application = {
    application match {
      case app @ Application.Prefix(fun, arguments, _, _, _, _) =>
        val function1  = analyseExpression(fun, graph, scope)
        val arguments1 = analyseCallArguments(arguments, graph, scope)
        if (function1 != fun || arguments1 != arguments)
          app.copy(
            function  = function1,
            arguments = arguments1
          )
        else app
      case app @ Application.Force(expr, _, _, _) =>
        val target1 = analyseExpression(expr, graph, scope)
        if (target1 != expr) app.copy(target = target1) else app
      case app @ Application.Sequence(items, _, _, _) =>
        val items1 = items.map(analyseExpression(_, graph, scope))
        if (items1 != items) app.copy(items = items1) else app
      case tSet @ Application.Typeset(expr, _, _, _) =>
        val newScope    = scope.addChild()
        val expression1 = expr.map(analyseExpression(_, graph, newScope))
        (if (expression1 != expr) tSet.copy(expression = expression1) else tSet)
          .updateMetadata(
            new MetadataPair(
              this,
              alias.Info.Scope.Child(graph, newScope)
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
    graph: alias.Graph,
    parentScope: alias.Graph.Scope
  ): List[CallArgument] = {
    args.map { case arg @ CallArgument.Specified(_, expr, _, _, _) =>
      val currentScope = expr match {
        case _: Literal => parentScope
        case _          => parentScope.addChild()
      }
      val value1 = analyseExpression(expr, graph, currentScope)
      (if (value1 != expr) arg.copy(value = value1) else arg)
        .updateMetadata(
          new MetadataPair(
            this,
            alias.Info.Scope.Child(graph, currentScope)
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
    graph: alias.Graph,
    parentScope: Scope,
    lambdaReuseScope: Boolean = false
  ): Function = {
    val currentScope =
      if (lambdaReuseScope) parentScope else parentScope.addChild()

    function match {
      case lambda @ Function.Lambda(arguments, body, _, _, _, _) =>
        val arguments1 = analyseArgumentDefs(arguments, graph, currentScope)
        val body1 = analyseExpression(
          body,
          graph,
          currentScope
        )

        (if (arguments1 != arguments || body1 != body)
           lambda.copy(arguments = arguments1, body = body1)
         else lambda)
          .updateMetadata(
            new MetadataPair(
              this,
              alias.Info.Scope.Child(graph, currentScope)
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
    graph: alias.Graph,
    parentScope: Scope
  ): Name = {
    val occurrenceId = graph.nextId()

    if (isInPatternContext && !isConstructorNameInPatternContext) {
      val definition =
        Occurrence.Def(occurrenceId, name.name, name.getId, name.getExternalId)
      parentScope.add(definition)
      parentScope.addDefinition(definition)
    } else {
      val occurrence =
        Occurrence.Use(occurrenceId, name.name, name.getId, name.getExternalId)
      parentScope.add(occurrence)
      if (!isConstructorNameInPatternContext && !name.isMethod) {
        graph.resolveLocalUsage(occurrence)
      } else {
        graph.resolveGlobalUsage(occurrence)
      }
    }
    name.updateMetadata(
      new MetadataPair(this, alias.Info.Occurrence(graph, occurrenceId))
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
    graph: alias.Graph,
    parentScope: Scope
  ): Case = {
    ir match {
      case caseExpr @ Case.Expr(scrutinee, branches, _, _, _, _) =>
        val scrutinee1 = analyseExpression(scrutinee, graph, parentScope)
        val branches1  = branches.map(analyseCaseBranch(_, graph, parentScope))
        if (scrutinee1 != scrutinee || branches1 != branches)
          caseExpr
            .copy(
              scrutinee = scrutinee1,
              branches  = branches1
            )
        else caseExpr
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
    graph: alias.Graph,
    parentScope: Scope
  ): Case.Branch = {
    val currentScope = parentScope.addChild()
    val pattern1     = analysePattern(branch.pattern, graph, currentScope)
    val expression1 = analyseExpression(
      branch.expression,
      graph,
      currentScope
    )
    (if (pattern1 != branch.pattern || expression1 != branch.expression)
       branch
         .copy(
           pattern    = pattern1,
           expression = expression1
         )
     else branch)
      .updateMetadata(
        new MetadataPair(
          this,
          alias.Info.Scope.Child(graph, currentScope)
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
    graph: alias.Graph,
    parentScope: Scope
  ): Pattern = {
    pattern match {
      case named @ Pattern.Name(name, _, _, _) =>
        val name1 = analyseName(
          name,
          isInPatternContext                = true,
          isConstructorNameInPatternContext = false,
          graph,
          parentScope
        )
        if (name1 != name) named.copy(name = name1) else named
      case cons @ Pattern.Constructor(constructor, fields, _, _, _) =>
        if (!cons.isDesugared) {
          throw new CompilerError(
            "Nested patterns should be desugared by the point of alias " +
            "analysis."
          )
        }
        val constructor1 = analyseName(
          constructor,
          isInPatternContext                = true,
          isConstructorNameInPatternContext = true,
          graph,
          parentScope
        )
        val fields1 = fields.map(analysePattern(_, graph, parentScope))

        if (constructor1 != constructor || fields1 != fields)
          cons.copy(
            constructor = constructor1,
            fields      = fields1
          )
        else cons
      case literalPattern: Pattern.Literal =>
        literalPattern
      case typePattern @ Pattern.Type(name, tpe, _, _, _) =>
        val name1 = analyseName(
          name,
          isInPatternContext                = true,
          isConstructorNameInPatternContext = false,
          graph,
          parentScope
        )
        val tpe1 = analyseName(
          tpe,
          isInPatternContext                = false,
          isConstructorNameInPatternContext = false,
          graph,
          parentScope
        )
        if (name1 != name || tpe1 != tpe)
          typePattern.copy(
            name = name1,
            tpe  = tpe1
          )
        else typePattern
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
