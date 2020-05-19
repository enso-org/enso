package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.ExternalId
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyInfo.Type.asStatic

import scala.collection.mutable

/** This pass implements dataflow analysis for Enso.
  *
  * Dataflow analysis is the processes of determining the dependencies between
  * program expressions.
  *
  * This pass requires the context to provide:
  *
  * - A [[org.enso.interpreter.runtime.scope.LocalScope]], where relevant.
  *
  * It must have the following passes run before it:
  *
  * - [[org.enso.compiler.pass.desugar.FunctionBinding]]
  * - [[AliasAnalysis]]
  * - [[DemandAnalysis]]
  * - [[TailCall]]
  *
  * It also requires that all members of [[IR.IRKind.Primitive]] have been
  * removed from the IR by the time it runs.
  */
//noinspection DuplicatedCode
case object DataflowAnalysis extends IRPass {
  override type Metadata = DependencyInfo
  override type Config   = IRPass.Configuration.Default

  /** Executes the dataflow analysis process on an Enso module.
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
  ): IR.Module = {
    val dependencyInfo = new DependencyInfo
    ir.copy(
        bindings = ir.bindings.map(analyseModuleDefinition(_, dependencyInfo))
      )
      .updateMetadata(this -->> dependencyInfo)
  }

  /** Performs dataflow analysis on an inline expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    val localScope = inlineContext.localScope.getOrElse(
      throw new CompilerError(
        "A valid local scope is required for the inline flow."
      )
    )
    analyseExpression(ir, localScope.dataflowInfo)
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
  def analyseModuleDefinition(
    binding: IR.Module.Scope.Definition,
    info: DependencyInfo
  ): IR.Module.Scope.Definition = {
    binding match {
      case atom @ IR.Module.Scope.Definition.Atom(_, arguments, _, _, _) =>
        arguments.foreach(arg =>
          info.updateAt(asStatic(arg), Set(asStatic(atom)))
        )

        atom
          .copy(
            arguments = arguments.map(analyseDefinitionArgument(_, info))
          )
          .updateMetadata(this -->> info)
      case method @ IR.Module.Scope.Definition.Method
            .Explicit(_, _, body, _, _, _) =>
        info.updateAt(asStatic(body), Set(asStatic(method)))

        method
          .copy(
            body = analyseExpression(body, info)
          )
          .updateMetadata(this -->> info)
      case _: IR.Module.Scope.Definition.Method.Binding =>
        throw new CompilerError(
          "Sugared method definitions should not occur during dataflow " +
          "analysis."
        )
      case err: IR.Error.Redefined => err
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
  def analyseExpression(
    expression: IR.Expression,
    info: DependencyInfo
  ): IR.Expression = {
    expression match {
      case empty: IR.Empty       => empty.updateMetadata(this -->> info)
      case function: IR.Function => analyseFunction(function, info)
      case app: IR.Application   => analyseApplication(app, info)
      case typ: IR.Type          => analyseType(typ, info)
      case name: IR.Name         => analyseName(name, info)
      case cse: IR.Case          => analyseCase(cse, info)
      case comment: IR.Comment   => analyseComment(comment, info)
      case literal: IR.Literal =>
        literal.updateMetadata(this -->> info)
      case foreign: IR.Foreign =>
        foreign.updateMetadata(this -->> info)

      case block @ IR.Expression.Block(expressions, returnValue, _, _, _, _) =>
        info.updateAt(asStatic(returnValue), Set(asStatic(block)))

        block
          .copy(
            expressions = expressions.map(analyseExpression(_, info)),
            returnValue = analyseExpression(returnValue, info)
          )
          .updateMetadata(this -->> info)
      case binding @ IR.Expression.Binding(name, expression, _, _, _) =>
        info.updateAt(asStatic(expression), Set(asStatic(binding)))
        info.updateAt(asStatic(name), Set(asStatic(binding)))

        binding
          .copy(
            name       = name.updateMetadata(this -->> info),
            expression = analyseExpression(expression, info)
          )
          .updateMetadata(this -->> info)

      case error: IR.Error => error
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
  def analyseFunction(
    function: IR.Function,
    info: DependencyInfo
  ): IR.Function = {
    function match {
      case lam @ IR.Function.Lambda(arguments, body, _, _, _, _) =>
        info.updateAt(asStatic(body), Set(asStatic(lam)))
        arguments.foreach(arg =>
          arg.defaultValue.foreach(d =>
            info.updateAt(asStatic(d), Set(asStatic(lam)))
          )
        )

        lam
          .copy(
            arguments = arguments.map(analyseDefinitionArgument(_, info)),
            body      = analyseExpression(body, info)
          )
          .updateMetadata(this -->> info)
      case _: IR.Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during alias analysis."
        )
    }
  }

  /** Performs dependency analysis on an application.
    *
    * A prefix application depends on the values of the function and arguments,
    * while a force depends purely on the term being forced.
    *
    * @param application the appliation to perform dependency analysis on
    * @param info the dependency information for the module
    * @return `application`, with attached dependency information
    */
  def analyseApplication(
    application: IR.Application,
    info: DependencyInfo
  ): IR.Application = {
    application match {
      case prefix @ IR.Application.Prefix(fn, args, _, _, _, _) =>
        info.updateAt(asStatic(fn), Set(asStatic(prefix)))
        args.foreach(arg => info.updateAt(asStatic(arg), Set(asStatic(prefix))))

        prefix
          .copy(
            function  = analyseExpression(fn, info),
            arguments = args.map(analyseCallArgument(_, info))
          )
          .updateMetadata(this -->> info)
      case force @ IR.Application.Force(target, _, _, _) =>
        info.updateAt(asStatic(target), Set(asStatic(force)))

        force
          .copy(target = analyseExpression(target, info))
          .updateMetadata(this -->> info)
      case vector @ IR.Application.Literal.Sequence(items, _, _, _) =>
        items.foreach(it => info.updateAt(asStatic(it), Set(asStatic(vector))))

        vector
          .copy(items = items.map(analyseExpression(_, info)))
          .updateMetadata(this -->> info)
      case _: IR.Application.Operator =>
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
  def analyseType(typ: IR.Type, info: DependencyInfo): IR.Type = {
    typ match {
      case asc @ IR.Type.Ascription(typed, signature, _, _, _) =>
        info.updateAt(asStatic(typed), Set(asStatic(asc)))
        info.updateAt(asStatic(signature), Set(asStatic(asc)))

        asc
          .copy(
            typed     = analyseExpression(typed, info),
            signature = analyseExpression(signature, info)
          )
          .updateMetadata(this -->> info)
      case ctx @ IR.Type.Context(typed, context, _, _, _) =>
        info.updateAt(asStatic(typed), Set(asStatic(ctx)))
        info.updateAt(asStatic(context), Set(asStatic(ctx)))

        ctx
          .copy(
            typed   = analyseExpression(typed, info),
            context = analyseExpression(context, info)
          )
          .updateMetadata(this -->> info)
      case member @ IR.Type.Set.Member(_, memberType, value, _, _, _) =>
        info.updateAt(asStatic(memberType), Set(asStatic(member)))
        info.updateAt(asStatic(value), Set(asStatic(member)))

        member
          .copy(
            memberType = analyseExpression(memberType, info),
            value      = analyseExpression(value, info)
          )
          .updateMetadata(this -->> info)
      case concat @ IR.Type.Set.Concat(left, right, _, _, _) =>
        info.updateAt(asStatic(left), Set(asStatic(concat)))
        info.updateAt(asStatic(right), Set(asStatic(concat)))

        concat
          .copy(
            left  = analyseExpression(left, info),
            right = analyseExpression(right, info)
          )
          .updateMetadata(this -->> info)
      case eq @ IR.Type.Set.Equality(left, right, _, _, _) =>
        info.updateAt(asStatic(left), Set(asStatic(eq)))
        info.updateAt(asStatic(right), Set(asStatic(eq)))

        eq.copy(
            left  = analyseExpression(left, info),
            right = analyseExpression(right, info)
          )
          .updateMetadata(this -->> info)
      case intersect @ IR.Type.Set.Intersection(left, right, _, _, _) =>
        info.updateAt(asStatic(left), Set(asStatic(intersect)))
        info.updateAt(asStatic(right), Set(asStatic(intersect)))

        intersect
          .copy(
            left  = analyseExpression(left, info),
            right = analyseExpression(right, info)
          )
          .updateMetadata(this -->> info)
      case union @ IR.Type.Set.Union(left, right, _, _, _) =>
        info.updateAt(asStatic(left), Set(asStatic(union)))
        info.updateAt(asStatic(right), Set(asStatic(union)))

        union
          .copy(
            left  = analyseExpression(left, info),
            right = analyseExpression(right, info)
          )
          .updateMetadata(this -->> info)
      case subsumption @ IR.Type.Set.Subsumption(left, right, _, _, _) =>
        info.updateAt(asStatic(left), Set(asStatic(subsumption)))
        info.updateAt(asStatic(right), Set(asStatic(subsumption)))

        subsumption
          .copy(
            left  = analyseExpression(left, info),
            right = analyseExpression(right, info)
          )
          .updateMetadata(this -->> info)
      case subtraction @ IR.Type.Set.Subtraction(left, right, _, _, _) =>
        info.updateAt(asStatic(left), Set(asStatic(subtraction)))
        info.updateAt(asStatic(right), Set(asStatic(subtraction)))

        subtraction
          .copy(
            left  = analyseExpression(left, info),
            right = analyseExpression(right, info)
          )
          .updateMetadata(this -->> info)
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
  def analyseName(name: IR.Name, info: DependencyInfo): IR.Name = {
    val aliasInfo = name.passData
      .getUnsafe(AliasAnalysis)(
        "Name occurrence with missing aliasing information."
      )
      .unsafeAs[AliasAnalysis.Info.Occurrence]

    name match {
      case _: IR.Name.Blank =>
        throw new CompilerError(
          "Blanks should not be present during dataflow analysis."
        )
      case _ =>
        val defIdForName = aliasInfo.graph.defLinkFor(aliasInfo.id)
        val key: DependencyInfo.Type = defIdForName match {
          case Some(defLink) =>
            aliasInfo.graph.getOccurrence(defLink.target) match {
              case Some(AliasAnalysis.Graph.Occurrence.Def(_, _, id, ext, _)) =>
                DependencyInfo.Type.Static(id, ext)
              case _ => DependencyInfo.Type.Dynamic(name.name, None)
            }

          case None => DependencyInfo.Type.Dynamic(name.name, None)
        }

        info.updateAt(key, Set(asStatic(name)))

        name.updateMetadata(this -->> info)
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
  def analyseCase(cse: IR.Case, info: DependencyInfo): IR.Case = {
    cse match {
      case expr @ IR.Case.Expr(scrutinee, branches, fallback, _, _, _) =>
        info.updateAt(asStatic(scrutinee), Set(asStatic(expr)))
        branches.foreach(branch =>
          info.updateAt(asStatic(branch), Set(asStatic(expr)))
        )
        fallback.foreach(fback =>
          info.updateAt(asStatic(fback), Set(asStatic(expr)))
        )

        expr
          .copy(
            scrutinee = analyseExpression(scrutinee, info),
            branches  = branches.map(analyseCaseBranch(_, info)),
            fallback  = fallback.map(analyseExpression(_, info))
          )
          .updateMetadata(this -->> info)
      case _: IR.Case.Branch =>
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
  def analyseCaseBranch(
    branch: IR.Case.Branch,
    info: DependencyInfo
  ): IR.Case.Branch = {
    val pattern    = branch.pattern
    val expression = branch.expression

    info.updateAt(asStatic(pattern), Set(asStatic(branch)))
    info.updateAt(asStatic(expression), Set(asStatic(branch)))

    branch
      .copy(
        pattern    = analyseExpression(pattern, info),
        expression = analyseExpression(expression, info)
      )
      .updateMetadata(this -->> info)
  }

  /** Performs dataflow analysis on a comment entity.
    *
    * A comment expression is simply dependent on the result of the commented
    * value.
    *
    * @param comment the comment to perform dataflow analysis on
    * @param info the dependency information for the module
    * @return `comment`, with attached dependency information
    */
  def analyseComment(comment: IR.Comment, info: DependencyInfo): IR.Comment = {
    comment match {
      case doc @ IR.Comment.Documentation(commented, _, _, _, _) =>
        info.updateAt(asStatic(commented), Set(asStatic(comment)))

        doc
          .copy(
            commented = analyseExpression(commented, info)
          )
          .updateMetadata(this -->> info)
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
  def analyseDefinitionArgument(
    argument: IR.DefinitionArgument,
    info: DependencyInfo
  ): IR.DefinitionArgument = {
    argument match {
      case spec @ IR.DefinitionArgument.Specified(_, defValue, _, _, _, _) =>
        defValue.foreach(expr =>
          info.updateAt(asStatic(expr), Set(asStatic(spec)))
        )

        spec
          .copy(
            defaultValue = defValue.map(analyseExpression(_, info))
          )
          .updateMetadata(this -->> info)
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
  def analyseCallArgument(
    argument: IR.CallArgument,
    info: DependencyInfo
  ): IR.CallArgument = {
    argument match {
      case spec @ IR.CallArgument.Specified(name, value, _, _, _, _) =>
        info.updateAt(asStatic(value), Set(asStatic(spec)))
        name.foreach(name => info.updateAt(asStatic(name), Set(asStatic(spec))))

        spec
          .copy(
            value = analyseExpression(value, info)
          )
          .updateMetadata(this -->> info)
    }
  }

  // === Pass Metadata ========================================================

  /** Storage for dependency information.
    *
    * @param dependencies storage for the direct dependencies between program
    *                     components
    */
  sealed case class DependencyInfo(
    dependencies: mutable.Map[DependencyInfo.Type, Set[DependencyInfo.Type]] =
      mutable.Map()
  ) extends IRPass.Metadata {
    override val metadataName: String = "DataflowAnalysis.Dependencies"

    /** Returns the set of all dependents for the provided key.
      *
      * Please note that the result set contains not just the _direct_
      * dependents of the key, but _all_ dependents of the key.
      *
      * @param key the key to get the dependents of
      * @return the set of all dependencies on `key`
      * @throws NoSuchElementException when `key` does not exist in the
      *                                dependencies mapping
      */
    @throws[NoSuchElementException]
    def apply(key: DependencyInfo.Type): Set[DependencyInfo.Type] = {
      if (dependencies.contains(key)) {
        get(key) match {
          case Some(deps) => deps
          case None       => throw new NoSuchElementException
        }
      } else {
        throw new NoSuchElementException
      }
    }

    /** Obtains the _direct_ dependents of a given node in the IR.
      *
      * Please note that this does _not_ return the transitive closure of all
      * dependents of the node.
      *
      * @param key the key to get the dependents of
      * @return the set of the _direct_ dependencies on `key`, if it exists
      */
    def getDirect(
      key: DependencyInfo.Type
    ): Option[Set[DependencyInfo.Type]] = {
      dependencies.get(key)
    }

    /** Obtains the external identifiers of the _direct_ dependents of a given
      * node in the IR.
      *
      * @param key the key to get the dependents of
      * @return the set of external identifiers for the direct dependencies of
      *         `key`, if they exist
      */
    def getExternalDirect(
      key: DependencyInfo.Type
    ): Option[Set[IR.ExternalId]] = {
      getDirect(key).map(_.flatMap(_.externalId))
    }

    /** Safely gets the set of all dependents for the provided key.
      *
      * Please note that the result set contains not just the _direct_
      * dependents of the key, but _all_ dependents of the key.
      *
      * @param key the key to get the dependents of
      * @return the set of all dependencies on `key`, if key exists
      */
    def get(key: DependencyInfo.Type): Option[Set[DependencyInfo.Type]] = {
      val visited = mutable.Set[DependencyInfo.Type]()

      def go(key: DependencyInfo.Type): Set[DependencyInfo.Type] = {
        if (!visited.contains(key)) {
          visited += key

          dependencies.get(key) match {
            case Some(deps) => deps ++ deps.map(go).reduceLeft(_ ++ _)
            case None       => Set()
          }
        } else {
          Set()
        }
      }

      if (dependencies.contains(key)) {
        Some(go(key))
      } else {
        None
      }
    }

    /** Safely gets the external identifiers for all dependents of the provided
      * key.
      *
      * Please note that the result set contains not just the _direct_
      * dependents of the key, but all dependents of the key.
      *
      * @param key the key from which to get the external identifiers of its
      *            dependents
      * @return the set of all external identifiers of dependents on `key`, if
      *         it exists
      */
    def getExternal(key: DependencyInfo.Type): Option[Set[IR.ExternalId]] = {
      get(key).map(_.flatMap(_.externalId))
    }

    /** Executes an update on the dependency information.
      *
      * @param key the key to update the dependents for
      * @param dependents the updated dependents for `key`
      */
    def update(
      key: DependencyInfo.Type,
      dependents: Set[DependencyInfo.Type]
    ): Unit =
      dependencies(key) = dependents

    /** Updates the dependents for the provided key, or creates them if they do
      * not already exist.
      *
      * @param key the key to add or update dependents for
      * @param dependents the new dependents information for `key`
      */
    def updateAt(
      key: DependencyInfo.Type,
      dependents: Set[DependencyInfo.Type]
    ): Unit = {
      if (dependencies.contains(key)) {
        dependencies(key) ++= dependents
      } else {
        dependencies(key) = dependents
      }
    }

    /** Updates the dependents for the provided keys, or creates them if they do
      * not already exist.
      *
      * @param keys the keys to add or update dependents for
      * @param dependents the new dependents information for each `key` in
      *                   `keys`
      */
    def updateAt(
      keys: List[DependencyInfo.Type],
      dependents: Set[DependencyInfo.Type]
    ): Unit = keys.foreach(key => updateAt(key, dependents))

    /** Combines two dependency information containers.
      *
      * @param that the other contaoner to combine with `this`
      * @return the result of combining `this` and `that`
      */
    def ++(that: DependencyInfo): DependencyInfo = {
      val combinedModule = new DependencyInfo(this.dependencies)

      for ((key, value) <- that.dependencies) {
        combinedModule.dependencies.get(key) match {
          case Some(xs) => combinedModule(key) = value ++ xs
          case None     => combinedModule(key) = value
        }
      }

      combinedModule
    }
  }
  object DependencyInfo {

    /** The type of identifiers in this analysis. */
    type Identifier = IR.Identifier

    /** The type of symbols in this analysis. */
    type Symbol = String

    /** The type of identification for a program component. */
    sealed trait Type {
      val externalId: Option[IR.ExternalId]
    }
    object Type {

      /** Program components identified by their unique identifier.
        *
        * @param id the unique identifier of the program component
        * @param externalId the external identifier corresponding to the program
        *                   component
        */
      sealed case class Static(
        id: DependencyInfo.Identifier,
        override val externalId: Option[ExternalId]
      ) extends Type

      /** Program components identified by their symbol.
        *
        * @param name the name of the symbol
        * @param externalId the external identifier corresponding to the program
        *                   component
        */
      sealed case class Dynamic(
        name: DependencyInfo.Symbol,
        override val externalId: Option[ExternalId]
      ) extends Type

      // === Utility Functions ================================================

      /** Creates a static dependency on an IR node.
        *
        * @param ir the IR node to create a dependency on
        * @return a static dependency on `ir`
        */
      def asStatic(ir: IR): Static = {
        Static(ir.getId, ir.getExternalId)
      }

      /** Creates a dynamic dependency on an IR node.
        *
        * @param ir the IR node to create a dependency on
        * @return a dynamic dependency on `ir`
        */
      def asDynamic(ir: IR.Name): Dynamic = {
        Dynamic(ir.name, ir.getExternalId)
      }
    }
  }
}
