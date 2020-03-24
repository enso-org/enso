package org.enso.compiler.codegen

import cats.Foldable
import cats.implicits._
import org.enso.compiler.core.IR._
import org.enso.compiler.exception.UnhandledEntity
import org.enso.interpreter.Constants
import org.enso.syntax.text.{AST, Location}

// FIXME [AA] All places where we currently throw a `RuntimeException` should
//  generate informative and useful nodes in core.

/**
  * This file contains the functionality that translates from the parser's
  * [[AST]] type to the internal representation used by the compiler.
  *
  * This representation is currently [[Expression]], but this will change as
  * [[Core]] becomes implemented. Most function docs will refer to [[Core]]
  * now, as this will become true soon.
  */
object AstToIR {

  /** Translates a program represented in the parser [[AST]] to the compiler's
    * [[Core]] internal representation.
    *
    * @param inputAST the [[AST]] representing the program to translate
    * @return the [[Core]] representation of `inputAST`
    */
  def translate(inputAST: AST): Module = {
    inputAST match {
      case AST.Module.any(inputAST) => translateModule(inputAST)
      case _ => {
        throw new UnhandledEntity(inputAST, "translate")
      }
    }
  }

  /** Translates an inline program expression represented in the parser [[AST]]
    * into the compiler's [[Core]] representation.
    *
    * Inline expressions must _only_ be expressions, and may not contain any
    * type of definition.
    *
    * @param inputAST the [[AST]] representing the expression to translate.
    * @return the [[Core]] representation of `inputAST` if it is valid,
    *         otherwise [[None]]
    */
  def translateInline(inputAST: AST): Option[Expression] = {
    inputAST match {
      case AST.Module.any(module) =>
        val presentBlocks = module.lines.collect {
          case t if t.elem.isDefined => t.elem.get
        }

        val expressions = presentBlocks.map(translateExpression)

        expressions match {
          case List()     => None
          case List(expr) => Some(expr)
          case _ =>
            Some(
              Expression.Block(
                expressions.dropRight(1),
                expressions.last,
                location = Foldable[List].foldMap(expressions)(_.location)
              )
            )
        }
      case _ => None
    }
  }

  /** Translate a top-level Enso module into [[Core]].
    *
    * @param module the [[AST]] representation of the module to translate
    * @return the [[Core]] representation of `module`
    */
  def translateModule(module: AST.Module): Module = {
    module match {
      case AST.Module(blocks) => {
        val presentBlocks = blocks.collect {
          case t if t.elem.isDefined => t.elem.get
        }

        val imports = presentBlocks.collect {
          case AST.Import.any(list) => translateImport(list)
        }

        val nonImportBlocks = presentBlocks.filter {
          case AST.Import.any(_) => false
          case _                 => true
        }

        val statements = nonImportBlocks.map(translateModuleSymbol)
        Module(imports, statements, module.location)
      }
    }
  }

  /** Translates a module-level definition from its [[AST]] representation into
    * [[Core]].
    *
    * @param inputAST the definition to be translated
    * @return the [[Core]] representation of `inputAST`
    */
  def translateModuleSymbol(inputAST: AST): Module.Scope.Definition = {
    inputAST match {
      case AST.Def(consName, args, body) =>
        if (body.isDefined) {
          throw new RuntimeException("Cannot support complex type defs yet!!!!")
        } else {
          Module.Scope.Definition
            .Atom(
              Name.Literal(consName.name, consName.location),
              args.map(translateArgumentDefinition(_)),
              inputAST.location
            )
        }
      case AstView.MethodDefinition(targetPath, name, definition) =>
        val (path, pathLoc) = if (targetPath.nonEmpty) {
          val pathSegments = targetPath.collect {
            case AST.Ident.Cons.any(c) => c
          }

          val pathStr = pathSegments.map(_.name).mkString(".")
          val loc = pathSegments.headOption
            .flatMap(_.location)
            .flatMap(
              locationStart =>
                pathSegments.lastOption
                  .flatMap(_.location)
                  .flatMap(
                    locationEnd =>
                      Some(locationStart.copy(end = locationEnd.end))
                  )
            )

          (pathStr, loc)
        } else {
          (Constants.Names.CURRENT_MODULE, None)
        }

        val nameStr = name match { case AST.Ident.Var.any(name) => name }
        Module.Scope.Definition.Method(
          Name.Literal(path, pathLoc),
          Name.Literal(nameStr.name, nameStr.location),
          translateExpression(definition),
          inputAST.location
        )
      case _ =>
        throw new UnhandledEntity(inputAST, "translateModuleSymbol")
    }
  }

  /** Translates an arbitrary program expression from [[AST]] into [[Core]].
    *
    * @param inputAST the expresion to be translated
    * @return the [[Core]] representation of `inputAST`
    */
  def translateExpression(inputAST: AST): Expression = {
    inputAST match {
      case AstView
            .SuspendedBlock(name, block @ AstView.Block(lines, lastLine)) =>
        Expression.Binding(
          Name.Literal(name.name, name.location),
          Expression.Block(
            lines.map(translateExpression),
            translateExpression(lastLine),
            block.location,
            suspended = true
          ),
          inputAST.location
        )
      case AstView.Assignment(name, expr) =>
        translateBinding(inputAST.location, name, expr)
      case AstView.MethodCall(target, name, args) =>
        val (validArguments, hasDefaultsSuspended) =
          calculateDefaultsSuspension(args)

        // Note [Uniform Call Syntax Translation]
        Application.Prefix(
          translateExpression(name),
          (target :: validArguments).map(translateCallArgument),
          hasDefaultsSuspended = hasDefaultsSuspended,
          inputAST.location
        )
      case AstView.CaseExpression(scrutinee, branches) =>
        val actualScrutinee = translateExpression(scrutinee)
        val nonFallbackBranches =
          branches
            .takeWhile(AstView.FallbackCaseBranch.unapply(_).isEmpty)
            .map(translateCaseBranch)
        val potentialFallback =
          branches
            .drop(nonFallbackBranches.length)
            .headOption
            .map(translateFallbackBranch)
        Case.Expr(
          actualScrutinee,
          nonFallbackBranches,
          potentialFallback,
          inputAST.location
        )
      case AST.App.any(inputAST)     => translateApplicationLike(inputAST)
      case AST.Mixfix.any(inputAST)  => translateApplicationLike(inputAST)
      case AST.Literal.any(inputAST) => translateLiteral(inputAST)
      case AST.Group.any(inputAST) =>
        translateGroup(inputAST, translateExpression)
      case AST.Ident.any(inputAST) => translateIdent(inputAST)
      case AstView.Block(lines, retLine) =>
        Expression.Block(
          lines.map(translateExpression),
          translateExpression(retLine),
          location = inputAST.location
        )
      case AST.Comment.any(inputAST) => translateComment(inputAST)
      case AST.Invalid.any(inputAST) => translateInvalid(inputAST)
      case AST.Foreign(_, _, _) =>
        throw new RuntimeException(
          "Enso does not yet support foreign language blocks"
        )
      case _ =>
        throw new UnhandledEntity(inputAST, "translateExpression")
    }
  }

  /* Note [Uniform Call Syntax Translation]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * As the uniform call syntax must work for both methods and functions, the
   * conversion can't take advantage of any by-name application semantics at the
   * current time.
   *
   * This means that it is a purely _positional_ conversion on the first
   * argument and cannot be performed any other way.
   */

  /** Translates a program literal from its [[AST]] representation into
    * [[Core]].
    *
    * @param literal the literal to translate
    * @return the [[Core]] representation of `literal`
    */
  def translateLiteral(literal: AST.Literal): Expression = {
    literal match {
      case AST.Literal.Number(base, number) => {
        if (base.isDefined && base.get != "10") {
          throw new RuntimeException("Only base 10 is currently supported")
        }

        Literal.Number(number, literal.location)
      }
      case AST.Literal.Text.any(literal) =>
        literal.shape match {
          case AST.Literal.Text.Line.Raw(segments) =>
            val fullString = segments.collect {
              case AST.Literal.Text.Segment.Plain(str)   => str
              case AST.Literal.Text.Segment.RawEsc(code) => code.repr
            }.mkString

            Literal.Text(fullString, literal.location)
          case AST.Literal.Text.Block.Raw(lines, _, _) =>
            val fullString = lines
              .map(
                t =>
                  t.text.collect {
                    case AST.Literal.Text.Segment.Plain(str)   => str
                    case AST.Literal.Text.Segment.RawEsc(code) => code.repr
                  }.mkString
              )
              .mkString("\n")

            Literal.Text(fullString, literal.location)
          case AST.Literal.Text.Block.Fmt(_, _, _) =>
            throw new RuntimeException("Format strings not yet supported")
          case AST.Literal.Text.Line.Fmt(_) =>
            throw new RuntimeException("Format strings not yet supported")
          case _ =>
            throw new UnhandledEntity(literal.shape, "translateLiteral")
        }
      case _ => throw new UnhandledEntity(literal, "processLiteral")
    }
  }

  /** Translates an argument definition from [[AST]] into [[Core]].
    *
    * @param arg the argument to translate
    * @param isSuspended `true` if the argument is suspended, otherwise `false`
    * @return the [[Core]] representation of `arg`
    */
  @scala.annotation.tailrec
  def translateArgumentDefinition(
    arg: AST,
    isSuspended: Boolean = false
  ): DefinitionArgument = {
    arg match {
      case AstView.LazyAssignedArgumentDefinition(name, value) =>
        DefinitionArgument.Specified(
          Name.Literal(name.name, name.location),
          Some(translateExpression(value)),
          suspended = true,
          arg.location
        )
      case AstView.LazyArgument(arg) =>
        translateArgumentDefinition(arg, isSuspended = true)
      case AstView.DefinitionArgument(arg) =>
        DefinitionArgument.Specified(
          Name.Literal(arg.name, arg.location),
          None,
          isSuspended,
          arg.location
        )
      case AstView.AssignedArgument(name, value) =>
        DefinitionArgument.Specified(
          Name.Literal(name.name, name.location),
          Some(translateExpression(value)),
          isSuspended,
          arg.location
        )
      case _ =>
        throw new UnhandledEntity(arg, "translateArgumentDefinition")
    }
  }

  /** Translates a call-site function argument from its [[AST]] representation
    * into [[Core]].
    *
    * @param arg the argument to translate
    * @return the [[Core]] representation of `arg`
    */
  def translateCallArgument(arg: AST): CallArgument.Specified = arg match {
    case AstView.AssignedArgument(left, right) =>
      CallArgument
        .Specified(
          Some(Name.Literal(left.name, left.location)),
          translateExpression(right),
          arg.location
        )
    case _ =>
      CallArgument.Specified(None, translateExpression(arg), arg.location)
  }

  /** Calculates whether a set of arguments has its defaults suspended, and
    * processes the argument list to remove that operator.
    *
    * @param args the list of arguments
    * @return the list of arguments with the suspension operator removed, and
    *         whether or not the defaults are suspended
    */
  def calculateDefaultsSuspension(args: List[AST]): (List[AST], Boolean) = {
    val validArguments = args.filter {
      case AstView.SuspendDefaultsOperator(_) => false
      case _                                  => true
    }

    val suspendPositions = args.zipWithIndex.collect {
      case (AstView.SuspendDefaultsOperator(_), ix) => ix
    }

    val hasDefaultsSuspended = suspendPositions.contains(args.length - 1)

    (validArguments, hasDefaultsSuspended)
  }

  /** Translates an arbitrary expression that takes the form of a syntactic
    * application from its [[AST]] representation into [[Core]].
    *
    * @param callable the callable to translate
    * @return the [[Core]] representation of `callable`
    */
  def translateApplicationLike(callable: AST): Expression = {
    callable match {
      case AstView.ContextAscription(expr, context) =>
        Type.Context(
          translateExpression(expr),
          translateExpression(context),
          callable.location
        )
      case AstView.ForcedTerm(term) =>
        Application.Force(translateExpression(term), callable.location)
      case AstView.Application(name, args) =>
        val (validArguments, hasDefaultsSuspended) =
          calculateDefaultsSuspension(args)

        Application.Prefix(
          translateExpression(name),
          validArguments.map(translateCallArgument),
          hasDefaultsSuspended,
          callable.location
        )
      case AstView.Lambda(args, body) =>
        val realArgs = args.map(translateArgumentDefinition(_))
        val realBody = translateExpression(body)
        Function.Lambda(realArgs, realBody, callable.location)
      case AST.App.Infix(left, fn, right) =>
        Application.Operator.Binary(
          translateExpression(left),
          Name.Literal(fn.name, fn.location),
          translateExpression(right),
          callable.location
        )
      case AST.App.Prefix(_, _) =>
        throw new RuntimeException(
          "Enso does not support arbitrary prefix expressions"
        )
      case AST.App.Section.any(_) =>
        throw new RuntimeException(
          "Enso does not yet support operator sections"
        )
      case AST.Mixfix(nameSegments, args) =>
        val realNameSegments = nameSegments.collect {
          case AST.Ident.Var.any(v) => v
        }

        if (realNameSegments.length != nameSegments.length) {
          throw new RuntimeException("Badly named mixfix function.")
        }

        val functionName =
          AST.Ident.Var(realNameSegments.map(_.name).mkString("_"))

        Application.Prefix(
          translateExpression(functionName),
          args.map(translateCallArgument).toList,
          false,
          callable.location
        )
      case _ => throw new UnhandledEntity(callable, "translateCallable")
    }
  }

  /** Translates an arbitrary program identifier from its [[AST]] representation
    * into [[Core]].
    *
    * @param identifier the identifier to translate
    * @return the [[Core]] representation of `identifier`
    */
  def translateIdent(identifier: AST.Ident): Expression = {
    identifier match {
      case AST.Ident.Var(name) =>
        if (name == "this") {
          Name.This(identifier.location)
        } else if (name == "here") {
          Name.Here(identifier.location)
        } else {
          Name.Literal(name, identifier.location)
        }
      case AST.Ident.Cons(name) => Name.Literal(name, identifier.location)
      case AST.Ident.Blank(_) =>
        throw new RuntimeException("Blanks not yet properly supported")
      case AST.Ident.Opr.any(_) =>
        throw new RuntimeException("Operators not generically supported yet")
      case AST.Ident.Mod(_) =>
        throw new RuntimeException(
          "Enso does not support arbitrary module identifiers yet"
        )
      case _ =>
        throw new UnhandledEntity(identifier, "translateIdent")
    }
  }

  /** Translates an arbitrary binding operation from its [[AST]] representation
    * into [[Core]].
    *
    * @param location the source location of the binding
    * @param name the name of the binding being assigned to
    * @param expr the expression being assigned to `name`
    * @return the [[Core]] representation of `expr` being bound to `name`
    */
  def translateBinding(
    location: Option[Location],
    name: AST,
    expr: AST
  ): Expression.Binding = {
    name match {
      case v @ AST.Ident.Var(name) =>
        Expression.Binding(
          Name.Literal(name, v.location),
          translateExpression(expr),
          location
        )
      case _ =>
        throw new UnhandledEntity(name, "translateAssignment")
    }
  }

  /** Translates the branch of a case expression from its [[AST]] representation
    * into [[Core]].
    *
    * @param branch the case branch to translate
    * @return the [[Core]] representation of `branch`
    */
  def translateCaseBranch(branch: AST): Case.Branch = {
    branch match {
      case AstView.ConsCaseBranch(cons, args, body) =>
        Case.Branch(
          translateExpression(cons),
          Function.Lambda(
            args.map(translateArgumentDefinition(_)),
            translateExpression(body),
            body.location,
            canBeTCO = false
          ),
          branch.location
        )

      case _ => throw new UnhandledEntity(branch, "translateCaseBranch")
    }
  }

  /** Translates the fallback branch of a case expression from its [[AST]]
    * representation into [[Core]].
    *
    * @param branch the fallback branch to translate
    * @return the [[Core]] representation of `branch`
    */
  def translateFallbackBranch(branch: AST): Function = {
    branch match {
      case AstView.FallbackCaseBranch(body) =>
        Function.Lambda(
          List(),
          translateExpression(body),
          body.location,
          canBeTCO = false
        )
      case _ => throw new UnhandledEntity(branch, "translateFallbackBranch")
    }
  }

  /** Translates an arbitrary grouped piece of syntax from its [[AST]]
    * representation into [[Core]].
    *
    * It is currently an error to have an empty group.
    *
    * @param group the group to translate
    * @param translator the function to apply to the group's contents
    * @tparam T the result type of translating the expression contained in
    *           `group`
    * @return the [[Core]] representation of the contents of `group`
    */
  def translateGroup[T](group: AST.Group, translator: AST => T): T = {
    group.body match {
      case Some(ast) => translator(ast)
      case None => {
        throw new RuntimeException("Empty group")
      }
    }
  }

  /** Translates an import statement from its [[AST]] representation into
    * [[Core]].
    *
    * @param imp the import to translate
    * @return the [[Core]] representation of `imp`
    */
  def translateImport(imp: AST.Import): Module.Scope.Import = {
    Module.Scope.Import(
      imp.path.map(t => t.name).reduceLeft((l, r) => l + "." + r),
      imp.location
    )
  }

  /** Translates an arbitrary invalid expression from the [[AST]] representation
    * of the program into its [[Core]] representation.
    *
    * @param invalid the invalid entity to translate
    * @return the [[Core]] representation of `invalid`
    */
  def translateInvalid(invalid: AST.Invalid): Expression = {
    invalid match {
      case AST.Invalid.Unexpected(_, _) =>
        throw new RuntimeException(
          "Enso does not yet support unexpected blocks properly"
        )
      case AST.Invalid.Unrecognized(_) =>
        throw new RuntimeException(
          "Enso does not yet support unrecognised tokens properly"
        )
      case AST.Ident.InvalidSuffix(_, _) =>
        throw new RuntimeException(
          "Enso does not yet support invalid suffixes properly"
        )
      case AST.Literal.Text.Unclosed(_) =>
        throw new RuntimeException(
          "Enso does not yet support unclosed text literals properly"
        )
      case _ =>
        throw new RuntimeException(
          "Fatal: Unhandled entity in processInvalid = " + invalid
        )
    }
  }

  /** Translates a comment from its [[AST]] representation into its [[Core]]
    * representation.
    *
    * Currently this only supports documentation comments, and not standarc
    * types of comments as they can't currently be represented.
    *
    * @param comment the comment to transform
    * @return the [[Core]] representation of `comment`
    */
  def translateComment(comment: AST): Expression = {
    comment match {
      case AST.Comment(_) =>
        throw new RuntimeException(
          "Enso does not yet support comments properly"
        )
      case AST.Documented(doc, _, ast) =>
        Comment.Documentation(translateExpression(ast), doc, comment.location)
      case _ =>
        throw new UnhandledEntity(comment, "processComment")
    }
  }
}
