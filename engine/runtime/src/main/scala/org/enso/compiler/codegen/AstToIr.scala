package org.enso.compiler.codegen

import cats.Foldable
import cats.implicits._
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR._
import org.enso.compiler.exception.UnhandledEntity
import org.enso.interpreter.Constants
import org.enso.syntax.text.AST

/**
  * This file contains the functionality that translates from the parser's
  * [[AST]] type to the internal representation used by the compiler.
  *
  * This representation is currently [[Expression]], but this will change as
  * [[Core]] becomes implemented. Most function docs will refer to [[Core]]
  * now, as this will become true soon.
  */
object AstToIr {
  private def getIdentifiedLocation(ast: AST): Option[IdentifiedLocation] =
    ast.location.map(IdentifiedLocation(_, ast.id))

  /** Translates a program represented in the parser [[AST]] to the compiler's
    * [[Core]] internal representation.
    *
    * @param inputAST the [[AST]] representing the program to translate
    * @return the [[Core]] representation of `inputAST`
    */
  def translate(inputAST: AST): Module = {
    inputAST match {
      case AST.Module.any(inputAST) => translateModule(inputAST)
      case _ =>
        throw new UnhandledEntity(inputAST, "translate")
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
            val locations    = expressions.map(_.location.map(_.location))
            val locationsSum = Foldable[List].fold(locations)
            Some(
              Expression.Block(
                expressions.dropRight(1),
                expressions.last,
                location = locationsSum.map(IdentifiedLocation(_))
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
          case AST.JavaImport.any(imp) =>
            val pkg = imp.path.init.map(_.name)
            val cls = imp.path.last.name
            Module.Scope.Import.Polyglot(
              Module.Scope.Import.Polyglot.Java(pkg.mkString("."), cls),
              getIdentifiedLocation(imp)
            )
        }

        val nonImportBlocks = presentBlocks.filter {
          case AST.Import.any(_)     => false
          case AST.JavaImport.any(_) => false
          case _                     => true
        }

        val statements = nonImportBlocks.map(translateModuleSymbol)
        Module(imports, statements, getIdentifiedLocation(module))
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
          throw new UnhandledEntity(inputAST, "translateModuleSymbol")
        } else {
          Module.Scope.Definition
            .Atom(
              Name.Literal(consName.name, getIdentifiedLocation(consName)),
              args.map(translateArgumentDefinition(_)),
              getIdentifiedLocation(inputAST)
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
            .flatMap(locationStart =>
              pathSegments.lastOption
                .flatMap(_.location)
                .flatMap(locationEnd =>
                  Some(locationStart.copy(end = locationEnd.end))
                )
            )

          (pathStr, loc)
        } else {
          (Constants.Names.CURRENT_MODULE, None)
        }

        val nameStr = name match { case AST.Ident.Var.any(name) => name }
        Module.Scope.Definition.Method(
          Name.Literal(path, pathLoc.map(IdentifiedLocation(_))),
          Name.Literal(nameStr.name, getIdentifiedLocation(nameStr)),
          translateExpression(definition),
          getIdentifiedLocation(inputAST)
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
          Name.Literal(name.name, getIdentifiedLocation(name)),
          Expression.Block(
            lines.map(translateExpression),
            translateExpression(lastLine),
            getIdentifiedLocation(block),
            suspended = true
          ),
          getIdentifiedLocation(inputAST)
        )
      case AstView.Assignment(name, expr) =>
        translateBinding(getIdentifiedLocation(inputAST), name, expr)
      case AstView.MethodCall(target, name, args) =>
        val (validArguments, hasDefaultsSuspended) =
          calculateDefaultsSuspension(args)

        // Note [Uniform Call Syntax Translation]
        Application.Prefix(
          translateExpression(name),
          (target :: validArguments).map(translateCallArgument),
          hasDefaultsSuspended = hasDefaultsSuspended,
          getIdentifiedLocation(inputAST)
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
          getIdentifiedLocation(inputAST)
        )
      case AST.App.any(inputAST)     => translateApplicationLike(inputAST)
      case AST.Mixfix.any(inputAST)  => translateApplicationLike(inputAST)
      case AST.Literal.any(inputAST) => translateLiteral(inputAST)
      case AST.Group.any(inputAST)   => translateGroup(inputAST)
      case AST.Ident.any(inputAST)   => translateIdent(inputAST)
      case AST.SequenceLiteral.any(inputAST) =>
        translateSequenceLiteral(inputAST)
      case AstView.Block(lines, retLine) =>
        Expression.Block(
          lines.map(translateExpression),
          translateExpression(retLine),
          location = getIdentifiedLocation(inputAST)
        )
      case AST.Comment.any(inputAST) => translateComment(inputAST)
      case AST.Invalid.any(inputAST) => translateInvalid(inputAST)
      case AST.Foreign(_, _, _) =>
        Error.Syntax(
          inputAST,
          Error.Syntax.UnsupportedSyntax("foreign blocks")
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
          Error.Syntax(
            literal,
            Error.Syntax.UnsupportedSyntax("non-base-10 number literals")
          )
        } else {
          Literal.Number(number, getIdentifiedLocation(literal))
        }
      }
      case AST.Literal.Text.any(literal) =>
        literal.shape match {
          case AST.Literal.Text.Line.Raw(segments) =>
            val fullString = segments.collect {
              case AST.Literal.Text.Segment.Plain(str)   => str
              case AST.Literal.Text.Segment.RawEsc(code) => code.repr
            }.mkString

            Literal.Text(fullString, getIdentifiedLocation(literal))
          case AST.Literal.Text.Block.Raw(lines, _, _) =>
            val fullString = lines
              .map(t =>
                t.text.collect {
                  case AST.Literal.Text.Segment.Plain(str)   => str
                  case AST.Literal.Text.Segment.RawEsc(code) => code.repr
                }.mkString
              )
              .mkString("\n")

            Literal.Text(fullString, getIdentifiedLocation(literal))
          case AST.Literal.Text.Block.Fmt(_, _, _) =>
            Error.Syntax(
              literal,
              Error.Syntax.UnsupportedSyntax("format strings")
            )
          case AST.Literal.Text.Line.Fmt(_) =>
            Error.Syntax(
              literal,
              Error.Syntax.UnsupportedSyntax("format strings")
            )
          case _ =>
            throw new UnhandledEntity(literal.shape, "translateLiteral")
        }
      case _ => throw new UnhandledEntity(literal, "processLiteral")
    }
  }

  /**
    * Translates a sequence literal into its [[Core]] counterpart.
    * @param literal the literal to translate
    * @return the [[Core]] representation of `literal`
    */
  def translateSequenceLiteral(literal: AST.SequenceLiteral): Expression = {
    IR.Application.Literal.Sequence(
      literal.items.map(translateExpression),
      getIdentifiedLocation(literal)
    )
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
        translateIdent(name) match {
          case name: IR.Name =>
            DefinitionArgument.Specified(
              name,
              Some(translateExpression(value)),
              suspended = true,
              getIdentifiedLocation(arg)
            )
          case _ =>
            throw new UnhandledEntity(arg, "translateArgumentDefinition")
        }
      case AstView.LazyArgument(arg) =>
        translateArgumentDefinition(arg, isSuspended = true)
      case AstView.DefinitionArgument(arg) =>
        translateIdent(arg) match {
          case name: IR.Name =>
            DefinitionArgument.Specified(
              name,
              None,
              isSuspended,
              getIdentifiedLocation(arg)
            )
          case _ =>
            throw new UnhandledEntity(arg, "translateArgumentDefinition")
        }
      case AstView.AssignedArgument(name, value) =>
        translateIdent(name) match {
          case name: IR.Name =>
            DefinitionArgument.Specified(
              name,
              Some(translateExpression(value)),
              isSuspended,
              getIdentifiedLocation(arg)
            )
          case _ =>
            throw new UnhandledEntity(arg, "translateArgumentDefinition")
        }
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
          Some(Name.Literal(left.name, getIdentifiedLocation(left))),
          translateExpression(right),
          getIdentifiedLocation(arg)
        )
    case _ =>
      CallArgument
        .Specified(None, translateExpression(arg), getIdentifiedLocation(arg))
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
          getIdentifiedLocation(callable)
        )
      case AstView.Application(name, args) =>
        val (validArguments, hasDefaultsSuspended) =
          calculateDefaultsSuspension(args)

        Application.Prefix(
          translateExpression(name),
          validArguments.map(translateCallArgument),
          hasDefaultsSuspended,
          getIdentifiedLocation(callable)
        )
      case AstView.Lambda(args, body) =>
        if (args.length > 1) {
          Error.Syntax(
            args(1),
            Error.Syntax.UnsupportedSyntax(
              "pattern matching function arguments"
            )
          )
        } else {
          val realArgs = args.map(translateArgumentDefinition(_))
          val realBody = translateExpression(body)
          Function.Lambda(realArgs, realBody, getIdentifiedLocation(callable))
        }
      case AST.App.Infix(left, fn, right) =>
        val leftArg  = translateCallArgument(left)
        val rightArg = translateCallArgument(right)

        if (leftArg.name.isDefined) {
          IR.Error.Syntax(left, IR.Error.Syntax.NamedArgInOperator)
        } else if (rightArg.name.isDefined) {
          IR.Error.Syntax(right, IR.Error.Syntax.NamedArgInOperator)
        } else {
          Application.Operator.Binary(
            leftArg,
            Name.Literal(fn.name, getIdentifiedLocation(fn)),
            rightArg,
            getIdentifiedLocation(callable)
          )
        }
      case AST.App.Prefix(_, _) =>
        throw new UnhandledEntity(callable, "translateCallable")
      case AST.App.Section.any(sec) => translateOperatorSection(sec)
      case AST.Mixfix(nameSegments, args) =>
        val realNameSegments = nameSegments.collect {
          case AST.Ident.Var.any(v)  => v.name
          case AST.Ident.Cons.any(v) => v.name.toLowerCase
        }

        val functionName =
          AST.Ident.Var(realNameSegments.mkString("_"))

        Application.Prefix(
          translateExpression(functionName),
          args.map(translateCallArgument).toList,
          hasDefaultsSuspended = false,
          getIdentifiedLocation(callable)
        )
      case _ => throw new UnhandledEntity(callable, "translateCallable")
    }
  }

  /** Translates an operator section from its [[AST]] representation into the
    * [[IR]] representation.
    *
    * @param section the operator section
    * @return the [[IR]] representation of `section`
    */
  def translateOperatorSection(
    section: AST.App.Section
  ): Expression = {
    section match {
      case AST.App.Section.Left.any(left) =>
        val leftArg = translateCallArgument(left.arg)

        if (leftArg.name.isDefined) {
          Error.Syntax(section, Error.Syntax.NamedArgInSection)
        } else {
          Application.Operator.Section.Left(
            leftArg,
            Name.Literal(left.opr.name, getIdentifiedLocation(left.opr)),
            getIdentifiedLocation(left)
          )
        }
      case AST.App.Section.Sides.any(sides) =>
        Application.Operator.Section.Sides(
          Name.Literal(sides.opr.name, getIdentifiedLocation(sides.opr)),
          getIdentifiedLocation(sides)
        )
      case AST.App.Section.Right.any(right) =>
        val rightArg = translateCallArgument(right.arg)

        if (rightArg.name.isDefined) {
          Error.Syntax(section, Error.Syntax.NamedArgInSection)
        } else {
          Application.Operator.Section.Right(
            Name.Literal(right.opr.name, getIdentifiedLocation(right.opr)),
            translateCallArgument(right.arg),
            getIdentifiedLocation(right)
          )
        }
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
          Name.This(getIdentifiedLocation(identifier))
        } else if (name == "here") {
          Name.Here(getIdentifiedLocation(identifier))
        } else {
          Name.Literal(name, getIdentifiedLocation(identifier))
        }
      case AST.Ident.Cons(name) =>
        Name.Literal(name, getIdentifiedLocation(identifier))
      case AST.Ident.Blank(_) =>
        Name.Blank(getIdentifiedLocation(identifier))
      case AST.Ident.Opr.any(_) =>
        Error.Syntax(
          identifier,
          Error.Syntax.UnsupportedSyntax("operator sections")
        )
      case AST.Ident.Mod(_) =>
        Error.Syntax(
          identifier,
          Error.Syntax.UnsupportedSyntax("module identifiers")
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
    location: Option[IdentifiedLocation],
    name: AST,
    expr: AST
  ): Expression.Binding = {
    val irName = translateExpression(name)

    irName match {
      case n: IR.Name =>
        Expression.Binding(n, translateExpression(expr), location)
      case _ =>
        throw new UnhandledEntity(name, "translateBinding")
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
            getIdentifiedLocation(body),
            canBeTCO = false
          ),
          getIdentifiedLocation(branch)
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
          getIdentifiedLocation(body),
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
    * @return the [[Core]] representation of the contents of `group`
    */
  def translateGroup(group: AST.Group): Expression = {
    group.body match {
      case Some(ast) => translateExpression(ast)
      case None      => Error.Syntax(group, Error.Syntax.EmptyParentheses)
    }
  }

  /** Translates an import statement from its [[AST]] representation into
    * [[Core]].
    *
    * @param imp the import to translate
    * @return the [[Core]] representation of `imp`
    */
  def translateImport(imp: AST.Import): Module.Scope.Import.Module = {
    Module.Scope.Import.Module(
      imp.path.map(t => t.name).reduceLeft((l, r) => l + "." + r),
      getIdentifiedLocation(imp)
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
        Error.Syntax(
          invalid,
          Error.Syntax.UnexpectedExpression
        )
      case AST.Invalid.Unrecognized(_) =>
        Error.Syntax(
          invalid,
          Error.Syntax.UnrecognizedToken
        )
      case AST.Ident.InvalidSuffix(_, _) =>
        Error.Syntax(
          invalid,
          Error.Syntax.InvalidSuffix
        )
      case AST.Literal.Text.Unclosed(_) =>
        Error.Syntax(
          invalid,
          Error.Syntax.UnclosedTextLiteral
        )
      case _ =>
        throw new UnhandledEntity(invalid, "translateInvalid")
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
        Error.Syntax(
          comment,
          Error.Syntax.UnsupportedSyntax("comments")
        )
      case AST.Documented(doc, _, ast) =>
        Comment.Documentation(
          translateExpression(ast),
          doc,
          getIdentifiedLocation(comment)
        )
      case _ =>
        throw new UnhandledEntity(comment, "processComment")
    }
  }
}
