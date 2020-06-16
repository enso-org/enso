package org.enso.compiler.codegen

import cats.Foldable
import cats.implicits._
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Name.MethodReference
import org.enso.compiler.core.IR._
import org.enso.compiler.exception.UnhandledEntity
import org.enso.syntax.text.AST

import scala.annotation.tailrec

/**
  * This file contains the functionality that translates from the parser's
  * [[AST]] type to the internal representation used by the compiler.
  *
  * The current internal representation is [[IR]].
  */
object AstToIr {
  private def getIdentifiedLocation(ast: AST): Option[IdentifiedLocation] =
    ast.location.map(IdentifiedLocation(_, ast.id))

  /** Translates a program represented in the parser [[AST]] to the compiler's
    * [[IR]].
    *
    * @param inputAST the [[AST]] representing the program to translate
    * @return the [[IR]] representation of `inputAST`
    */
  def translate(inputAST: AST): Module = {
    inputAST match {
      case AST.Module.any(inputAST) => translateModule(inputAST)
      case _ =>
        throw new UnhandledEntity(inputAST, "translate")
    }
  }

  /** Translates an inline program expression represented in the parser [[AST]]
    * into the compiler's [[IR]] representation.
    *
    * Inline expressions must _only_ be expressions, and may not contain any
    * type of definition.
    *
    * @param inputAST the [[AST]] representing the expression to translate.
    * @return the [[IR]] representation of `inputAST` if it is valid, otherwise
    *         [[None]]
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

  /** Translate a top-level Enso module into [[IR]].
    *
    * @param module the [[AST]] representation of the module to translate
    * @return the [[IR]] representation of `module`
    */
  def translateModule(module: AST.Module): Module = {
    module match {
      case AST.Module(blocks) =>
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

  /** Translates a module-level definition from its [[AST]] representation into
    * [[IR]].
    *
    * @param inputAst the definition to be translated
    * @return the [[IR]] representation of `inputAST`
    */
  def translateModuleSymbol(inputAst: AST): Module.Scope.Definition = {
    inputAst match {
      case AstView.Atom(consName, args) =>
        Module.Scope.Definition
          .Atom(
            Name.Literal(consName.name, getIdentifiedLocation(consName)),
            args.map(translateArgumentDefinition(_)),
            getIdentifiedLocation(inputAst)
          )
      case AstView.TypeDef(typeName, args, body) =>
        val translatedBody = translateTypeBody(body)
        val containsAtomDefOrInclude = translatedBody.exists {
          case _: IR.Module.Scope.Definition.Atom => true
          case _: IR.Name.Literal                 => true
          case _                                  => false
        }
        val hasArgs = args.nonEmpty

        if (containsAtomDefOrInclude && !hasArgs) {
          Module.Scope.Definition.Type(
            Name.Literal(typeName.name, getIdentifiedLocation(typeName)),
            args.map(translateArgumentDefinition(_)),
            translatedBody,
            getIdentifiedLocation(inputAst)
          )
        } else if (!containsAtomDefOrInclude) {
          Error.Syntax(inputAst, Error.Syntax.InterfaceDefinition)
        } else {
          Error.Syntax(inputAst, Error.Syntax.InvalidTypeDefinition)
        }
      case AstView.MethodDefinition(targetPath, name, args, definition) =>
        val nameStr = name match { case AST.Ident.Var.any(name) => name }

        val methodRef = if (targetPath.nonEmpty) {
          val pathSegments = targetPath.collect {
            case AST.Ident.Cons.any(c) => c
          }
          val pathNames = pathSegments.map(c =>
            IR.Name.Literal(c.name, getIdentifiedLocation(c))
          )

          val methodSegments = pathNames :+ Name.Literal(
              nameStr.name,
              getIdentifiedLocation(nameStr)
            )

          Name.MethodReference(
            methodSegments.init,
            methodSegments.last,
            MethodReference.genLocation(methodSegments)
          )
        } else {
          val methodSegments = List(
            Name.Here(None),
            Name.Literal(nameStr.name, getIdentifiedLocation(nameStr))
          )

          Name.MethodReference(
            List(methodSegments.head),
            methodSegments.last,
            MethodReference.genLocation(methodSegments)
          )
        }

        Module.Scope.Definition.Method.Binding(
          methodRef,
          args.map(translateArgumentDefinition(_)),
          translateExpression(definition),
          getIdentifiedLocation(inputAst)
        )
      case AstView.FunctionSugar(name, args, body) =>
        val methodSegments = List(
          Name.Here(None),
          Name.Literal(name.name, getIdentifiedLocation(name))
        )
        val methodReference = Name.MethodReference(
          List(methodSegments.head),
          methodSegments.last,
          MethodReference.genLocation(methodSegments)
        )

        Module.Scope.Definition.Method.Binding(
          methodReference,
          args.map(translateArgumentDefinition(_)),
          translateExpression(body),
          getIdentifiedLocation(inputAst)
        )
      case AST.Comment.any(comment) => translateComment(comment)
      case AstView.TypeAscription(typed, sig) =>
        typed match {
          case AST.Ident.any(ident) =>
            val methodSegments = List(
              Name.Here(None),
              Name.Literal(ident.name, getIdentifiedLocation(ident))
            )
            val methodReference = Name.MethodReference(
              List(methodSegments.head),
              methodSegments.last,
              MethodReference.genLocation(methodSegments)
            )

            IR.Type.Ascription(
              methodReference,
              translateExpression(sig),
              getIdentifiedLocation(inputAst)
            )
          case AstView.MethodReference(_, _) =>
            IR.Type.Ascription(
              translateMethodReference(typed),
              translateExpression(sig),
              getIdentifiedLocation(inputAst)
            )
          case _ => Error.Syntax(typed, Error.Syntax.InvalidStandaloneSignature)
        }
      case _ =>
        throw new UnhandledEntity(inputAst, "translateModuleSymbol")
    }
  }

  /** Translates the body of a type expression.
    *
    * @param body the body to be translated
    * @return the [[IR]] representation of `body`
    */
  def translateTypeBody(body: AST): List[IR] = {
    body match {
      case AST.Block.any(block) =>
        val actualLines: List[AST] =
          block.firstLine.elem :: block.lines.flatMap(_.elem)

        if (actualLines.nonEmpty) {
          actualLines.map(translateTypeBodyExpression)
        } else {
          List(Error.Syntax(body, Error.Syntax.InvalidTypeDefinition))
        }
      case _ => List(Error.Syntax(body, Error.Syntax.InvalidTypeDefinition))
    }
  }

  /** Translates any expression that can be found in the body of a type
    * declaration from [[AST]] into [[IR]].
    *
    * @param maybeParensedInput the expression to be translated
    * @return the [[IR]] representation of `maybeParensedInput`
    */
  def translateTypeBodyExpression(maybeParensedInput: AST): IR = {
    val inputAst = AstView.MaybeParensed
      .unapply(maybeParensedInput)
      .getOrElse(maybeParensedInput)

    inputAst match {
      case AST.Ident.Cons.any(include)         => translateIdent(include)
      case atom @ AstView.Atom(_, _)           => translateModuleSymbol(atom)
      case fs @ AstView.FunctionSugar(_, _, _) => translateExpression(fs)
      case AST.Comment.any(inputAST)           => translateComment(inputAST)
      case AstView.TypeAscription(typed, sig) =>
        IR.Type.Ascription(
          translateExpression(typed),
          translateExpression(sig),
          getIdentifiedLocation(inputAst)
        )
      case assignment @ AstView.BasicAssignment(_, _) =>
        translateExpression(assignment)
      case _ =>
        IR.Error.Syntax(inputAst, IR.Error.Syntax.UnexpectedDeclarationInType)
    }
  }

  /** Translates a method reference from [[AST]] into [[IR]].
    *
    * @param inputAst the method reference to translate
    * @return the [[IR]] representation of `inputAst`
    */
  def translateMethodReference(inputAst: AST): IR.Name.MethodReference = {
    inputAst match {
      case AstView.MethodReference(path, methodName) =>
        IR.Name.MethodReference(
          path.map(translateExpression(_).asInstanceOf[IR.Name]),
          translateExpression(methodName).asInstanceOf[IR.Name],
          getIdentifiedLocation(inputAst)
        )
      case _ => throw new UnhandledEntity(inputAst, "translateMethodReference")
    }
  }

  /** Translates an arbitrary program expression from [[AST]] into [[IR]].
    *
    * @param maybeParensedInput the expresion to be translated
    * @return the [[IR]] representation of `maybeParensedInput`
    */
  def translateExpression(maybeParensedInput: AST): Expression = {
    val inputAst = AstView.MaybeParensed
      .unapply(maybeParensedInput)
      .getOrElse(maybeParensedInput)

    inputAst match {
      case AST.Def(consName, _, _) =>
        IR.Error
          .Syntax(inputAst, IR.Error.Syntax.TypeDefinedInline(consName.name))
      case AstView.UnaryMinus(expression) =>
        expression match {
          case AST.Literal.Number(base, number) =>
            translateExpression(
              AST.Literal
                .Number(base, s"-$number")
                .setLocation(inputAst.location)
            )
          case _ =>
            IR.Application.Prefix(
              IR.Name.Literal("negate", None),
              List(
                IR.CallArgument.Specified(
                  None,
                  translateExpression(expression),
                  getIdentifiedLocation(expression)
                )
              ),
              hasDefaultsSuspended = false,
              getIdentifiedLocation(inputAst)
            )
        }
      case AstView.FunctionSugar(name, args, body) =>
        Function.Binding(
          translateIdent(name).asInstanceOf[IR.Name.Literal],
          args.map(translateArgumentDefinition(_)),
          translateExpression(body),
          getIdentifiedLocation(inputAst)
        )
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
          getIdentifiedLocation(inputAst)
        )
      case AstView.BasicAssignment(name, expr) =>
        translateBinding(getIdentifiedLocation(inputAst), name, expr)
      case AstView.MethodDefinition(_, name, _, _) =>
        IR.Error.Syntax(
          inputAst,
          IR.Error.Syntax.MethodDefinedInline(name.asInstanceOf[AST.Ident].name)
        )
      case AstView.MethodCall(target, name, args) =>
        val (validArguments, hasDefaultsSuspended) =
          calculateDefaultsSuspension(args)

        // Note [Uniform Call Syntax Translation]
        Application.Prefix(
          translateExpression(name),
          (target :: validArguments).map(translateCallArgument),
          hasDefaultsSuspended = hasDefaultsSuspended,
          getIdentifiedLocation(inputAst)
        )
      case AstView.CaseExpression(scrutinee, branches) =>
        val actualScrutinee = translateExpression(scrutinee)
        val allBranches     = branches.map(translateCaseBranch)

        Case.Expr(
          actualScrutinee,
          allBranches,
          getIdentifiedLocation(inputAst)
        )
      case AST.App.any(inputAST)     => translateApplicationLike(inputAST)
      case AST.Mixfix.any(inputAST)  => translateApplicationLike(inputAST)
      case AST.Literal.any(inputAST) => translateLiteral(inputAST)
      case AST.Group.any(inputAST)   => translateGroup(inputAST)
      case AST.Ident.any(inputAST)   => translateIdent(inputAST)
      case AST.TypesetLiteral.any(tSet) =>
        IR.Application.Literal.Typeset(
          tSet.expression.map(translateExpression),
          getIdentifiedLocation(tSet)
        )
      case AST.SequenceLiteral.any(inputAST) =>
        translateSequenceLiteral(inputAST)
      case AstView.Block(lines, retLine) =>
        Expression.Block(
          lines.map(translateExpression),
          translateExpression(retLine),
          location = getIdentifiedLocation(inputAst)
        )
      case AST.Comment.any(inputAST) => translateComment(inputAST)
      case AST.Invalid.any(inputAST) => translateInvalid(inputAST)
      case AST.Foreign(_, _, _) =>
        Error.Syntax(
          inputAst,
          Error.Syntax.UnsupportedSyntax("foreign blocks")
        )
      case AstView.Pattern(_) =>
        Error.Syntax(inputAst, Error.Syntax.InvalidPattern)
      case _ =>
        throw new UnhandledEntity(inputAst, "translateExpression")
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
    * [[IR]].
    *
    * @param literal the literal to translate
    * @return the [[IR]] representation of `literal`
    */
  def translateLiteral(literal: AST.Literal): Expression = {
    literal match {
      case AST.Literal.Number(base, number) =>
        if (base.isDefined && base.get != "10") {
          Error.Syntax(
            literal,
            Error.Syntax.UnsupportedSyntax("non-base-10 number literals")
          )
        } else {
          Literal.Number(number, getIdentifiedLocation(literal))
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
    * Translates a sequence literal into its [[IR]] counterpart.
    * @param literal the literal to translate
    * @return the [[IR]] representation of `literal`
    */
  def translateSequenceLiteral(literal: AST.SequenceLiteral): Expression = {
    IR.Application.Literal.Sequence(
      literal.items.map(translateExpression),
      getIdentifiedLocation(literal)
    )
  }

  /** Translates an argument definition from [[AST]] into [[IR]].
    *
    * @param arg the argument to translate
    * @param isSuspended `true` if the argument is suspended, otherwise `false`
    * @return the [[IR]] representation of `arg`
    */
  @tailrec
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
    * into [[IR]].
    *
    * @param arg the argument to translate
    * @return the [[IR]] representation of `arg`
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
    * application from its [[AST]] representation into [[IR]].
    *
    * @param callable the callable to translate
    * @return the [[IR]] representation of `callable`
    */
  def translateApplicationLike(callable: AST): Expression = {
    callable match {
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

        fn match {
          case AST.Ident.Opr.any(fn) =>
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
          case _ => IR.Error.Syntax(left, IR.Error.Syntax.InvalidOperatorName)
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
    * into [[IR]].
    *
    * @param identifier the identifier to translate
    * @return the [[IR]] representation of `identifier`
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
    * into [[IR]].
    *
    * @param location the source location of the binding
    * @param name the name of the binding being assigned to
    * @param expr the expression being assigned to `name`
    * @return the [[IR]] representation of `expr` being bound to `name`
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
    * into [[IR]].
    *
    * @param branch the case branch to translate
    * @return the [[IR]] representation of `branch`
    */
  def translateCaseBranch(branch: AST): Case.Branch = {
    branch match {
      case AstView.CaseBranch(pattern, expression) =>
        Case.Branch(
          translatePattern(pattern),
          translateExpression(expression),
          getIdentifiedLocation(branch)
        )
      case _ => throw new UnhandledEntity(branch, "translateCaseBranch")
    }
  }

  /** Translates a pattern in a case expression from its [[AST]] representation
    * into [[IR]].
    *
    * @param pattern the case pattern to translate
    * @return
    */
  def translatePattern(pattern: AST): Pattern = {
    AstView.MaybeParensed.unapply(pattern).getOrElse(pattern) match {
      case AstView.ConstructorPattern(cons, fields) =>
        Pattern.Constructor(
          translateIdent(cons).asInstanceOf[IR.Name],
          fields.map(translatePattern),
          getIdentifiedLocation(pattern)
        )
      case AstView.CatchAllPattern(name) =>
        Pattern.Name(
          translateIdent(name).asInstanceOf[IR.Name],
          getIdentifiedLocation(pattern)
        )
      case _ =>
        throw new UnhandledEntity(pattern, "translatePattern")
    }
  }

  /** Translates an arbitrary grouped piece of syntax from its [[AST]]
    * representation into [[IR]].
    *
    * It is currently an error to have an empty group.
    *
    * @param group the group to translate
    * @return the [[IR]] representation of the contents of `group`
    */
  def translateGroup(group: AST.Group): Expression = {
    group.body match {
      case Some(ast) => translateExpression(ast)
      case None      => Error.Syntax(group, Error.Syntax.EmptyParentheses)
    }
  }

  /** Translates an import statement from its [[AST]] representation into
    * [[IR]].
    *
    * @param imp the import to translate
    * @return the [[IR]] representation of `imp`
    */
  def translateImport(imp: AST.Import): Module.Scope.Import = {
    imp.path match {
      case AstView.ModulePath(segments) =>
        IR.Module.Scope.Import.Module(
          segments.map(_.name).mkString("."),
          getIdentifiedLocation(imp.path)
        )
      case _ =>
        IR.Error.Syntax(imp, IR.Error.Syntax.InvalidImport)
    }
  }

  /** Translates an arbitrary invalid expression from the [[AST]] representation
    * of the program into its [[IR]] representation.
    *
    * @param invalid the invalid entity to translate
    * @return the [[IR]] representation of `invalid`
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

  /** Translates a comment from its [[AST]] representation into its [[IR]]
    * representation.
    *
    * Currently this only supports documentation comments, and not standarc
    * types of comments as they can't currently be represented.
    *
    * @param comment the comment to transform
    * @return the [[IR]] representation of `comment`
    */
  def translateComment(comment: AST): Comment = {
    comment match {
      case AST.Comment(lines) =>
        Comment.Documentation(
          lines.mkString("\n"),
          getIdentifiedLocation(comment)
        )
      case _ =>
        throw new UnhandledEntity(comment, "processComment")
    }
  }
}
