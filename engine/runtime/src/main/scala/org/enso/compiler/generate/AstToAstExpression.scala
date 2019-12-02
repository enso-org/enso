package org.enso.compiler.generate

import org.enso.compiler.core
import org.enso.compiler.core._
import org.enso.compiler.exception.UnhandledEntity
import cats.Foldable
import org.enso.compiler.core.IR
import org.enso.compiler.exception.UnhandledEntity
import org.enso.interpreter._
import org.enso.syntax.text.{AST, Debug, Location}
import cats.implicits._

// TODO [AA] Please note that this entire translation is _very_ work-in-progress
//  and is hence quite ugly right now. It will be cleaned up as work progresses,
//  but it was thought best to land in increments where possible.

// TODO [AA} Things that will need to be done at later stages:
//  - Retain information about groups for diagnostics and desugarings

// TODO [Generic]
//  - Suspended blocks
//  - Groups
//  - String literals
//  - Remove evalOld / rewrite benchmarks to new syntax
//  - Translate if-then-else to ifZero for now
//  - Type signatures
//  - Import Tests

/**
  * This is a representation of the raw conversion from the Parser [[AST AST]]
  * to the internal [[IR IR]] used by the static transformation passes.
  */
object AstToAstExpression {

  /**
    * Transforms the input [[AST]] into the compiler's high-level intermediate
    * representation.
    *
    * @param inputAST the AST to transform
    * @return a representation of the program construct represented by
    *         `inputAST` in the compiler's [[IR IR]]
    */
  def translate(inputAST: AST): AstModuleScope = {
//    println(Debug.pretty(inputAST.toString))
//    println("=========================================")

    inputAST match {
      case AST.Module.any(inputAST) => translateModule(inputAST)
      case _ => {
        throw new UnhandledEntity(inputAST, "translate")
      }
    }
  }

  def translateModuleSymbol(inputAST: AST): AstModuleSymbol = {
    inputAST match {
      case AST.Def(consName, args, body) =>
        if (body.isDefined) {
          throw new RuntimeException("Cannot support complex type defs yet!!!!")
        } else {
          AstTypeDef(consName.name, args.map(translateArgumentDefinition(_)))
        }
      case AstView.MethodDefinition(targetPath, name, definition) =>
        val path =
          targetPath.collect { case AST.Ident.Cons(name) => name }.mkString(".")
        val nameStr       = name match { case AST.Ident.Var(name) => name }
        val defExpression = translateExpression(definition)
        val defExpr: AstFunction = defExpression match {
          case fun: AstFunction => fun
          case expr             => AstFunction(expr.location, List(), expr)
        }
        AstMethodDef(path, nameStr, defExpr)
      case _ =>
        throw new UnhandledEntity(inputAST, "translateModuleSymbol")
    }
  }

  def translateLiteral(literal: AST.Literal): AstExpression = {
    literal match {
      case AST.Literal.Number(base, number) => {
        if (base.isDefined && base.get != "10") {
          throw new RuntimeException("Only base 10 is currently supported")
        }

        AstLong(literal.location, number.toLong)
      }
      case AST.Literal.Text.any(literal) =>
        println("===== TEXT =====")
        println(Debug.pretty(literal.toString))

        AstStringLiteral(literal.location, ???)
      case _ => throw new UnhandledEntity(literal, "processLiteral")
    }
  }

  def translateArgumentDefinition(
    arg: AST,
    isSuspended: Boolean = false
  ): AstArgDefinition = {
    arg match {
      case AstView.LazyAssignedArgument(name, value) =>
        AstArgDefinition(
          name.name,
          Some(translateExpression(value)),
          true
        )
      case AstView.LazyArgument(arg) =>
        translateArgumentDefinition(arg, isSuspended = true)
      case AstView.DefinitionArgument(arg) =>
        AstArgDefinition(arg.name, None, isSuspended)
      case AstView.AssignedArgument(name, value) =>
        AstArgDefinition(
          name.name,
          Some(translateExpression(value)),
          isSuspended
        )
      case _ =>
        throw new UnhandledEntity(arg, "translateArgumentDefinition")
    }
  }

  def translateCallArgument(arg: AST): AstCallArg = arg match {
    case AstView.AssignedArgument(left, right) =>
      AstNamedCallArg(left.name, translateExpression(right))
    case _ => AstUnnamedCallArg(translateExpression(arg))
  }

  def translateMethodCall(
    location: Option[Location],
    target: AST,
    ident: AST.Ident,
    args: List[AST]
  ): AstExpression = {
    AstApply(
      location,
      translateExpression(ident),
      (target :: args).map(translateCallArgument),
      false
    )
  }

  def translateCallable(application: AST): AstExpression = {
    application match {
      case AstView.ForcedTerm(term) =>
        AstDesuspend(term.location, translateExpression(term))
      case AstView.Application(name, args) =>
        val validArguments = args.filter {
          case AstView.SuspendDefaultsOperator(_) => false
          case _                                  => true
        }

        val suspendPositions = args.view.zipWithIndex.collect {
          case (AstView.SuspendDefaultsOperator(_), ix) => ix
        }

        val hasDefaultsSuspended = suspendPositions.contains(args.length - 1)

        AstApply(
          application.location,
          translateExpression(name),
          validArguments.map(translateCallArgument),
          hasDefaultsSuspended
        )
      case AstView.Lambda(args, body) =>
        val realArgs = args.map(translateArgumentDefinition(_))
        val realBody = translateExpression(body)
        AstFunction(application.location, realArgs, realBody)
      case AST.App.Infix(left, fn, right) =>
        // FIXME [AA] We should accept all ops when translating to core
        val validInfixOps = List("+", "/", "-", "*", "%")

        if (validInfixOps.contains(fn.name)) {
          AstArithOp(
            application.location,
            fn.name,
            translateExpression(left),
            translateExpression(right)
          )
        } else {
          throw new RuntimeException(
            s"${fn.name} is not currently a valid infix operator"
          )
        }
      //      case AST.App.Prefix(fn, arg) =>
//      case AST.App.Section.any(application) => // TODO [AA] left, sides, right
//      case AST.Mixfix(application) => // TODO [AA] translate if
      case _ => throw new UnhandledEntity(application, "translateCallable")
    }
  }

  def translateIdent(identifier: AST.Ident): AstExpression = {
    identifier match {
//      case AST.Ident.Blank(_) => throw new UnhandledEntity("Blank") IR.Identifier.Blank()
      case AST.Ident.Var(name)  => AstVariable(identifier.location, name)
      case AST.Ident.Cons(name) => AstVariable(identifier.location, name)
//      case AST.Ident.Opr.any(identifier) => processIdentOperator(identifier)
//      case AST.Ident.Mod(name) => IR.Identifier.Module(name)
      case _ =>
        throw new UnhandledEntity(identifier, "translateIdent")
    }
  }

  def translateAssignment(
    location: Option[Location],
    name: AST,
    expr: AST
  ): AstAssignment = {
    name match {
      case AST.Ident.Var(name) =>
        AstAssignment(location, name, translateExpression(expr))
      case _ =>
        throw new UnhandledEntity(name, "translateAssignment")
    }
  }

  def translateCaseBranch(branch: AST): AstCase = {
    branch match {
      case AstView.ConsCaseBranch(cons, args, body) =>
        AstCase(
          branch.location,
          translateExpression(cons),
          AstCaseFunction(
            body.location,
            args.map(translateArgumentDefinition(_)),
            translateExpression(body)
          )
        )

      case _ => throw new UnhandledEntity(branch, "translateCaseBranch")
    }
  }

  def translateFallbackBranch(branch: AST): AstCaseFunction = {
    branch match {
      case AstView.FallbackCaseBranch(body) =>
        AstCaseFunction(body.location, List(), translateExpression(body))
      case _ => throw new UnhandledEntity(branch, "translateFallbackBranch")
    }
  }

  def translateExpression(inputAST: AST): AstExpression = {
    inputAST match {
      case AstView.Assignment(name, expr) =>
        translateAssignment(inputAST.location, name, expr)
      case AstView.MethodCall(target, name, args) =>
        translateMethodCall(inputAST.location, target, name, args)
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
        AstMatch(
          inputAST.location,
          actualScrutinee,
          nonFallbackBranches,
          potentialFallback
        )
      case AST.App.any(inputAST)     => translateCallable(inputAST)
      case AST.Literal.any(inputAST) => translateLiteral(inputAST)
      case AST.Group.any(inputAST)   => translateGroup(inputAST)
      case AST.Ident.any(inputAST) => translateIdent(inputAST)
      case AstView.Block(lines, retLine) =>
        AstBlock(
          inputAST.location,
          lines.map(translateExpression),
          translateExpression(retLine)
        )
      case _ =>
        throw new UnhandledEntity(inputAST, "translateExpression")
    }
    //    inputAST match {
    //      case AST.Comment.any(inputAST) => processComment(inputAST)
    //      case AST.Ident.any(inputAST)   => processIdent(inputAST)
    //      case AST.Import.any(inputAST)  => processBinding(inputAST)
    //      case AST.Invalid.any(inputAST) => processInvalid(inputAST)
    //      case AST.Mixfix.any(inputAST)  => processApplication(inputAST)
    //      case AST.Def.any(inputAST)     => processBinding(inputAST)
    //      case AST.Foreign.any(inputAST) => processBlock(inputAST)
    //      case _ =>
    //        IR.Error.UnhandledAST(inputAST)
    //    }
  }

  def translateGroup(group: AST.Group): AstExpression = {
    group.body match {
      case Some(ast) => translateExpression(ast)
      case None => {
        // FIXME [AA] This should generate an error node in core
        throw new RuntimeException("Empty group")
      }
    }
  }

  // TODO [AA] Fix the types
  def translateModule(module: AST.Module): AstModuleScope = {
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

        val definitions = nonImportBlocks.takeWhile {
          case AST.Def(_, _, _)                  => true
          case AstView.MethodDefinition(_, _, _) => true
          case _                                 => false
        }

        val executableExpressions = nonImportBlocks.drop(definitions.length)

        val statements  = definitions.map(translateModuleSymbol)
        val expressions = executableExpressions.map(translateExpression)
        val block = expressions match {
          case List()     => None
          case List(expr) => Some(expr)
          case _ =>
            Some(
              AstBlock(
                Foldable[List].foldMap(expressions)(_.location),
                expressions.dropRight(1),
                expressions.last
              )
            )
        }
        core.AstModuleScope(imports, statements, block)
      }
    }
  }

  def translateImport(imp: AST.Import): AstImport = {
    AstImport(imp.path.map(t => t.name).reduceLeft((l, r) => l + "." + r))
  }

  /**
    * Transforms invalid entities from the parser AST.
    *
    * @param invalid the invalid entity
    * @return a representation of `invalid` in the compiler's [[IR IR]]
    */
  def processInvalid(invalid: AST.Invalid): IR.Error = {
    ???
//    invalid match {
//      case AST.Invalid.Unexpected(str, unexpectedTokens) =>
//        IR.Error.UnexpectedToken(str, unexpectedTokens.map(t => process(t.el)))
//      case AST.Invalid.Unrecognized(str) => IR.Error.UnrecognisedSymbol(str)
//      case AST.Ident.InvalidSuffix(identifier, suffix) =>
//        IR.Error.InvalidSuffix(processIdent(identifier), suffix)
//      case AST.Literal.Text.Unclosed(AST.Literal.Text.Line.Raw(text)) =>
//        IR.Error.UnclosedText(List(processLine(text)))
//      case AST.Literal.Text.Unclosed(AST.Literal.Text.Line.Fmt(text)) =>
//        IR.Error.UnclosedText(List(processLine(text)))
//      case _ =>
//        throw new RuntimeException(
//          "Fatal: Unhandled entity in processInvalid = " + invalid
//        )
//    }
  }

  /**
    * Transforms identifiers from the parser AST.
    *
    * @param identifier the identifier
    * @return a representation of `identifier` in the compiler's [[IR IR]]
    */
  def processIdent(identifier: AST.Ident): IR.Identifier = {
    ???
//    identifier match {
//      case AST.Ident.Blank(_)             => IR.Identifier.Blank()
//      case AST.Ident.Var(name)            => IR.Identifier.Variable(name)
//      case AST.Ident.Cons.any(identifier) => processIdentConstructor(identifier)
//      case AST.Ident.Opr.any(identifier)  => processIdentOperator(identifier)
//      case AST.Ident.Mod(name)            => IR.Identifier.Module(name)
//      case _ =>
//        throw new RuntimeException(
//          "Fatal: Unhandled entity in processIdent = " + identifier
//        )
//    }
  }

  /**
    * Transforms an operator identifier from the parser AST.
    *
    * @param operator the operator to transform
    * @return a representation of `operator` in the compiler's [[IR IR]]
    */
  def processIdentOperator(
    operator: AST.Ident.Opr
  ): IR.Identifier.Operator = {
    ???
//    IR.Identifier.Operator(operator.name)
  }

  /**
    * Transforms a constructor identifier from the parser AST.
    *
    * @param constructor the constructor name to transform
    * @return a representation of `constructor` in the compiler's [[IR IR]]
    */
  def processIdentConstructor(
    constructor: AST.Ident.Cons
  ): IR.Identifier.Constructor = {
    ???
//    IR.Identifier.Constructor(constructor.name)
  }

  /**
    * Transforms a line of a text literal from the parser AST.
    *
    * @param line the literal line to transform
    * @return a representation of `line` in the compiler's [[IR IR]]
    */
  def processLine(
    line: List[AST.Literal.Text.Segment[AST]]
  ): IR.Literal.Text.Line = {
    ???
//    IR.Literal.Text.Line(line.map(processTextSegment))
  }

  /**
    * Transforms a segment of text from the parser AST.
    *
    * @param segment the text segment to transform
    * @return a representation of `segment` in the compiler's [[IR IR]]
    */
  def processTextSegment(
    segment: AST.Literal.Text.Segment[AST]
  ): IR.Literal.Text.Segment = {
    ???
//    segment match {
//      case AST.Literal.Text.Segment._Plain(str) =>
//        IR.Literal.Text.Segment.Plain(str)
//      case AST.Literal.Text.Segment._Expr(expr) =>
//        IR.Literal.Text.Segment.Expression(expr.map(process))
//      case AST.Literal.Text.Segment._Escape(code) =>
//        IR.Literal.Text.Segment.EscapeCode(code)
//      case _ => throw new UnhandledEntity(segment, "processTextSegment")
//    }
  }

  /**
    * Transforms a function application from the parser AST.
    *
    * @param application the function application to transform
    * @return a representation of `application` in the compiler's [[IR IR]]
    */
  def processApplication(application: AST): IR.Application = {
    ???
//    application match {
//      case AST.App.Prefix(fn, arg) =>
//        IR.Application.Prefix(process(fn), process(arg))
//      case AST.App.Infix(leftArg, fn, rightArg) =>
//        IR.Application.Infix(
//          process(leftArg),
//          processIdentOperator(fn),
//          process(rightArg)
//        )
//      case AST.App.Section.Left(arg, fn) =>
//        IR.Application.Section.Left(process(arg), processIdentOperator(fn))
//      case AST.App.Section.Right(fn, arg) =>
//        IR.Application.Section.Right(processIdentOperator(fn), process(arg))
//      case AST.App.Section.Sides(fn) =>
//        IR.Application.Section.Sides(processIdentOperator(fn))
//      case AST.Mixfix(fnSegments, args) =>
//        IR.Application
//          .Mixfix(fnSegments.toList.map(processIdent), args.toList.map(process))
//      case _ =>
//        throw new UnhandledEntity(application, "processApplication")
//    }
  }

  /**
    * Transforms a source code block from the parser AST.
    *
    * This handles both blocks of Enso-native code, and blocks of foreign
    * language code.
    *
    * @param block the block to transform
    * @return a representation of `block` in the compiler's [[IR IR]]
    */
  def processBlock(block: AST): IR.Block = {
    ???
//    block match {
//      case AST.Block(_, _, firstLine, lines) =>
//        IR.Block
//          .Enso(
//            process(firstLine.elem) ::
//              lines.filter(t => t.elem.isDefined).map(t => process(t.elem.get))
//          )
//      case AST.Foreign(_, language, code) => IR.Block.Foreign(language, code)
//      case _                              => throw new UnhandledEntity(block, "processBlock")
//    }
  }

  /**
    * Transforms a module top-level from the parser AST.
    *
    * @param module the module to transform
    * @return a representation of `module` in the compiler's [[IR IR]]
    */
  def processModule(module: AST.Module): IR.Module = {
    ???
//    module match {
//      case AST.Module(lines) =>
//        IR.Module(
//          lines.filter(t => t.elem.isDefined).map(t => process(t.elem.get))
//        )
//      case _ => throw new UnhandledEntity(module, "processModule")
//    }
  }

  /**
    * Transforms a comment from the parser AST.
    *
    * @param comment the comment to transform
    * @return a representation of `comment` in the compiler's [[IR IR]]
    */
  def processComment(comment: AST): IR.Comment = {
    ???
//    comment match {
//      case AST.Comment(lines) => IR.Comment(lines)
//      case _                  => throw new UnhandledEntity(comment, "processComment")
//    }
  }

  /**
    * Transforms a binding from the parser AST.
    *
    * Bindings are any constructs that give some Enso language construct a name.
    * This includes type definitions, imports, assignments, and so on.
    *
    * @param binding the binding to transform
    * @return a representation of `binding` in the compiler's [[IR IR]]
    */
  def processBinding(binding: AST): IR.Binding = {
    ???
//    binding match {
//      case AST.Def(constructor, arguments, optBody) =>
//        IR.Binding.RawType(
//          processIdentConstructor(constructor),
//          arguments.map(process),
//          optBody.map(process)
//        )
//      case AST.Import(components) => {
//        IR.Binding.Import(
//          components.toList.map(t => processIdentConstructor(t))
//        )
//      }
//      case _ => throw new UnhandledEntity(binding, "processBinding")
//    }
  }
}
