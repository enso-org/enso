package org.enso.compiler.generate

import org.enso.compiler.exception.UnhandledEntity
import org.enso.compiler.core.IR
import org.enso.syntax.text.AST

/**
  * This is a representation of the raw conversion from the Parser [[AST AST]]
  * to the internal [[IR IR]] used by the static transformation passes.
  */
object AstToIr {

  /**
    * Transforms the input [[AST]] into the compiler's high-level intermediate
    * representation.
    *
    * @param inputAST the AST to transform
    * @return a representation of the program construct represented by
    *         `inputAST` in the compiler's [[IR IR]]
    */
  def process(inputAST: AST): IR = inputAST match {
    case AST.App.any(inputAST)     => processApplication(inputAST)
    case AST.Block.any(inputAST)   => processBlock(inputAST)
    case AST.Comment.any(inputAST) => processComment(inputAST)
    case AST.Ident.any(inputAST)   => processIdent(inputAST)
    case AST.Import.any(inputAST)  => processBinding(inputAST)
    case AST.Invalid.any(inputAST) => processInvalid(inputAST)
    case AST.Literal.any(inputAST) => processLiteral(inputAST)
    case AST.Mixfix.any(inputAST)  => processApplication(inputAST)
    case AST.Module.any(inputAST)  => processModule(inputAST)
    case AST.Group.any(inputAST)   => processGroup(inputAST)
    case AST.Def.any(inputAST)     => processBinding(inputAST)
    case AST.Foreign.any(inputAST) => processBlock(inputAST)
    case _ =>
      IR.Error.UnhandledAST(inputAST)
  }

  /**
    * Transforms invalid entities from the parser AST.
    *
    * @param invalid the invalid entity
    * @return a representation of `invalid` in the compiler's [[IR IR]]
    */
  def processInvalid(invalid: AST.Invalid): IR.Error = invalid match {
    case AST.Invalid.Unexpected(str, unexpectedTokens) =>
      IR.Error.UnexpectedToken(str, unexpectedTokens.map(t => process(t.el)))
    case AST.Invalid.Unrecognized(str) => IR.Error.UnrecognisedSymbol(str)
    case AST.Ident.InvalidSuffix(identifier, suffix) =>
      IR.Error.InvalidSuffix(processIdent(identifier), suffix)
    case AST.Literal.Text.Unclosed(AST.Literal.Text.Line.Raw(text)) =>
      IR.Error.UnclosedText(List(processLine(text)))
    case AST.Literal.Text.Unclosed(AST.Literal.Text.Line.Fmt(text)) =>
      IR.Error.UnclosedText(List(processLine(text)))
    case _ =>
      throw new RuntimeException(
        "Fatal: Unhandled entity in processInvalid = " + invalid
      )
  }

  /**
    * Transforms identifiers from the parser AST.
    *
    * @param identifier the identifier
    * @return a representation of `identifier` in the compiler's [[IR IR]]
    */
  def processIdent(identifier: AST.Ident): IR.Identifier = identifier match {
    case AST.Ident.Blank(_)             => IR.Identifier.Blank()
    case AST.Ident.Var(name)            => IR.Identifier.Variable(name)
    case AST.Ident.Cons.any(identifier) => processIdentConstructor(identifier)
    case AST.Ident.Opr.any(identifier)  => processIdentOperator(identifier)
    case AST.Ident.Mod(name)            => IR.Identifier.Module(name)
    case _ =>
      throw new RuntimeException(
        "Fatal: Unhandled entity in processIdent = " + identifier
      )
  }

  /**
    * Transforms an operator identifier from the parser AST.
    *
    * @param operator the operator to transform
    * @return a representation of `operator` in the compiler's [[IR IR]]
    */
  def processIdentOperator(
    operator: AST.Ident.Opr
  ): IR.Identifier.Operator = IR.Identifier.Operator(operator.name)

  /**
    * Transforms a constructor identifier from the parser AST.
    *
    * @param constructor the constructor name to transform
    * @return a representation of `constructor` in the compiler's [[IR IR]]
    */
  def processIdentConstructor(
    constructor: AST.Ident.Cons
  ): IR.Identifier.Constructor = IR.Identifier.Constructor(constructor.name)

  /**
    * Transforms a literal from the parser AST.
    *
    * @param literal the literal to transform
    * @return a representation of `literal` in the compiler's [[IR IR]]
    */
  def processLiteral(literal: AST.Literal): IR.Literal = {
    literal match {
      case AST.Literal.Number(base, number) => IR.Literal.Number(number, base)

//      TODO [AA] Handle text properly
//      case AST.Literal.Text.Raw(body) =>
//        IR.Literal.Text.Raw(body.lines.toList.map(processLine))
//
//      case AST.Literal.Text.Line.Fmt(lines) =>
//        IR.Literal.Text.Format(lines.toList.map(processLine))

      case _ => throw new UnhandledEntity(literal, "processLiteral")
    }
  }

  /**
    * Transforms a line of a text literal from the parser AST.
    *
    * @param line the literal line to transform
    * @return a representation of `line` in the compiler's [[IR IR]]
    */
  def processLine(
    line: List[AST.Literal.Text.Segment[AST]]
  ): IR.Literal.Text.Line =
    IR.Literal.Text.Line(line.map(processTextSegment))

  /**
    * Transforms a segment of text from the parser AST.
    *
    * @param segment the text segment to transform
    * @return a representation of `segment` in the compiler's [[IR IR]]
    */
  def processTextSegment(
    segment: AST.Literal.Text.Segment[AST]
  ): IR.Literal.Text.Segment = segment match {
    case AST.Literal.Text.Segment._Plain(str) =>
      IR.Literal.Text.Segment.Plain(str)
    case AST.Literal.Text.Segment._Expr(expr) =>
      IR.Literal.Text.Segment.Expression(expr.map(process))
    case AST.Literal.Text.Segment._Escape(code) =>
      IR.Literal.Text.Segment.EscapeCode(code)
    case _ => throw new UnhandledEntity(segment, "processTextSegment")
  }

  /**
    * Transforms a function application from the parser AST.
    *
    * @param application the function application to transform
    * @return a representation of `application` in the compiler's [[IR IR]]
    */
  def processApplication(application: AST): IR.Application =
    application match {
      case AST.App.Prefix(fn, arg) =>
        IR.Application.Prefix(process(fn), process(arg))
      case AST.App.Infix(leftArg, fn, rightArg) =>
        IR.Application.Infix(
          process(leftArg),
          processIdentOperator(fn),
          process(rightArg)
        )
      case AST.App.Section.Left(arg, fn) =>
        IR.Application.Section.Left(process(arg), processIdentOperator(fn))
      case AST.App.Section.Right(fn, arg) =>
        IR.Application.Section.Right(processIdentOperator(fn), process(arg))
      case AST.App.Section.Sides(fn) =>
        IR.Application.Section.Sides(processIdentOperator(fn))
      case AST.Mixfix(fnSegments, args) =>
        IR.Application
          .Mixfix(fnSegments.toList.map(processIdent), args.toList.map(process))
      case _ =>
        throw new UnhandledEntity(application, "processApplication")
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
  def processBlock(block: AST): IR.Block = block match {
    case AST.Block(_, _, firstLine, lines) =>
      IR.Block
        .Enso(
          process(firstLine.elem) ::
          lines.filter(t => t.elem.isDefined).map(t => process(t.elem.get))
        )
    case AST.Foreign(_, language, code) => IR.Block.Foreign(language, code)
    case _ => throw new UnhandledEntity(block, "processBlock")
  }

  /**
    * Transforms a module top-level from the parser AST.
    *
    * @param module the module to transform
    * @return a representation of `module` in the compiler's [[IR IR]]
    */
  def processModule(module: AST.Module): IR.Module = module match {
    case AST.Module(lines) =>
      IR.Module(
        lines.filter(t => t.elem.isDefined).map(t => process(t.elem.get))
      )
    case _ => throw new UnhandledEntity(module, "processModule")
  }

  /**
    * Transforms a comment from the parser AST.
    *
    * @param comment the comment to transform
    * @return a representation of `comment` in the compiler's [[IR IR]]
    */
  def processComment(comment: AST): IR.Comment = comment match {
    case AST.Comment(lines) => IR.Comment(lines)
    case _ => throw new UnhandledEntity(comment, "processComment")
  }

  /**
    * Transforms a group from the parser AST.
    *
    * In [[IR]], groups are actually non-entities, as all grouping is handled
    * implicitly by the IR format. A valid group is replaced by its contents in
    * the IR, while invalid groups are replaced by error nodes.
    *
    * @param group the group to transform
    * @return a representation of `group` in the compiler's [[IR IR]]
    */
  def processGroup(group: AST): IR = group match {
    case AST.Group(maybeAST) =>
      maybeAST match {
        case Some(ast) => process(ast)
        case None      => IR.Error.EmptyGroup()
      }
    case _ => throw new UnhandledEntity(group, "processGroup")
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
  def processBinding(binding: AST): IR.Binding = binding match {
    case AST.Def(constructor, arguments, optBody) =>
      IR.Binding.RawType(
        processIdentConstructor(constructor),
        arguments.map(process),
        optBody.map(process)
      )
    case AST.Import(components) => {
      IR.Binding.Import(
        components.toList.map(t => processIdentConstructor(t))
      )
    }
    case _ => throw new UnhandledEntity(binding, "processBinding")
  }
}