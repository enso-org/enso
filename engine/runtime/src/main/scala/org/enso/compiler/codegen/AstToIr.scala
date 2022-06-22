package org.enso.compiler.codegen

import java.math.BigInteger
import java.nio.ByteBuffer

import cats.Foldable
import cats.implicits._
import org.enso.interpreter.Constants
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Name.MethodReference
import org.enso.compiler.core.IR._
import org.enso.compiler.exception.UnhandledEntity
import org.enso.interpreter.epb.EpbParser
import org.enso.syntax.text.AST
import org.enso.syntax.text.Shape.{
  SegmentEscape,
  SegmentExpr,
  SegmentPlain,
  SegmentRawEscape,
  TextUnclosed
}
import org.enso.syntax.text.ast.text.Escape
import org.enso.syntax.text.ast.text.Escape.Unicode

import scala.annotation.tailrec
import scala.util.control.Breaks.{break, breakable}

/** This file contains the functionality that translates from the parser's
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
  def translate(inputAST: AST): Module =
    inputAST match {
      case AST.Module.any(inputAST) => translateModule(inputAST)
      case _ =>
        throw new UnhandledEntity(inputAST, "translate")
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

        val expressions = presentBlocks.map(translateExpression(_))

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
            val pkg    = imp.path.init.map(_.name)
            val cls    = imp.path.last.name
            val rename = imp.rename.map(_.name)
            Module.Scope.Import.Polyglot(
              Module.Scope.Import.Polyglot.Java(pkg.mkString("."), cls),
              rename,
              getIdentifiedLocation(imp)
            )
        }

        val exports = presentBlocks.collect { case AST.Export.any(export) =>
          translateExport(export)
        }

        val nonImportBlocks = presentBlocks.filter {
          case AST.Import.any(_)     => false
          case AST.JavaImport.any(_) => false
          case AST.Export.any(_)     => false
          case _                     => true
        }

        val statements = nonImportBlocks.map(translateModuleSymbol)
        Module(imports, exports, statements, getIdentifiedLocation(module))
      case _ => throw new UnhandledEntity(module, "translateModule")
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
      case AST.Ident.Annotation.any(annotation) =>
        IR.Name.Annotation(annotation.name, getIdentifiedLocation(annotation))
      case AstView.Atom(consName, args) =>
        val newArgs = args.map(translateArgumentDefinition(_))

        if (newArgs.exists(_.suspended)) {
          val ast = newArgs
            .zip(args)
            .collect { case (arg, ast) if arg.suspended => ast }
            .head
          Error.Syntax(ast, Error.Syntax.SuspendedArgInAtom)
        } else {
          Module.Scope.Definition.Atom(
            buildName(consName),
            newArgs,
            getIdentifiedLocation(inputAst)
          )
        }
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
            buildName(typeName),
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
        val nameId: AST.Ident = name match {
          case AST.Ident.Var.any(name) => name
          case AST.Ident.Opr.any(opr)  => opr
          case _ =>
            throw new UnhandledEntity(name, "translateModuleSymbol")
        }

        val methodRef = if (targetPath.nonEmpty) {
          val pathSegments = targetPath.collect { case AST.Ident.Cons.any(c) =>
            c
          }
          val pathNames = pathSegments.map(buildName(_))

          val methodSegments = pathNames :+ buildName(nameId)

          val typeSegments = methodSegments.init

          Name.MethodReference(
            IR.Name.Qualified(
              typeSegments,
              MethodReference.genLocation(typeSegments)
            ),
            methodSegments.last,
            MethodReference.genLocation(methodSegments)
          )
        } else {
          val typeName   = Name.Here(None)
          val methodName = buildName(nameId)
          Name.MethodReference(
            typeName,
            methodName,
            methodName.location
          )
        }

        Module.Scope.Definition.Method.Binding(
          methodRef,
          args.map(translateArgumentDefinition(_)),
          translateExpression(definition),
          getIdentifiedLocation(inputAst)
        )
      case AstView.FunctionSugar(
            AST.Ident.Var("foreign"),
            header,
            body
          ) =>
        translateForeignDefinition(header, body) match {
          case Right((name, arguments, body)) =>
            val typeName = Name.Here(None)
            val methodRef =
              Name.MethodReference(typeName, name, name.location)
            Module.Scope.Definition.Method.Binding(
              methodRef,
              arguments,
              body,
              getIdentifiedLocation(inputAst)
            )
          case Left(reason) =>
            IR.Error.Syntax(
              inputAst,
              IR.Error.Syntax.InvalidForeignDefinition(reason)
            )
        }
      case AstView.FunctionSugar(name, args, body) =>
        val typeName   = Name.Here(None)
        val methodName = buildName(name)

        val methodReference = Name.MethodReference(
          typeName,
          methodName,
          methodName.location
        )

        Module.Scope.Definition.Method.Binding(
          methodReference,
          args.map(translateArgumentDefinition(_)),
          translateExpression(body),
          getIdentifiedLocation(inputAst)
        )
      case AST.Comment.any(comment) => translateComment(comment)
      case AstView.TypeAscription(typed, sig) =>
        def buildAscription(ident: AST.Ident): IR.Type.Ascription = {
          val typeName   = Name.Here(None)
          val methodName = buildName(ident)
          val methodReference = Name.MethodReference(
            typeName,
            methodName,
            methodName.location
          )

          IR.Type.Ascription(
            methodReference,
            translateExpression(sig, insideTypeSignature = true),
            getIdentifiedLocation(inputAst)
          )
        }
        typed match {
          case AST.Ident.any(ident)       => buildAscription(ident)
          case AST.App.Section.Sides(opr) => buildAscription(opr)
          case AstView.MethodReference(_, _) =>
            IR.Type.Ascription(
              translateMethodReference(typed),
              translateExpression(sig, insideTypeSignature = true),
              getIdentifiedLocation(inputAst)
            )
          case _ => Error.Syntax(typed, Error.Syntax.InvalidStandaloneSignature)
        }
      case _ => Error.Syntax(inputAst, Error.Syntax.UnexpectedExpression)
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
    val inputAst = AstView.MaybeManyParensed
      .unapply(maybeParensedInput)
      .getOrElse(maybeParensedInput)

    inputAst match {
      case AST.Ident.Annotation.any(ann) =>
        IR.Name.Annotation(ann.name, getIdentifiedLocation(ann))
      case AST.Ident.Cons.any(include) => translateIdent(include)
      case atom @ AstView.Atom(_, _)   => translateModuleSymbol(atom)
      case AstView.FunctionSugar(
            AST.Ident.Var("foreign"),
            header,
            body
          ) =>
        translateForeignDefinition(header, body) match {
          case Right((name, arguments, body)) =>
            IR.Function.Binding(
              name,
              arguments,
              body,
              getIdentifiedLocation(inputAst)
            )
          case Left(reason) =>
            IR.Error.Syntax(
              inputAst,
              IR.Error.Syntax.InvalidForeignDefinition(reason)
            )
        }
      case fs @ AstView.FunctionSugar(_, _, _) => translateExpression(fs)
      case AST.Comment.any(inputAST)           => translateComment(inputAST)
      case AstView.Binding(AST.App.Section.Right(opr, arg), body) =>
        Function.Binding(
          buildName(opr),
          List(translateArgumentDefinition(arg)),
          translateExpression(body),
          getIdentifiedLocation(inputAst)
        )
      case AstView.TypeAscription(typed, sig) =>
        val typedIdent = typed match {
          case AST.App.Section.Sides(opr) => buildName(opr)
          case AST.Ident.any(ident)       => buildName(ident)
          case other                      => translateExpression(other)
        }
        IR.Type.Ascription(
          typedIdent,
          translateExpression(sig, insideTypeSignature = true),
          getIdentifiedLocation(inputAst)
        )
      case assignment @ AstView.BasicAssignment(_, _) =>
        translateExpression(assignment)
      case _ =>
        IR.Error.Syntax(inputAst, IR.Error.Syntax.UnexpectedDeclarationInType)
    }
  }

  private def translateForeignDefinition(header: List[AST], body: AST): Either[
    String,
    (IR.Name, List[IR.DefinitionArgument], IR.Foreign.Definition)
  ] = {
    header match {
      case AST.Ident.Var(lang) :: AST.Ident.Var.any(name) :: args =>
        body.shape match {
          case AST.Literal.Text.Block.Raw(lines, _, _) =>
            val code = lines
              .map(t =>
                t.text.collect {
                  case AST.Literal.Text.Segment.Plain(str)   => str
                  case AST.Literal.Text.Segment.RawEsc(code) => code.repr
                }.mkString
              )
              .mkString("\n")
            val methodName = buildName(name)
            val arguments  = args.map(translateArgumentDefinition(_))
            val language   = EpbParser.ForeignLanguage.getBySyntacticTag(lang)
            if (language == null) {
              Left(s"Language $lang is not a supported polyglot language.")
            } else {
              val foreign = IR.Foreign.Definition(
                language,
                code,
                getIdentifiedLocation(body)
              )
              Right((methodName, arguments, foreign))
            }
          case _ =>
            Left(
              "The body of a foreign block must be an uninterpolated string block literal."
            )
        }
      case _ => Left("The method name is not specified.")
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
        val typeParts = path.map(translateExpression(_).asInstanceOf[IR.Name])
        IR.Name.MethodReference(
          IR.Name.Qualified(typeParts, MethodReference.genLocation(typeParts)),
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
  def translateExpression(
    maybeParensedInput: AST,
    insideTypeSignature: Boolean = false
  ): Expression = {
    val inputAst = AstView.MaybeManyParensed
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
              IR.Name
                .Literal("negate", isReferent = false, isMethod = true, None),
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
          buildName(name),
          Expression.Block(
            lines.map(translateExpression(_)),
            translateExpression(lastLine),
            getIdentifiedLocation(block),
            suspended = true
          ),
          getIdentifiedLocation(inputAst)
        )
      case AstView.BasicAssignment(name, expr) =>
        translateBinding(getIdentifiedLocation(inputAst), name, expr)
      case AstView.TypeAscription(left, right) =>
        IR.Application.Operator.Binary(
          translateCallArgument(left),
          buildName(AST.Ident.Opr(AstView.TypeAscription.operatorName)),
          translateCallArgument(right, insideTypeSignature = true),
          getIdentifiedLocation(inputAst)
        )
      case AstView.MethodDefinition(_, name, _, _) =>
        IR.Error.Syntax(
          inputAst,
          IR.Error.Syntax.MethodDefinedInline(name.asInstanceOf[AST.Ident].name)
        )
      case AstView.MethodCall(target, name, args) =>
        inputAst match {
          case AstView.QualifiedName(idents) if insideTypeSignature =>
            IR.Name.Qualified(
              idents.map(x => translateIdent(x).asInstanceOf[IR.Name]),
              getIdentifiedLocation(inputAst)
            )
          case _ =>
            val (validArguments, hasDefaultsSuspended) =
              calculateDefaultsSuspension(args)

            Application.Prefix(
              buildName(name, isMethod = true),
              (target :: validArguments).map(translateCallArgument(_)),
              hasDefaultsSuspended = hasDefaultsSuspended,
              getIdentifiedLocation(inputAst)
            )
        }
      case AstView.CaseExpression(scrutinee, branches) =>
        val actualScrutinee = translateExpression(scrutinee)
        val allBranches     = branches.map(translateCaseBranch)

        Case.Expr(
          actualScrutinee,
          allBranches,
          getIdentifiedLocation(inputAst)
        )
      case AstView.DecimalLiteral(intPart, fracPart) =>
        translateDecimalLiteral(inputAst, intPart, fracPart)
      case AST.App.any(inputAST) =>
        translateApplicationLike(inputAST, insideTypeSignature)
      case AST.Mixfix.any(inputAST)  => translateApplicationLike(inputAST)
      case AST.Literal.any(inputAST) => translateLiteral(inputAST)
      case AST.Group.any(inputAST)   => translateGroup(inputAST)
      case AST.Ident.any(inputAST)   => translateIdent(inputAST)
      case AST.TypesetLiteral.any(tSet) =>
        IR.Application.Literal.Typeset(
          tSet.expression.map(translateExpression(_)),
          getIdentifiedLocation(tSet)
        )
      case AST.SequenceLiteral.any(inputAST) =>
        translateSequenceLiteral(inputAST)
      case AstView.Block(lines, retLine) =>
        Expression.Block(
          lines.map(translateExpression(_)),
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
      case AST.Macro.Ambiguous(_, _) =>
        Error.Syntax(inputAst, Error.Syntax.AmbiguousExpression)
      case _ =>
        throw new UnhandledEntity(inputAst, "translateExpression")
    }
  }

  def translateDecimalLiteral(
    ast: AST,
    int: AST.Literal.Number,
    frac: AST.Literal.Number
  ): Expression = {
    if (int.base.isDefined && int.base.get != "10") {
      Error.Syntax(
        int,
        Error.Syntax.UnsupportedSyntax("non-base-10 decimal literals")
      )
    } else if (frac.base.isDefined && frac.base.get != "10") {
      Error.Syntax(frac, Error.Syntax.InvalidBaseInDecimalLiteral)
    } else {
      Literal.Number(
        None,
        s"${int.shape.int}.${frac.shape.int}",
        getIdentifiedLocation(ast)
      )
    }
  }

  /** Translates a program literal from its [[AST]] representation into
    * [[IR]].
    *
    * @param literal the literal to translate
    * @return the [[IR]] representation of `literal`
    */
  def translateLiteral(literal: AST.Literal): Expression =
    literal match {
      case AST.Literal.Number(base, number) =>
        if (base.isDefined) {
          val baseNum =
            try { Integer.parseInt(base.get) }
            catch {
              case _: NumberFormatException =>
                return Error.Syntax(
                  literal,
                  Error.Syntax.InvalidBase(base.get)
                )
            }
          try { new BigInteger(number, baseNum) }
          catch {
            case _: NumberFormatException =>
              return Error.Syntax(
                literal,
                Error.Syntax.InvalidNumberForBase(number, base.get)
              )
          }
        }
        Literal.Number(base, number, getIdentifiedLocation(literal))
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
          case AST.Literal.Text.Block.Fmt(lines, _, _) =>
            val ls  = lines.map(l => parseFmtSegments(literal, l.text))
            val err = ls.collectFirst { case Left(e) => e }
            err match {
              case Some(err) => err
              case None =>
                val str = ls.collect { case Right(str) => str }.mkString("\n")
                IR.Literal.Text(str, getIdentifiedLocation(literal))
            }
          case AST.Literal.Text.Line.Fmt(segments) =>
            parseFmtSegments(literal, segments) match {
              case Left(err) => err
              case Right(str) =>
                IR.Literal.Text(str, getIdentifiedLocation(literal))
            }
          case TextUnclosed(_) =>
            Error.Syntax(literal, Error.Syntax.UnclosedTextLiteral)

          case _ =>
            throw new UnhandledEntity(literal.shape, "translateLiteral")
        }
      case _ => throw new UnhandledEntity(literal, "processLiteral")
    }

  private def parseFmtSegments(
    literal: AST,
    segments: Seq[AST.Literal.Text.Segment[AST]]
  ): Either[IR.Error, String] = {
    val bldr                  = new StringBuilder
    var err: Option[IR.Error] = None
    breakable {
      segments.foreach {
        case SegmentEscape(code) =>
          code match {
            case Escape.Number(_) =>
              err = Some(
                Error.Syntax(
                  literal,
                  Error.Syntax.UnsupportedSyntax("escaped numbers")
                )
              )
              break()
            case unicode: Escape.Unicode =>
              unicode match {
                case Unicode.InvalidUnicode(unicode) =>
                  err = Some(
                    Error.Syntax(
                      literal,
                      Error.Syntax.InvalidEscapeSequence(unicode.repr)
                    )
                  )
                  break()
                case Unicode._U16(digits) =>
                  val buffer = ByteBuffer.allocate(2)
                  buffer.putChar(
                    Integer.parseInt(digits, 16).asInstanceOf[Char]
                  )
                  val str = new String(buffer.array(), "UTF-16")
                  bldr.addAll(str)
                case Unicode._U32(digits) =>
                  val buffer = ByteBuffer.allocate(4)
                  buffer.putInt(Integer.parseInt(digits, 16))
                  val str = new String(buffer.array(), "UTF-32")
                  bldr.addAll(str)
                case Unicode._U21(digits) =>
                  val buffer = ByteBuffer.allocate(4)
                  buffer.putInt(Integer.parseInt(digits, 16))
                  val str = new String(buffer.array(), "UTF-32")
                  bldr.addAll(str)
              }
            case e: Escape.Character => bldr.addOne(e.code)
            case e: Escape.Control   => bldr.addAll(e.repr)
          }
        case SegmentPlain(text) => bldr.addAll(text)
        case SegmentExpr(_) =>
          err = Some(
            Error.Syntax(
              literal,
              Error.Syntax.UnsupportedSyntax("interpolated expressions")
            )
          )
          break()
        case SegmentRawEscape(e) => bldr.addAll(e.repr)
      }
    }
    err.map(Left(_)).getOrElse(Right(bldr.toString))
  }

  /** Translates a sequence literal into its [[IR]] counterpart.
    * @param literal the literal to translate
    * @return the [[IR]] representation of `literal`
    */
  def translateSequenceLiteral(literal: AST.SequenceLiteral): Expression = {
    IR.Application.Literal.Sequence(
      literal.items.map(translateExpression(_)),
      getIdentifiedLocation(literal)
    )
  }

  /** Translates an arbitrary expression, making sure to properly recognize
    * qualified names. Qualified names should, probably, at some point be
    * handled deeper in the compiler pipeline.
    */
  private def translateQualifiedNameOrExpression(arg: AST): IR.Expression =
    arg match {
      case AstView.QualifiedName(segments) =>
        IR.Name.Qualified(
          segments.map(buildName(_)),
          getIdentifiedLocation(arg)
        )
      case _ => translateExpression(arg)
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
      case AstView.AscribedArgument(name, ascType, mValue, isSuspended) =>
        translateIdent(name) match {
          case name: IR.Name =>
            DefinitionArgument.Specified(
              name,
              Some(translateQualifiedNameOrExpression(ascType)),
              mValue.map(translateExpression(_)),
              isSuspended,
              getIdentifiedLocation(arg)
            )
          case _ =>
            throw new UnhandledEntity(arg, "translateArgumentDefinition")
        }
      case AstView.LazyAssignedArgumentDefinition(name, value) =>
        translateIdent(name) match {
          case name: IR.Name =>
            DefinitionArgument.Specified(
              name,
              None,
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
              None,
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
  def translateCallArgument(
    arg: AST,
    insideTypeSignature: Boolean = false
  ): CallArgument.Specified =
    arg match {
      case AstView.AssignedArgument(left, right) =>
        CallArgument
          .Specified(
            Some(buildName(left)),
            translateExpression(right, insideTypeSignature),
            getIdentifiedLocation(arg)
          )
      case _ =>
        CallArgument
          .Specified(
            None,
            translateExpression(arg, insideTypeSignature),
            getIdentifiedLocation(arg)
          )
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
  def translateApplicationLike(
    callable: AST,
    insideTypeAscription: Boolean = false
  ): Expression = {
    callable match {
      case AstView.Application(name, args) =>
        val (validArguments, hasDefaultsSuspended) =
          calculateDefaultsSuspension(args)

        val fun = name match {
          case AstView.Method(ast) => buildName(ast, isMethod = true)
          case AstView.Expr(ast) =>
            translateExpression(ast, insideTypeAscription)
        }

        Application.Prefix(
          fun,
          validArguments.map(translateCallArgument(_, insideTypeAscription)),
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
          val realArgs =
            args.map(translateArgumentDefinition(_, insideTypeAscription))
          val realBody = translateExpression(body, insideTypeAscription)
          Function.Lambda(realArgs, realBody, getIdentifiedLocation(callable))
        }
      case AST.App.Infix(left, fn, right) =>
        val leftArg  = translateCallArgument(left, insideTypeAscription)
        val rightArg = translateCallArgument(right, insideTypeAscription)

        fn match {
          case AST.Ident.Opr.any(fn) =>
            if (leftArg.name.isDefined) {
              IR.Error.Syntax(left, IR.Error.Syntax.NamedArgInOperator)
            } else if (rightArg.name.isDefined) {
              IR.Error.Syntax(right, IR.Error.Syntax.NamedArgInOperator)
            } else {
              Application.Operator.Binary(
                leftArg,
                buildName(fn),
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
          buildName(functionName, isMethod = true),
          args.map(translateCallArgument(_, insideTypeAscription)).toList,
          hasDefaultsSuspended = false,
          getIdentifiedLocation(callable)
        )
      case AST.Macro.Ambiguous(_, _) =>
        Error.Syntax(callable, Error.Syntax.AmbiguousExpression)
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
            buildName(left.opr),
            getIdentifiedLocation(left)
          )
        }
      case AST.App.Section.Sides.any(sides) =>
        Application.Operator.Section.Sides(
          buildName(sides.opr),
          getIdentifiedLocation(sides)
        )
      case AST.App.Section
            .Right(AST.Ident.Opr("."), AstView.ConsOrVar(ident)) =>
        buildName(ident, isMethod = true)
      case AST.App.Section.Right.any(right) =>
        val rightArg = translateCallArgument(right.arg)

        if (rightArg.name.isDefined) {
          Error.Syntax(section, Error.Syntax.NamedArgInSection)
        } else {
          Application.Operator.Section.Right(
            buildName(right.opr),
            translateCallArgument(right.arg),
            getIdentifiedLocation(right)
          )
        }
      case _ => throw new UnhandledEntity(section, "translateOperatorSection")
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
        if (name == Constants.Names.SELF_ARGUMENT) {
          Name.Self(getIdentifiedLocation(identifier))
        } else if (name == Constants.Names.CURRENT_MODULE) {
          Name.Here(getIdentifiedLocation(identifier))
        } else {
          buildName(identifier)
        }
      case AST.Ident.Annotation(name) =>
        Name.Annotation(name, getIdentifiedLocation(identifier))
      case AST.Ident.Cons(_) =>
        buildName(identifier)
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
    * into [[IR]], also handling the documentation comments in between branches.
    *
    * The documentation comments are translated to dummy branches that contain
    * an empty expression and a dummy [[IR.Pattern.Documentation]] pattern
    * containing the comment. These dummy branches are removed in the
    * DocumentationComments pass where the comments are attached to the actual
    * branches.
    *
    * @param branch the case branch or comment to translate
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
      case c @ AST.Comment(lines) =>
        val doc      = lines.mkString("\n")
        val location = getIdentifiedLocation(c)
        Case.Branch(
          Pattern.Documentation(doc, location),
          IR.Empty(None),
          location
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
    AstView.MaybeManyParensed.unapply(pattern).getOrElse(pattern) match {
      case AstView.ConstructorPattern(conses, fields) =>
        val irConses = conses.map(translateIdent(_).asInstanceOf[IR.Name])
        val name = irConses match {
          case List(n) => n
          case _       => IR.Name.Qualified(irConses, None)
        }
        Pattern.Constructor(
          name,
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
    imp match {
      case AST.Import(path, rename, isAll, onlyNames, hiddenNames) =>
        IR.Module.Scope.Import.Module(
          IR.Name.Qualified(path.map(buildName(_)).toList, None),
          rename.map(buildName(_)),
          isAll,
          onlyNames.map(_.map(buildName(_)).toList),
          hiddenNames.map(_.map(buildName(_)).toList),
          getIdentifiedLocation(imp)
        )
      case _ =>
        IR.Error.Syntax(imp, IR.Error.Syntax.InvalidImport)
    }
  }

  /** Translates an export statement from its [[AST]] representation into
    * [[IR]].
    *
    * @param exp the export to translate
    * @return the [[IR]] representation of `imp`
    */
  def translateExport(exp: AST.Export): Module.Scope.Export.Module = {
    exp match {
      case AST.Export(path, rename, isAll, onlyNames, hiddenNames) =>
        IR.Module.Scope.Export.Module(
          IR.Name.Qualified(path.map(buildName(_)).toList, None),
          rename.map(buildName(_)),
          isAll,
          onlyNames.map(_.map(buildName(_)).toList),
          hiddenNames.map(_.map(buildName(_)).toList),
          getIdentifiedLocation(exp)
        )
      case _ => throw new UnhandledEntity(exp, "translateExport")
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

  private def isReferant(ident: AST.Ident): Boolean =
    ident match {
      case AST.Ident.Cons.any(_) => true
      case _                     => false
    }

  private def buildName(
    ident: AST.Ident,
    isMethod: Boolean = false
  ): IR.Name.Literal =
    IR.Name.Literal(
      ident.name,
      isReferant(ident),
      isMethod || AST.Opr.any.unapply(ident).isDefined,
      getIdentifiedLocation(ident)
    )
}
