package org.enso.compiler;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Application$Literal$Sequence;
import org.enso.compiler.core.IR$Application$Operator$Binary;
import org.enso.compiler.core.IR$Application$Prefix;
import org.enso.compiler.core.IR$CallArgument$Specified;
import org.enso.compiler.core.IR$Case$Branch;
import org.enso.compiler.core.IR$Case$Expr;
import org.enso.compiler.core.IR$Comment$Documentation;
import org.enso.compiler.core.IR$DefinitionArgument$Specified;
import org.enso.compiler.core.IR$Error$Syntax;
import org.enso.compiler.core.IR$Error$Syntax$InvalidBaseInDecimalLiteral$;
import org.enso.compiler.core.IR$Error$Syntax$UnexpectedDeclarationInType$;
import org.enso.compiler.core.IR$Error$Syntax$UnexpectedExpression$;
import org.enso.compiler.core.IR$Error$Syntax$UnsupportedSyntax;
import org.enso.compiler.core.IR$Expression$Binding;
import org.enso.compiler.core.IR$Expression$Block;
import org.enso.compiler.core.IR$Function$Lambda;
import org.enso.compiler.core.IR$Function$Binding;
import org.enso.compiler.core.IR$Literal$Text;
import org.enso.compiler.core.IR$Literal$Number;
import org.enso.compiler.core.IR$Module$Scope$Definition;
import org.enso.compiler.core.IR$Module$Scope$Definition$Data;
import org.enso.compiler.core.IR$Module$Scope$Definition$Method$Binding;
import org.enso.compiler.core.IR$Module$Scope$Definition$SugaredType;
import org.enso.compiler.core.IR$Module$Scope$Export;
import org.enso.compiler.core.IR$Module$Scope$Export$Module;
import org.enso.compiler.core.IR$Module$Scope$Import;
import org.enso.compiler.core.IR$Module$Scope$Import$Module;
import org.enso.compiler.core.IR$Module$Scope$Import$Polyglot;
import org.enso.compiler.core.IR$Module$Scope$Import$Polyglot$Java;
import org.enso.compiler.core.IR$Name$Annotation;
import org.enso.compiler.core.IR$Name$Blank;
import org.enso.compiler.core.IR$Name$Literal;
import org.enso.compiler.core.IR$Name$MethodReference;
import org.enso.compiler.core.IR$Name$Qualified;
import org.enso.compiler.core.IR$Pattern$Constructor;
import org.enso.compiler.core.IR$Pattern$Name;
import org.enso.compiler.core.IR$Pattern$Literal;
import org.enso.compiler.core.IR$Type$Ascription;
import org.enso.compiler.core.IR$Type$Function;
import org.enso.compiler.core.IR.IdentifiedLocation;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.exception.UnhandledEntity;
import org.enso.syntax.text.Location;
import org.enso.syntax2.ArgumentDefinition;
import org.enso.syntax2.Case;
import org.enso.syntax2.Either;
import org.enso.syntax2.FractionalDigits;
import org.enso.syntax2.Line;
import org.enso.syntax2.MultipleOperatorError;
import org.enso.syntax2.Token;
import org.enso.syntax2.Token.Operator;
import org.enso.syntax2.Tree;

import scala.Option;
import scala.collection.immutable.List;
import scala.jdk.javaapi.CollectionConverters;

final class TreeToIr {
  static final TreeToIr MODULE = new TreeToIr();

  private TreeToIr() {
  }

  /** Translates a program represented in the parser {@link Tree} to the compiler's
    * {@link IR}.
    *
    * @param ast the tree representing the program to translate
    * @return the IR representation of `inputAST`
    */
  IR.Module translate(Tree ast) {
    return translateModule(ast);
  }

  /** Translate a top-level Enso module into [[IR]].
    *
    * @param module the [[AST]] representation of the module to translate
    * @return the [[IR]] representation of `module`
    */
  IR.Module translateModule(Tree module) {
    return switch (module) {
      case Tree.BodyBlock b -> {
        List<IR$Module$Scope$Definition> bindings = nil();
        List<IR$Module$Scope$Import> imports = nil();
        List<IR$Module$Scope$Export> exports = nil();
        for (Line line : b.getStatements()) {
          switch (line.getExpression()) {
            case Tree.Assignment a -> {
              var r = switch (a.getPattern()) {
                case Tree.Ident id -> {
                  var name = new IR$Name$Literal(
                    id.getToken().codeRepr(), false,
                    getIdentifiedLocation(id),
                    meta(), diag()
                  );
                  yield new IR$Name$MethodReference(
                    Option.empty(),
                    name,
                    name.location(),
                    meta(), diag()
                  );
                }
                case Tree.OprApp opr -> {
                  var pointer = buildQualifiedName(opr.getLhs());
                  var name = buildName(opr.getRhs());
                  yield new IR$Name$MethodReference(
                    Option.apply(pointer),
                    name,
                    Option.empty(),
                    meta(), diag()
                  );
                }
                default -> throw new IllegalStateException("Not an identifier: " + a.getPattern());
              };
              var m = new IR$Module$Scope$Definition$Method$Binding(
                r,
                nil(),
                translateExpression(a.getExpr(), false),
                getIdentifiedLocation(module),
                meta(), diag()
              );
              bindings = cons(m, bindings);
            }
            case Tree.TypeDef def -> {
              var t = translateModuleSymbol(def);
              bindings= cons(t, bindings);
            }
            case Tree.Import imp -> {
              imports = cons(translateImport(imp), imports);
            }
            case Tree.Export exp -> {
              exports = cons(translateExport(exp), exports);
            }
            case Tree.Function fn -> {
              var t = translateModuleSymbol(fn);
              bindings = cons(t, bindings);
            }
//            case Tree.Comment comment -> {
//              var doc = comment.getToken().codeRepr();
//              doc = doc.replace("##", "");
//              var t = new IR$Comment$Documentation(doc, getIdentifiedLocation(comment), meta(), diag());
//              bindings = cons(t, bindings);
//            }
            case Tree.TypeSignature def -> {
              var t = translateModuleSymbol(def);
              bindings = cons(t, bindings);
            }
            case Tree.ArgumentBlockApplication app -> {
              var t = translateComment(app);
              bindings = cons(t, bindings);
            }
            case Tree.Annotated anno -> {
              var n = new IR$Name$Annotation("@" + anno.getAnnotation().codeRepr(), getIdentifiedLocation(anno), meta(), diag());
              bindings = cons(n, bindings);
              var t = translateModuleSymbol(anno.getExpression());
              bindings = cons(t, bindings);
            }
            case null -> {
            }
            default -> {
              throw new UnhandledEntity(line.getExpression(), "translateModule");
            }
          }
        }
        yield new IR.Module(imports.reverse(), exports.reverse(), bindings.reverse(), getIdentifiedLocation(module), meta(), diag());
      }
      default -> throw new UnhandledEntity(module, "translateModule");
    };

            /*
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
    }
      */
  }

  /*



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

  /** Translates a module-level definition from its [[AST]] representation into
    * [[IR]].
    *
    * @param inputAst the definition to be translated
    * @return the [[IR]] representation of `inputAST`
    */
  IR$Module$Scope$Definition translateModuleSymbol(Tree inputAst) {
    return switch (inputAst) {
    /*
    inputAst match {
      case AST.Ident.Annotation.any(annotation) =>
        IR.Name.Annotation(annotation.name, getIdentifiedLocation(annotation))
      */
      case Tree.TypeDef def -> {
        var typeName = buildName(def.getName());
        var translatedBody = translateTypeBody(def.getBlock(), true);
        for (var c : def.getConstructors()) {
          var cExpr = c.getExpression();
          if (cExpr == null) {
            continue;
          }
          var constructorName = buildName(inputAst, cExpr.getConstructor());
          List<IR.DefinitionArgument> args = translateArgumentsDefinition(cExpr.getArguments());
          var cAt = getIdentifiedLocation(inputAst);
          var data = new IR$Module$Scope$Definition$Data(constructorName, args, cAt, meta(), diag());

          translatedBody = cons(data, translatedBody);
        }
        // type
        List<IR.DefinitionArgument> args = translateArgumentsDefinition(def.getParams());
        yield new IR$Module$Scope$Definition$SugaredType(
          typeName,
          args,
          translatedBody.reverse(),
          getIdentifiedLocation(inputAst),
          meta(), diag()
        );
      }
      case Tree.Function fn -> {
        var nameId = buildName(fn, fn.getName());

        /*
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
            Some(
              IR.Name.Qualified(
                typeSegments,
                MethodReference.genLocation(typeSegments)
              )
            ),
            methodSegments.last,
            MethodReference.genLocation(methodSegments)
          )
        } else {
        */
        var methodName = nameId;
        var methodRef = new IR$Name$MethodReference(
          Option.empty(),
          methodName,
          getIdentifiedLocation(fn),
          meta(), diag()
        );
        var args = translateArgumentsDefinition(fn.getArgs());
        var body = translateExpression(fn.getBody(), false);

        yield new IR$Module$Scope$Definition$Method$Binding(
          methodRef,
          args,
          body,
          getIdentifiedLocation(inputAst),
          meta(), diag()
        );
      }
      /*
      case AstView.FunctionSugar(
            AST.Ident.Var("foreign"),
            header,
            body
          ) =>
        translateForeignDefinition(header, body) match {
          case Right((name, arguments, body)) =>
            val methodRef =
              Name.MethodReference(None, name, name.location)
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
        val methodName = buildName(name)

        val methodReference = Name.MethodReference(
          None,
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
      */
      case Tree.TypeSignature sig -> {
//      case AstView.TypeAscription(typed, sig) =>
        var methodName = buildName(sig, sig.getVariable());
        var methodReference = new IR$Name$MethodReference(
          Option.empty(),
          methodName,
          getIdentifiedLocation(sig),
          meta(), diag()
        );
        var signature = translateExpression(sig.getType(), true);
        yield new IR$Type$Ascription(methodReference, signature, getIdentifiedLocation(sig), meta(), diag());
        /*
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
    */
      }
    default -> new IR$Error$Syntax(inputAst, new IR$Error$Syntax$UnexpectedExpression$(), meta(), diag());
    };
  }

  private List<IR.DefinitionArgument> translateArgumentsDefinition(java.util.List<ArgumentDefinition> args) {
    return CollectionConverters.asScala(args.stream().map(p -> translateArgumentDefinition(p)).iterator()).toList();
  }

  /** Translates the body of a type expression.
    *
    * @param body the body to be translated
    * @return the [[IR]] representation of `body`
    */
  private scala.collection.immutable.List<IR> translateTypeBody(java.util.List<Line> block, boolean found) {
    var ir = block.stream().map(line -> translateTypeBodyExpression(line)).filter(line -> line != null).iterator();
    return CollectionConverters.asScala(ir).toList();
  }

  /** Translates any expression that can be found in the body of a type
    * declaration from [[AST]] into [[IR]].
    *
    * @param maybeParensedInput the expression to be translated
    * @return the [[IR]] representation of `maybeParensedInput`
    */
  private IR translateTypeBodyExpression(Line maybeParensedInput) {
    var inputAst = maybeManyParensed(maybeParensedInput.getExpression());
    return switch (inputAst) {
      case null -> null;
    /*
    inputAst match {
      case AST.Ident.Annotation.any(ann) =>
        IR.Name.Annotation(ann.name, getIdentifiedLocation(ann))
      case AST.Ident.Cons.any(include) => translateIdent(include)
      */
      case Tree.TypeDef def -> translateModuleSymbol(def);
      case Tree.ArgumentBlockApplication app -> {
//        if (app.getLhs() instanceof Tree.Comment comment) {
//          var doc = new StringBuilder();
//          doc.append(comment.getToken().codeRepr());
//          yield new IR$Comment$Documentation(
//            doc.toString(), getIdentifiedLocation(comment), meta(), diag()
//          );
//        }
        yield null;
      }
      /*
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
    */
      default ->
        new IR$Error$Syntax(inputAst, IR$Error$Syntax$UnexpectedDeclarationInType$.MODULE$, meta(), diag());
    };
  }

  /*
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

  def translateMethodReference(inputAst: AST): IR.Name.MethodReference = {
    inputAst match {
      case AstView.MethodReference(path, methodName) =>
        val typeParts = path.map(translateExpression(_).asInstanceOf[IR.Name])
        IR.Name.MethodReference(
          Some(
            IR.Name.Qualified(typeParts, MethodReference.genLocation(typeParts))
          ),
          translateExpression(methodName).asInstanceOf[IR.Name],
          getIdentifiedLocation(inputAst)
        )
      case _ => throw new UnhandledEntity(inputAst, "translateMethodReference")
    }
  }

  /** Translates an arbitrary program expression from {@link Tree} into {@link IR}.
    *
    * @param maybeParensedInput the expresion to be translated
    * @return the {@link IR} representation of `maybeParensedInput`
    */
  IR.Expression translateExpression(Tree tree, boolean insideTypeSignature) {
    return translateExpression(tree, nil(), insideTypeSignature, false);
  }

  IR.Expression translateExpression(Tree tree, List<Tree> moreArgs, boolean insideTypeSignature, boolean isMethod) {
    return switch (tree) {
      case null -> null;
      case Tree.OprApp app -> {
        var op = app.getOpr().getRight();
        yield switch (op.codeRepr()) {
          case "." -> {
            var rhs = translateExpression(app.getRhs(), nil(), insideTypeSignature, true);
            var lhs = translateExpression(app.getLhs(), insideTypeSignature);
            IR.CallArgument callArgument = new IR$CallArgument$Specified(Option.empty(), lhs, getIdentifiedLocation(tree), meta(), diag());
            var firstArg = cons(callArgument, nil());
            var args = moreArgs.isEmpty() ? firstArg : translateCallArguments(moreArgs, firstArg, insideTypeSignature);
            var prefix = new IR$Application$Prefix(
                rhs, args,
                false,
                getIdentifiedLocation(tree),
                meta(),
                diag()
            );
            yield prefix;
          }
          case "->" -> {
            if (insideTypeSignature) {
              var literal = translateExpression(app.getLhs(), insideTypeSignature);
              var body = translateExpression(app.getRhs(), insideTypeSignature);
              var args = switch (body) {
                case IR$Type$Function fn -> {
                  body = fn.result();
                  yield cons(literal, fn.args());
                }
                default -> cons(literal, nil());
              };
              yield new IR$Type$Function(args, body, Option.empty(), meta(), diag());
            } else {
              // Old-style lambdas; this syntax will be eliminated after the parser transition is complete.
              var arg = app.getLhs();
              var isSuspended = false;
              if (arg instanceof Tree.UnaryOprApp susApp && "~".equals(susApp.getOpr().codeRepr())) {
                  arg = susApp.getRhs();
                  isSuspended = true;
              }
              IR.Name name = switch (arg) {
                case Tree.Wildcard wild -> new IR$Name$Blank(getIdentifiedLocation(wild.getToken()), meta(), diag());
                case Tree.Ident id -> {
                  IR.Expression identifier = translateIdent(id, false);
                  yield switch (identifier) {
                    case IR.Name name_ -> name_;
                    default -> throw new UnhandledEntity(identifier, "translateExpression");
                  };
                }
                default -> throw new UnhandledEntity(arg, "translateExpressiontranslateArgumentDefinition");
              };
              var arg_ = new IR$DefinitionArgument$Specified(
                      name,
                      Option.empty(),
                      Option.empty(),
                      isSuspended,
                      getIdentifiedLocation(arg),
                      meta(),
                      diag()
              );
              List<IR.DefinitionArgument> args = cons(arg_, nil());
              var body = translateExpression(app.getRhs(), false);
              yield new IR$Function$Lambda(args, body, getIdentifiedLocation(tree), true, meta(), diag());
            }
          }
          default -> {
            var lhs = translateCallArgument(app.getLhs(), insideTypeSignature);
            var rhs = translateCallArgument(app.getRhs(), insideTypeSignature);
            yield new IR$Application$Operator$Binary(
              lhs,
              new IR$Name$Literal(
                op.codeRepr(), true,
                getIdentifiedLocation(app),
                meta(), diag()
              ),
              rhs,
              getIdentifiedLocation(app),
              meta(), diag()
            );
          }
        };
      }

      case Tree.App app -> {
        var fn = translateExpression(app.getFunc(), cons(app.getArg(), moreArgs), insideTypeSignature, false);
        yield fn;
      }
      case Tree.NamedApp app -> {
        var fn = translateExpression(app.getFunc(), cons(app, moreArgs), insideTypeSignature, false);
        yield fn;
      }
      case Tree.Array arr -> {
        List<IR.Expression> items = nil();
        if (arr.getFirst() != null) {
          var exp = translateExpression(arr.getFirst(), insideTypeSignature);
          items = cons(exp, items);
        }
        for (var next : arr.getRest()) {
          var exp = translateExpression(next.getBody(), insideTypeSignature);
          items = cons(exp, items);
        }
        yield new IR$Application$Literal$Sequence(
          items.reverse(),
          getIdentifiedLocation(arr), meta(), diag()
        );
      }
      case Tree.Number n -> translateDecimalLiteral(n);
      case Tree.Ident id -> {
        var exprId = translateIdent(id, isMethod);
        if (moreArgs.isEmpty()) {
          yield exprId;
        } else {
          var args = translateCallArguments(moreArgs, nil(), insideTypeSignature);
          var prefix = new IR$Application$Prefix(
              exprId, args,
              false,
              getIdentifiedLocation(tree),
              meta(),
              diag()
          );
          yield prefix;
        }
      }
      case Tree.MultiSegmentApp app -> {
        var fnName = new StringBuilder();
        var sep = "";
        List<IR.CallArgument> args = nil();
        for (var seg : app.getSegments()) {
          var id = seg.getHeader().codeRepr();
          fnName.append(sep);
          fnName.append(id);

          var body = translateCallArgument(seg.getBody(), insideTypeSignature);
          args = cons(body, args);

          sep = "_";
        }
        var fn = new IR$Name$Literal(fnName.toString(), true, Option.empty(), meta(), diag());
        yield new IR$Application$Prefix(fn, args.reverse(), false, getIdentifiedLocation(tree), meta(), diag());
      }
      case Tree.BodyBlock body -> {
        List<IR.Expression> expressions = nil();
        IR.Expression last = null;
        for (var line : body.getStatements()) {
          final Tree expr = line.getExpression();
          if (expr == null) {
            continue;
          }
          if (last != null) {
            expressions = cons(last, expressions);
          }
          last = translateExpression(expr, insideTypeSignature);
        }
        yield new IR$Expression$Block(expressions.reverse(), last, getIdentifiedLocation(body), false, meta(), diag());
      }
      case Tree.Assignment assign -> {
        var name = buildName(assign.getPattern());
        var expr = translateExpression(assign.getExpr(), insideTypeSignature);
        yield new IR$Expression$Binding(name, expr, getIdentifiedLocation(tree), meta(), diag());
      }
      case Tree.ArgumentBlockApplication body -> {
        List<IR.Expression> expressions = nil();
        IR.Expression last = null;
        for (var line : body.getArguments()) {
          final Tree expr = line.getExpression();
          if (expr == null) {
            continue;
          }
          if (last != null) {
            expressions = cons(last, expressions);
          }
          last = translateExpression(expr, insideTypeSignature);
        }
        yield new IR$Expression$Block(expressions.reverse(), last, getIdentifiedLocation(body), false, meta(), diag());
      }
      case Tree.TypeAnnotated anno -> {
        var type = translateCallArgument(anno.getType(), true);
        var expr = translateCallArgument(anno.getExpression(), false);
        var opName = new IR$Name$Literal(anno.getOperator().codeRepr(), true, Option.empty(), meta(), diag());
        yield new IR$Application$Operator$Binary(
          expr,
          opName,
          type,
          getIdentifiedLocation(anno),
          meta(), diag()
        );
      }
      case Tree.Group group -> {
        yield translateExpression(group.getBody(), moreArgs, insideTypeSignature, isMethod);
      }
      case Tree.TextLiteral txt -> translateLiteral(txt);
      case Tree.CaseOf cas -> {
        var expr = translateExpression(cas.getExpression(), insideTypeSignature);
        List<IR$Case$Branch> branches = nil();
        for (var line : cas.getCases()) {
          if (line.getCase() == null) {
            continue;
          }
          var br = translateCaseBranch(line.getCase());
          branches = cons(br, branches);
        }
        yield new IR$Case$Expr(expr, branches.reverse(), getIdentifiedLocation(tree), meta(), diag());
      }
      case Tree.Function fun -> {
        var name = buildName(fun.getName());
        var args = translateArgumentsDefinition(fun.getArgs());
        var body = translateExpression(fun.getBody(), false);

        yield new IR$Function$Binding(name, args, body,
            getIdentifiedLocation(fun), true, meta(), diag()
        );
      }
      case Tree.OprSectionBoundary bound -> {
        var ast = translateExpression(bound.getAst(), insideTypeSignature);
        yield switch (ast) {
          case IR$Application$Prefix p -> p.function();
          default -> ast;
        };
      }
      case Tree.UnaryOprApp un -> switch (translateExpression(un.getRhs(), insideTypeSignature)) {
        case IR$Literal$Number n when "-".equals(un.getOpr().codeRepr()) -> n.copy(
          n.copy$default$1(),
          "-" + n.copy$default$2(),
          n.copy$default$3(),
          n.copy$default$4(),
          n.copy$default$5(),
          n.copy$default$6()
        );
        case IR.Expression e -> e;
      };
      case Tree.TypeSignature sig -> {
        var methodName = buildName(sig, sig.getVariable());
        var methodReference = new IR$CallArgument$Specified(
                Option.empty(),
                methodName,
                getIdentifiedLocation(sig),
                meta(), diag()
        );
        var opName = buildName(null, sig.getOperator(), true);
        var signature = translateCallArgument(sig.getType(), true);
        yield new IR$Application$Operator$Binary(methodReference, opName, signature, getIdentifiedLocation(sig), meta(), diag());
      }
      case Tree.TemplateFunction templ -> {
        yield translateExpression(templ.getAst(), insideTypeSignature);
      }
      case Tree.Wildcard wild -> {
        yield new IR$Name$Blank(getIdentifiedLocation(wild), meta(), diag());
      }
      case Tree.Annotated anno -> {
        var ir = new IR$Name$Annotation("@" + anno.getAnnotation().codeRepr(), getIdentifiedLocation(anno), meta(), diag());
        yield translateAnnotation(ir, anno.getExpression(), nil());
      }
      default -> throw new UnhandledEntity(tree, "translateExpression");
    };
    /*
    val inputAst = AstView.MaybeManyParensed
      .unapply(maybeParensedInput)
      .getOrElse(maybeParensedInput)

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
                .Literal("negate", isMethod = true, None),
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
    */
  }

  private IR$Application$Prefix translateAnnotation(IR$Name$Annotation ir, Tree expr, List<IR.CallArgument> callArgs) {
    boolean insideTypeSignature = false;
    return switch (expr) {
      case Tree.App fn -> {
        var fnAsArg = translateCallArgument(fn.getArg(), insideTypeSignature);
        yield translateAnnotation(ir, fn.getFunc(), cons(fnAsArg, callArgs));
      }
      case Tree.ArgumentBlockApplication fn -> {
        var fnAsArg = translateCallArgument(fn.getLhs(), insideTypeSignature);
        var arg = translateCallArgument(expr, insideTypeSignature);
        callArgs = cons(fnAsArg, cons(arg, callArgs));
        yield translateAnnotation(ir, null, callArgs);
      }
      case null -> {
        yield new IR$Application$Prefix(ir, callArgs, false, ir.location(), meta(), diag());
      }
      default -> {
        var arg = translateCallArgument(expr, insideTypeSignature);
        callArgs = cons(arg, callArgs);
        yield translateAnnotation(ir, null, callArgs);
      }
    };
  }

  IR.Expression translateDecimalLiteral(Tree.Number ast) {
    return translateDecimalLiteral(ast, ast.getInteger(), ast.getFractionalDigits());
  }

  IR.Expression translateDecimalLiteral(
    Tree ast,
    Token.Digits intPart,
    FractionalDigits fracPart
  ) {
    if (intPart.getBase() != null && !"10".equals(intPart.getBase())) {
      return new IR$Error$Syntax(
        intPart,
        new IR$Error$Syntax$UnsupportedSyntax("non-base-10 decimal literals"),
        meta(), diag()
      );
    } else {
      if (fracPart != null && fracPart.getDigits().getBase() != null) {
        if (!"10".equals(fracPart.getDigits().getBase())) {
          return new IR$Error$Syntax(
            intPart,
            IR$Error$Syntax$InvalidBaseInDecimalLiteral$.MODULE$,
            meta(), diag()
          );
        }
      }
      String literal = fracPart != null ?
          intPart.codeRepr() + "." + fracPart.getDigits().codeRepr() :
          intPart.codeRepr();
      return new IR$Literal$Number(
        Option.empty(),
        literal,
        getIdentifiedLocation(ast), meta(), diag()
      );
    }
  }

  IR.Literal translateLiteral(Tree.TextLiteral txt) {
    var fullTxt = txt.codeRepr().trim();
    var t = fullTxt.substring(1, fullTxt.length() - 1);
    return new IR$Literal$Text(t, getIdentifiedLocation(txt), meta(), diag());
  }

  /** Translates a program literal from its [[AST]] representation into
    * [[IR]].
    *
    * @param literal the literal to translate
    * @return the [[IR]] representation of `literal`
    *
  IR.Expression translateLiteral(Tree.TextLiteral literal) {
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
  IR.Expression translateQualifiedNameOrExpression(Tree arg) {
    IR$Name$Qualified name = buildQualifiedName(arg, false);
    if (name != null) {
      return name;
    } else {
      return translateExpression(arg, false);
    }
  }

  private static boolean isOperator(String txt, Either<MultipleOperatorError, Operator> op) {
    if (op.getRight() == null) {
      return false;
    }
    return txt.equals(op.getRight().codeRepr());
  }


  /** Translates an argument definition from [[AST]] into [[IR]].
   *
   * @param def the argument to translate
   * @return the [[IR]] representation of `arg`
   */
  IR.DefinitionArgument translateArgumentDefinition(ArgumentDefinition def) {
    Tree pattern = def.getPattern();
    IR.Name name = switch (pattern) {
      case Tree.Wildcard wild -> new IR$Name$Blank(getIdentifiedLocation(wild.getToken()), meta(), diag());
      case Tree.Ident id -> {
        IR.Expression identifier = translateIdent(id, false);
        yield switch (identifier) {
          case IR.Name name_ -> name_;
          // TODO: Other types of pattern. Needs IR support.
          default -> throw new UnhandledEntity(pattern, "translateArgumentDefinition");
        };
      }
      // TODO: Other types of pattern. Needs IR support.
      default -> throw new UnhandledEntity(pattern, "translateArgumentDefinition");
    };
    boolean isSuspended = def.getSuspension() != null;
    var ascribedType = Option.apply(def.getType()).map(ascription -> translateExpression(ascription.getType(), true));
    var defaultValue = Option.apply(def.getDefault()).map(default_ -> translateExpression(default_.getExpression(), false));
    return new IR$DefinitionArgument$Specified(
            name,
            ascribedType,
            defaultValue,
            isSuspended,
            getIdentifiedLocation(def),
            meta(),
            diag()
    );
  }

  @SuppressWarnings("unchecked")
  private List<IR.CallArgument> translateCallArguments(List<Tree> args, List<IR.CallArgument> res, boolean insideTypeSignature) {
    while (args.nonEmpty()) {
      var a = translateCallArgument(args.head(), insideTypeSignature);
      if (a != null) {
        res = cons(a, res);
      }
      args = (List<Tree>) args.tail();
    }
    return res.reverse();
  }

  /** Translates a call-site function argument from its [[AST]] representation
    * into [[IR]].
    *
    * @param arg the argument to translate
    * @return the [[IR]] representation of `arg`
    */
  IR$CallArgument$Specified translateCallArgument(
    Tree arg,
    boolean insideTypeSignature
  ) {
    var loc = getIdentifiedLocation(arg);
    return switch (arg) {
      case Tree.NamedApp app -> {
        var expr = translateExpression(app.getArg(), insideTypeSignature);
        var id = buildName(app, app.getName());
        yield new IR$CallArgument$Specified(Option.apply(id), expr, loc, meta(), diag());
      }
      case null -> null;
      default -> {
        var expr = translateExpression(arg, insideTypeSignature);
        yield new IR$CallArgument$Specified(Option.empty(), expr, loc, meta(), diag());
      }
    };
  }

  /** Calculates whether a set of arguments has its defaults suspended, and
    * processes the argument list to remove that operator.
    *
    * @param args the list of arguments
    * @return the list of arguments with the suspension operator removed, and
    *         whether or not the defaults are suspended

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
  private IR.Expression translateApplicationLike(
    Tree callable,
    boolean insideTypeAscription
  ) {
    /*
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
    */
    return null;
  }

  /** Translates an operator section from its [[AST]] representation into the
    * [[IR]] representation.
    *
    * @param section the operator section
    * @return the [[IR]] representation of `section`

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
  IR.Expression translateIdent(Tree identifier, boolean isMethod) {
    return switch (identifier) {
      case null -> null;
      case Tree.Ident id -> buildName(id, id.getToken(), isMethod);
      default -> throw new UnhandledEntity(identifier, "translateIdent");
    };
    /*
    identifier match {
      case AST.Ident.Var(name) =>
        if (name == Constants.Names.SELF_ARGUMENT) {
          Name.Self(getIdentifiedLocation(identifier))
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
    }
    */
  }

  /** Translates an arbitrary binding operation from its [[AST]] representation
    * into [[IR]].
    *
    * @param location the source location of the binding
    * @param name the name of the binding being assigned to
    * @param expr the expression being assigned to `name`
    * @return the [[IR]] representation of `expr` being bound to `name`

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
  IR$Case$Branch translateCaseBranch(Case branch) {
    if (branch.getPattern() == null) {
      throw new UnhandledEntity(branch, "translateCaseBranch");
    }
    return new IR$Case$Branch(
          translatePattern(branch.getPattern(), nil()),
          translateExpression(branch.getExpression(), false),
          getIdentifiedLocation(branch.getExpression()), meta(), diag()
      );
  }

  /** Translates a pattern in a case expression from its [[AST]] representation
    * into [[IR]].
    *
    * @param block the case pattern to translate
    * @return
    */
  IR.Pattern translatePattern(Tree block, List<IR.Pattern> fields) {
    var pattern = maybeManyParensed(block);
    return switch (pattern) {
      case Tree.Ident id -> {
        yield new IR$Pattern$Constructor(
          buildName(id), fields,
          getIdentifiedLocation(id), meta(), diag()
        );
      }
      case Tree.OprApp id -> {
        var qualifiedName = buildQualifiedName(pattern);
        yield new IR$Pattern$Constructor(
          qualifiedName, fields,
          getIdentifiedLocation(id), meta(), diag()
        );
      }
      case Tree.App id -> {
        var args = translatePatternArguments(id.getArg(), fields);
        yield translatePattern(id.getFunc(), args);
      }
      case Tree.Wildcard wild -> translateWildcardPattern(wild);
      case Tree.TextLiteral lit -> {
        yield new IR$Pattern$Literal(translateLiteral(lit), getIdentifiedLocation(lit), meta(), diag());
      }
      case Tree.Number num -> {
        yield new IR$Pattern$Literal((IR.Literal) translateDecimalLiteral(num), getIdentifiedLocation(num), meta(), diag());
      }
      default -> throw new UnhandledEntity(pattern, "translatePattern");
    };
  }

  private List<IR.Pattern> translatePatternArguments(Tree t, List<IR.Pattern> prev) {
    var tree = maybeManyParensed(t);
    return switch (tree) {
      case Tree.OprApp app -> {
       var tail = translatePatternArguments(app.getRhs(), prev);
       yield cons(translatePattern(app.getLhs(), tail), nil());
      }
      case Tree.App app -> {
       var tail = translatePatternArguments(app.getArg(), prev);
       yield cons(translatePattern(app.getFunc(), tail), nil());
      }
      case Tree.Ident id -> {
        var name = buildName(id);
        var pattern = Character.isLowerCase(name.name().charAt(0)) ?
                new IR$Pattern$Name(name, getIdentifiedLocation(id), meta(), diag()) :
                new IR$Pattern$Constructor(name, nil(), getIdentifiedLocation(id), meta(), diag());
        yield cons(pattern, prev);
      }
      case Tree.Wildcard wild -> cons(translateWildcardPattern(wild), prev);
      default -> throw new UnhandledEntity(tree, "translatePattern");
    };
  }

  private IR$Pattern$Name translateWildcardPattern(Tree.Wildcard wild) {
      var at = getIdentifiedLocation(wild);
      var blank = new IR$Name$Blank(
              at, meta(), diag()
      );
      return new IR$Pattern$Name(blank, at, meta(), diag());
  }

  /** Translates an arbitrary grouped piece of syntax from its [[AST]]
    * representation into [[IR]].
    *
    * It is currently an error to have an empty group.
    *
    * @param group the group to translate
    * @return the [[IR]] representation of the contents of `group`

  def translateGroup(group: AST.Group): Expression = {
    group.body match {
      case Some(ast) => translateExpression(ast)
      case None      => Error.Syntax(group, Error.Syntax.EmptyParentheses)
    }
  }
  */

  private IR$Name$Qualified buildQualifiedName(Tree t) {
    return buildQualifiedName(t, true);
  }
  private IR$Name$Qualified buildQualifiedName(Tree t, boolean fail) {
    var segments = buildNames(t, '.', fail);
    return segments == null ? null : new IR$Name$Qualified(segments, Option.empty(), meta(), diag());
  }

  private List<IR.Name> buildNames(Tree t, char separator, boolean fail) {
    List<IR.Name> segments = nil();
    for (;;) {
      switch (t) {
        case Tree.OprApp app -> {
          if (!String.valueOf(separator).equals(app.getOpr().getRight().codeRepr())) {
            throw new UnhandledEntity(t, "buildNames with " + separator);
          }
          segments = cons(buildName(app.getRhs()), segments);
          t = app.getLhs();
        }
        case Tree.Ident id -> {
          segments = cons(buildName(id), segments);
          return segments;
        }
        default -> {
          if (fail) {
            throw new UnhandledEntity(t, "buildNames");
          } else {
            return null;
          }
        }
      }
    }
  }

  /** Translates an import statement from its [[AST]] representation into
    * [[IR]].
    *
    * @param imp the import to translate
    * @return the [[IR]] representation of `imp`
    */
  IR$Module$Scope$Import translateImport(Tree.Import imp) {
    Option<IR$Name$Literal> rename = Option.apply(imp.getAs()).map(as -> buildName(as.getBody()));
    if (imp.getPolyglot() != null) {
      List<IR.Name> qualifiedName = buildNames(imp.getImport().getBody(), '.', true);
      StringBuilder pkg = new StringBuilder();
      String cls = extractPackageAndName(qualifiedName, pkg);
      return new IR$Module$Scope$Import$Polyglot(
              new IR$Module$Scope$Import$Polyglot$Java(pkg.toString(), cls),
              rename.map(name -> name.name()), getIdentifiedLocation(imp),
              meta(), diag()
      );
    }
    IR$Name$Qualified qualifiedName;
    // TODO: onlyNames, hiddenNames
    if (imp.getFrom() != null) {
      qualifiedName = buildQualifiedName(imp.getFrom().getBody());
    } else {
      qualifiedName = buildQualifiedName(imp.getImport().getBody());
    }
    var isAll = imp.getAll() != null;
    return new IR$Module$Scope$Import$Module(
      qualifiedName, rename, isAll, Option.empty(),
      Option.empty(), getIdentifiedLocation(imp), false,
      meta(), diag()
    );
  }

  @SuppressWarnings("unchecked")
  private String extractPackageAndName(List<IR.Name> qualifiedName, StringBuilder pkg) {
      String cls = null;
      for (List<IR.Name> next = qualifiedName; !next.isEmpty();) {
        if (cls != null) {
          if (pkg.length() != 0) {
            pkg.append(".");
          }
          pkg.append(cls);
        }
        cls = next.head().name();
        next = (List<IR.Name>) next.tail();
      }
    return cls;
  }

  /** Translates an export statement from its [[AST]] representation into
    * [[IR]].
    *
    * @param exp the export to translate
    * @return the [[IR]] representation of `imp`
    */
  @SuppressWarnings("unchecked")
  IR$Module$Scope$Export$Module translateExport(Tree.Export exp) {
    Option<IR$Name$Literal> rename = Option.apply(exp.getAs()).map(as -> buildName(as.getBody()));
    if (exp.getFrom() != null) {
      var qualifiedName = buildQualifiedName(exp.getFrom().getBody());
      var onlyBodies = exp.getExport().getBody();
      var isAll = exp.getAll() != null;
      final Option<List<IR$Name$Literal>> onlyNames = isAll ? Option.empty() :
        Option.apply((List<IR$Name$Literal>) (Object)buildNames(onlyBodies, ',', false));

      var hidingList = exp.getHiding() == null ? nil() : buildNames(exp.getHiding().getBody(), ',', false);
      final Option<List<IR$Name$Literal>> hidingNames = hidingList.isEmpty() ? Option.empty() :
        Option.apply((List<IR$Name$Literal>) (Object)hidingList);

      return new IR$Module$Scope$Export$Module(
        qualifiedName, rename,
        true, onlyNames, hidingNames, getIdentifiedLocation(exp), false,
        meta(), diag()
      );
    } else {
      var qualifiedName = buildQualifiedName(exp.getExport().getBody());
      return new IR$Module$Scope$Export$Module(
        qualifiedName, rename, false, Option.empty(),
        Option.empty(), getIdentifiedLocation(exp), false,
        meta(), diag()
        );
    }
  }

  /** Translates an arbitrary invalid expression from the [[AST]] representation
    * of the program into its [[IR]] representation.
    *
    * @param invalid the invalid entity to translate
    * @return the [[IR]] representation of `invalid`

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
    * @param tree the comment to transform
    * @return the [[IR]] representation of `comment`
    */
  IR.Comment translateComment(Tree tree) {
    return switch (tree) {
      case Tree.ArgumentBlockApplication comment -> {
        var doc = new StringBuilder();
        var sep = "";
        for (var l : comment.getArguments()) {
          if (l.getExpression() != null) {
            doc.append(sep);
            doc.append(l.getExpression().codeRepr());
            sep = "\n";
          }
        }
        yield new IR$Comment$Documentation(doc.toString(), getIdentifiedLocation(comment), meta(), diag());
      }
      default ->
        throw new UnhandledEntity(tree, "translateComment");
    };
  }

  private IR$Name$Literal buildName(Token name) {
    return new IR$Name$Literal(
            name.codeRepr(),
            false,
            getIdentifiedLocation(name),
            meta(), diag()
    );
  }
  private IR$Name$Literal buildName(Tree ident) {
    return switch (ident) {
      case Tree.Ident id -> buildName(id.getToken());
      default -> throw new UnhandledEntity(ident, "buildName");
    };
  }
  private IR$Name$Literal buildName(Tree ident, Token id) {
    return buildName(ident, id, false);
  }
  private IR$Name$Literal buildName(Tree ident, Token id, boolean isMethod) {
    return new IR$Name$Literal(
      id.codeRepr(),
      isMethod , // || AST.Opr.any.unapply(ident).isDefined,
      getIdentifiedLocation(ident),
      meta(), diag()
    );
  }

  private Option<IdentifiedLocation> getIdentifiedLocation(Tree ast) {
    return Option.apply(ast).map(ast_ -> {
      int begin = Math.toIntExact(ast_.getStartCode());
      int end = Math.toIntExact(ast_.getEndCode());
      return new IdentifiedLocation(new Location(begin, end), Option.empty());
    });
  }

  private Option<IdentifiedLocation> getIdentifiedLocation(ArgumentDefinition ast) {
    // Note: In the IR, `DefinitionArgument` is a type of AST node; in the parser, it is not an AST-level type, but a
    // type used only in specific locations. As a result, the IR expects it to have a source-code location, but the
    // parser doesn't have a span for it. Here we synthesize one. This will not be necessary if we refactor IR so that
    // `ArgumentDefinition` is not an AST node, though that change would have effects throughout the compiler and may
    // not be worthwhile if IR is expected to be replaced.
    long begin;
    if (ast.getOpen() != null) {
      begin = ast.getOpen().getStartCode();
    } else if (ast.getOpen2() != null) {
      begin = ast.getOpen2().getStartCode();
    } else if (ast.getSuspension() != null) {
      begin = ast.getSuspension().getStartCode();
    } else {
      begin = ast.getPattern().getStartCode();
    }
    int begin_ = Math.toIntExact(begin);
    long end;
    if (ast.getClose() != null) {
      end = ast.getClose().getEndCode();
    } else if (ast.getDefault() != null) {
      end = ast.getDefault().getEquals().getEndCode();
    } else if (ast.getClose2() != null) {
      end = ast.getClose2().getEndCode();
    } else if (ast.getType() != null) {
      end = ast.getType().getOperator().getEndCode();
    } else {
      end = ast.getPattern().getEndCode();
    }
    int end_ = Math.toIntExact(end);
    return Option.apply(new IdentifiedLocation(new Location(begin_, end_), Option.empty()));
  }

  private Option<IdentifiedLocation> getIdentifiedLocation(Token ast) {
    return Option.apply(ast).map(ast_ -> {
      int begin = Math.toIntExact(ast_.getStartCode());
      int end = Math.toIntExact(ast_.getEndCode());
      return new IdentifiedLocation(new Location(begin, end), Option.empty());
    });
  }
  private MetadataStorage meta() {
    return MetadataStorage.apply(nil());
  }
  private DiagnosticStorage diag() {
    return DiagnosticStorage.apply(nil());
  }
  @SuppressWarnings("unchecked")
  private static final <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }
  private static final <T> scala.collection.immutable.List<T> cons(T head, scala.collection.immutable.List<T> tail) {
    return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
  }

  private static Tree maybeManyParensed(Tree t) {
    for (;;) {
      switch (t) {
        case null -> {
          return null;
        }
        case Tree.Group g -> {
          assert "(".equals(g.getOpen().codeRepr());
          assert ")".equals(g.getClose().codeRepr());
          t = g.getBody();
        }
        default -> {
          return t;
        }
      }
    }
  }
}
