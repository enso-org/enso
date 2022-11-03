package org.enso.compiler;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Application$Literal$Sequence;
import org.enso.compiler.core.IR$Application$Operator$Binary;
import org.enso.compiler.core.IR$Application$Operator$Section$Left;
import org.enso.compiler.core.IR$Application$Operator$Section$Right;
import org.enso.compiler.core.IR$Application$Operator$Section$Sides;
import org.enso.compiler.core.IR$Application$Prefix;
import org.enso.compiler.core.IR$CallArgument$Specified;
import org.enso.compiler.core.IR$Case$Branch;
import org.enso.compiler.core.IR$Case$Expr;
import org.enso.compiler.core.IR$Comment$Documentation;
import org.enso.compiler.core.IR$DefinitionArgument$Specified;
import org.enso.compiler.core.IR$Error$Syntax;
import org.enso.compiler.core.IR$Error$Syntax$InvalidForeignDefinition;
import org.enso.compiler.core.IR$Error$Syntax$UnexpectedDeclarationInType$;
import org.enso.compiler.core.IR$Error$Syntax$UnexpectedExpression$;
import org.enso.compiler.core.IR$Error$Syntax$EmptyParentheses$;
import org.enso.compiler.core.IR$Error$Syntax$UnrecognizedToken$;
import org.enso.compiler.core.IR$Expression$Binding;
import org.enso.compiler.core.IR$Expression$Block;
import org.enso.compiler.core.IR$Foreign$Definition;
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
import org.enso.compiler.core.IR$Name$Self;
import org.enso.compiler.core.IR$Name$SelfType;
import org.enso.compiler.core.IR$Name$MethodReference;
import org.enso.compiler.core.IR$Name$Qualified;
import org.enso.compiler.core.IR$Pattern$Constructor;
import org.enso.compiler.core.IR$Pattern$Documentation;
import org.enso.compiler.core.IR$Pattern$Name;
import org.enso.compiler.core.IR$Pattern$Literal;
import org.enso.compiler.core.IR$Pattern$Type;
import org.enso.compiler.core.IR$Type$Ascription;
import org.enso.compiler.core.IR$Type$Function;
import org.enso.compiler.core.IR.IdentifiedLocation;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.exception.UnhandledEntity;
import org.enso.interpreter.epb.EpbParser;
import org.enso.syntax.text.Location;
import org.enso.syntax2.ArgumentDefinition;
import org.enso.syntax2.Base;
import org.enso.syntax2.DocComment;
import org.enso.syntax2.Line;
import org.enso.syntax2.TextElement;
import org.enso.syntax2.Token;
import org.enso.syntax2.Tree;

import scala.Option;
import scala.collection.immutable.LinearSeq;
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
          var expr = line.getExpression();
          // Documentation found among imports/exports or at the top of the module (if it starts with imports) is
          // placed in `bindings` by AstToIr.
          while (expr instanceof Tree.Documented doc) {
            bindings = cons(translateComment(doc, doc.getDocumentation()), bindings);
            expr = doc.getExpression();
          }
          switch (expr) {
            case Tree.Import imp -> imports = cons(translateImport(imp), imports);
            case Tree.Export exp -> exports = cons(translateExport(exp), exports);
            case null -> {}
            default -> bindings = translateModuleSymbol(expr, bindings);
          }
        }
        yield new IR.Module(imports.reverse(), exports.reverse(), bindings.reverse(), getIdentifiedLocation(module), meta(), diag());
      }
      default -> throw new UnhandledEntity(module, "translateModule");
    };
  }

  /** Translates a module-level definition from its [[AST]] representation into
    * [[IR]].
    *
    * @param inputAst the definition to be translated
    * @param appendTo list of already collected definitions
    * @return the [[IR]] representation of `inputAST` appended
    */
  List<IR$Module$Scope$Definition> translateModuleSymbol(Tree inputAst, List<IR$Module$Scope$Definition> appendTo) {
    return switch (inputAst) {
      case null -> appendTo;
      case Tree.TypeDef def -> {
        var typeName = buildName(def.getName());
        var translatedBody = translateTypeBody(def.getBlock());
        var irConstructors = new java.util.ArrayList<IR>();
        for (var constructorLine : def.getConstructors()) {
          var definition = constructorLine.getExpression();
          if (definition == null) {
            continue;
          }
          if (definition.getDocumentation() != null) {
            irConstructors.add(translateComment(def, definition.getDocumentation()));
          }
          var constructorName = buildName(inputAst, definition.getConstructor());
          List<IR.DefinitionArgument> args = translateArgumentsDefinition(definition.getArguments());
          var cAt = getIdentifiedLocation(inputAst);
          irConstructors.add(new IR$Module$Scope$Definition$Data(constructorName, args, cAt, meta(), diag()));
        }
        var translatedConstructors = CollectionConverters.asScala(irConstructors.iterator()).toList();
        List<IR.DefinitionArgument> args = translateArgumentsDefinition(def.getParams());
        var type = new IR$Module$Scope$Definition$SugaredType(
          typeName,
          args,
          translatedConstructors.appendedAll(translatedBody),
          getIdentifiedLocation(inputAst),
          meta(), diag()
        );
        yield cons(type, appendTo);
      }
      case Tree.Function fn -> {
        var methodRef = translateMethodReference(fn.getName(), false);
        var args = translateArgumentsDefinition(fn.getArgs());
        var body = translateExpression(fn.getBody());

        if (body == null) {
            throw new NullPointerException();
        }
        var binding = new IR$Module$Scope$Definition$Method$Binding(
          methodRef,
          args,
          body,
          getIdentifiedLocation(inputAst, 0, 1),
          meta(), diag()
        );
        yield cons(binding, appendTo);
      }
      case Tree.ForeignFunction fn when fn.getBody() instanceof Tree.TextLiteral body -> {
        var name = fn.getName();
        var nameLoc = getIdentifiedLocation(name);
        var methodRef = new IR$Name$MethodReference(Option.empty(), buildName(name), nameLoc, meta(), diag());
        var args = translateArgumentsDefinition(fn.getArgs());
        var languageName = fn.getLanguage().codeRepr();
        var language = EpbParser.ForeignLanguage.getBySyntacticTag(languageName);
        if (language == null) {
          var message = "Language '" + languageName + "' is not a supported polyglot language.";
          var error = new IR$Error$Syntax(inputAst, new IR$Error$Syntax$InvalidForeignDefinition(message), meta(), diag());
          yield cons(error, appendTo);
        }
        var text = buildTextConstant(body.getElements(), true);
        var def = new IR$Foreign$Definition(language, text, getIdentifiedLocation(fn.getBody()), meta(), diag());
        var binding = new IR$Module$Scope$Definition$Method$Binding(
                methodRef, args, def, getIdentifiedLocation(inputAst), meta(), diag()
        );
        yield cons(binding, appendTo);
      }
      case Tree.Annotated anno -> {
        var annotation = new IR$Name$Annotation("@" + anno.getAnnotation().codeRepr(), getIdentifiedLocation(anno), meta(), diag());
        yield translateModuleSymbol(anno.getExpression(), cons(annotation, appendTo));
      }
      case Tree.Documented doc -> {
        var comment = translateComment(doc, doc.getDocumentation());
        yield translateModuleSymbol(doc.getExpression(), cons(comment, appendTo));
      }
      case Tree.Assignment a -> {
        var reference = translateMethodReference(a.getPattern(), false);
        var body = translateExpression(a.getExpr());
        if (body == null) {
            throw new NullPointerException();
        }
        var aLoc = expandToContain(getIdentifiedLocation(a.getExpr()), body.location());
        var binding = new IR$Module$Scope$Definition$Method$Binding(
          reference,
          nil(),
          body.setLocation(aLoc),
          expandToContain(getIdentifiedLocation(a), aLoc),
          meta(), diag()
        );
        yield cons(binding, appendTo);
      }

      case Tree.TypeSignature sig -> {
        var methodReference = translateMethodReference(sig.getVariable(), true);
        var signature = translateType(sig.getType(), false);
        var ascription = new IR$Type$Ascription(methodReference, signature, getIdentifiedLocation(sig), meta(), diag());
        yield cons(ascription, appendTo);
      }
      default -> {
        var error = new IR$Error$Syntax(inputAst, new IR$Error$Syntax$UnexpectedExpression$(), meta(), diag());
        yield cons(error, appendTo);
      }
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
  private List<IR> translateTypeBody(java.util.List<Line> block) {
    List<IR> res = nil();
    for (var line : block) {
      res = translateTypeBodyExpression(line.getExpression(), res);
    }
    return res.reverse();
  }

  /** Translates any expression that can be found in the body of a type
    * declaration from [[AST]] into [[IR]].
    *
    * @param exp the expression to be translated
    * @return the [[IR]] representation of `maybeParensedInput`
    */
  @SuppressWarnings("unchecked")
  private List<IR> translateTypeBodyExpression(Tree exp, List<IR> appendTo) {
    var inputAst = maybeManyParensed(exp);
    return switch (inputAst) {
      case null -> appendTo;
      case Tree.TypeDef def -> translateModuleSymbol(def, (List) appendTo);
      case Tree.ArgumentBlockApplication app -> appendTo;
      case Tree.TypeSignature sig -> {
        var isMethod = false;
        if (sig.getVariable() instanceof Tree.Ident ident) {
          isMethod = ident.getToken().isOperatorLexically();
        }
        var typeName = translateExpression(sig.getVariable(), isMethod);
        var ir = translateTypeSignature(sig, sig.getType(), typeName);
        yield cons(ir, appendTo);
      }
      case Tree.Function fun -> {
        IR.Name name;
        if (fun.getName() instanceof Tree.Ident ident) {
          var isMethod = ident.getToken().isOperatorLexically();
          name = buildName(getIdentifiedLocation(fun.getName()), ident.getToken(), isMethod);
        } else {
          name = buildNameOrQualifiedName(fun.getName());
        }
        var ir = translateFunction(fun, name, fun.getArgs(), fun.getBody());
        yield cons(ir, appendTo);
      }
      // In some cases this is a `Function` in IR, but an `Assignment` in Tree.
      // See: https://discord.com/channels/401396655599124480/1001476608957349917
      case Tree.Assignment assignment -> {
        var name = buildName(assignment.getPattern());
        java.util.List<ArgumentDefinition> args = java.util.Collections.emptyList();
        var ir = translateFunction(assignment, name, args, assignment.getExpr());
        yield cons(ir, appendTo);
      }
      case Tree.ForeignFunction fn when fn.getBody() instanceof Tree.TextLiteral body -> {
        var name = buildName(fn.getName());
        var args = translateArgumentsDefinition(fn.getArgs());
        var languageName = fn.getLanguage().codeRepr();
        var language = EpbParser.ForeignLanguage.getBySyntacticTag(languageName);
        if (language == null) {
          var message = "Language '" + languageName + "' is not a supported polyglot language.";
          var error = new IR$Error$Syntax(inputAst, new IR$Error$Syntax$InvalidForeignDefinition(message), meta(), diag());
          yield cons(error, appendTo);
        }
        var text = buildTextConstant(body.getElements(), true);
        var def = new IR$Foreign$Definition(language, text, getIdentifiedLocation(fn.getBody()), meta(), diag());
        var binding = new IR$Function$Binding(name, args, def, getIdentifiedLocation(fn), true, meta(), diag());
        yield cons(binding, appendTo);
      }
      case Tree.Documented doc -> {
        var irDoc = translateComment(doc, doc.getDocumentation());
        yield translateTypeBodyExpression(doc.getExpression(), cons(irDoc, appendTo));
      }
      case Tree.Annotated anno -> {
        var ir = new IR$Name$Annotation("@" + anno.getAnnotation().codeRepr(), getIdentifiedLocation(anno), meta(), diag());
        var annotation = translateAnnotation(ir, anno.getExpression(), nil());
        yield cons(annotation, appendTo);
      }
      default -> {
        var ir = new IR$Error$Syntax(inputAst, IR$Error$Syntax$UnexpectedDeclarationInType$.MODULE$, meta(), diag());
        yield cons(ir, appendTo);
      }
    };
  }

  @SuppressWarnings("unchecked")
  private IR$Application$Prefix translateTypeApplication(Tree.App app) {
      List<IR.CallArgument> args = nil();
      Tree t = app;
      while (t instanceof Tree.App tApp) {
        args = cons(translateTypeCallArgument(tApp.getArg()), args);
        t = tApp.getFunc();
      }
      var fullQualifiedNames = qualifiedNameSegments(t).reverse();
      var segments = fullQualifiedNames.length();
      var type = switch (segments) {
        case 1 -> fullQualifiedNames.head();
        default -> {
          var name = fullQualifiedNames.head();
          name = new IR$Name$Literal(name.name(), true, name.location(), name.passData(), name.diagnostics());
          List<IR.Name> tail = (List<IR.Name>)fullQualifiedNames.tail();
          tail = tail.reverse();
          final Option<IdentifiedLocation> loc = getIdentifiedLocation(app);
          IR.Name arg;
          if (segments == 2) {
            arg = tail.head();
          } else {
            arg = new IR$Name$Qualified(tail, loc, meta(), diag());
          }
          var ca = new IR$CallArgument$Specified(Option.empty(), arg, loc, meta(), diag());
          args = cons(ca, args);
          yield name;
        }
      };
      return new IR$Application$Prefix(type, args, false, getIdentifiedLocation(app), meta(), diag());
    }
    private IR.Expression translateFunction(Tree fun, IR.Name name, java.util.List<ArgumentDefinition> arguments, final Tree treeBody) {
        var args = translateArgumentsDefinition(arguments);
        var body = translateExpression(treeBody);
        if (args.isEmpty()) {
          if (body instanceof IR$Expression$Block block) {
            // suspended block has a name and no arguments
            body = block.copy(
              block.copy$default$1(),
              block.copy$default$2(),
              block.copy$default$3(),
              true,
              block.copy$default$5(),
              block.copy$default$6(),
              block.copy$default$7()
            );
          }
          return new IR$Expression$Binding(name, body,
            getIdentifiedLocation(fun), meta(), diag()
          );
        } else {
          return new IR$Function$Binding(name, args, body,
            getIdentifiedLocation(fun), true, meta(), diag()
          );
        }
   }

  private IR$Type$Ascription translateTypeSignature(Tree sig, Tree type, IR.Expression typeName) throws UnhandledEntity {
    var fn = translateType(type, false);
    return new IR$Type$Ascription(typeName, fn, getIdentifiedLocation(sig), meta(), diag());
  }


  /** Translates a method reference from [[AST]] into [[IR]].
    *
    * @param inputAst the method reference to translate
    * @return the [[IR]] representation of `inputAst`
    */
  IR$Name$MethodReference translateMethodReference(Tree sig, boolean alwaysLocation) {
    IR.Name method;
    Option<IR.Name> type;
    Option<IdentifiedLocation> loc;
    switch (sig) {
      case Tree.Ident id -> {
        type = Option.empty();
        method = buildName(id);
        loc = getIdentifiedLocation(sig);
      }
      case Tree.OprApp app when ".".equals(app.getOpr().getRight().codeRepr()) -> {
        type = Option.apply(buildQualifiedName(app.getLhs()));
        method = buildName(app.getRhs());
        if (alwaysLocation) {
          loc = getIdentifiedLocation(sig);
        } else {
          loc = Option.empty();
        }
      }
      default -> throw new UnhandledEntity(sig, "translateMethodReference");
    }
    return new IR$Name$MethodReference(type, method,
      loc, meta(), diag()
    );
  }

  private IR.Expression translateCall(Tree ast) {
    var args = new java.util.ArrayList<IR.CallArgument>();
    var hasDefaultsSuspended = false;
    var tree = ast;
    for (;;) {
      switch (tree) {
        case Tree.App app when app.getArg() instanceof Tree.AutoScope -> {
          hasDefaultsSuspended = true;
          tree = app.getFunc();
        }
        case Tree.App app -> {
          var expr = translateExpression(app.getArg(), false);
          var loc = getIdentifiedLocation(app.getArg());
          args.add(new IR$CallArgument$Specified(Option.empty(), expr, loc, meta(), diag()));
          tree = app.getFunc();
        }
        case Tree.NamedApp app -> {
          var expr = translateExpression(app.getArg(), false);
          var loc = getIdentifiedLocation(app.getArg());
          var id = buildName(app, app.getName());
          args.add(new IR$CallArgument$Specified(Option.apply(id), expr, loc, meta(), diag()));
          tree = app.getFunc();
        }
        case Tree.DefaultApp app -> {
          var loc = getIdentifiedLocation(app.getDefault());
          var expr = buildName(app.getDefault());
          args.add(new IR$CallArgument$Specified(Option.empty(), expr, loc, meta(), diag()));
          tree = app.getFunc();
        }
        default -> {
          IR.Expression func;
          if (tree instanceof Tree.OprApp oprApp
                  && oprApp.getOpr().getRight() != null
                  && ".".equals(oprApp.getOpr().getRight().codeRepr())
                  && oprApp.getRhs() instanceof Tree.Ident) {
            func = translateExpression(oprApp.getRhs(), true);
            if (oprApp.getLhs() == null && args.isEmpty()) {
              return func;
            }
            if (oprApp.getLhs() != null) {
              var self = translateExpression(oprApp.getLhs(), false);
              var loc = getIdentifiedLocation(oprApp.getLhs());
              args.add(new IR$CallArgument$Specified(Option.empty(), self, loc, meta(), diag()));
            }
          } else if (args.isEmpty()) {
            return null;
          } else {
            func = translateExpression(tree, false);
          }
          java.util.Collections.reverse(args);
          var argsList = CollectionConverters.asScala(args.iterator()).toList();
          return new IR$Application$Prefix(
                  func, argsList,
                  hasDefaultsSuspended,
                  getIdentifiedLocation(ast),
                  meta(),
                  diag()
          );
        }
      }
    }
  }

  private IR.Name translateOldStyleLambdaArgumentName(Tree arg, boolean[] suspended, IR.Expression[] defaultValue) {
    return switch (arg) {
      case Tree.Group g -> translateOldStyleLambdaArgumentName(g.getBody(), suspended, defaultValue);
      case Tree.Wildcard wild -> new IR$Name$Blank(getIdentifiedLocation(wild.getToken()), meta(), diag());
      case Tree.OprApp app when "=".equals(app.getOpr().getRight().codeRepr()) -> {
          if (defaultValue != null) {
            defaultValue[0] = translateExpression(app.getRhs(), false);
          }
          yield translateOldStyleLambdaArgumentName(app.getLhs(), suspended, null);
      }
      case Tree.Ident id -> {
        IR.Expression identifier = translateIdent(id, false);
        yield switch (identifier) {
          case IR.Name name_ -> name_;
          default -> throw new UnhandledEntity(identifier, "translateOldStyleLambdaArgumentName");
        };
      }
      case Tree.UnaryOprApp app when "~".equals(app.getOpr().codeRepr()) -> {
          if (suspended != null) {
            suspended[0] = true;
          }
          yield translateOldStyleLambdaArgumentName(app.getRhs(), null, defaultValue);
      }
      default -> throw new UnhandledEntity(arg, "translateOldStyleLambdaArgumentName");
    };
  }

  /** Translates an arbitrary program expression from {@link Tree} into {@link IR}.
   *
   * @param tree the expression to be translated
   * @return the {@link IR} representation of `tree`
   */
  IR.Expression translateExpression(Tree tree) {
    return translateExpression(tree, false);
  }
  IR.Expression translateExpression(Tree tree, boolean isMethod) {
    if (tree == null) {
      return null;
    }
    var callExpression = translateCall(tree);
    if (callExpression != null) {
      return callExpression;
    }
    return switch (tree) {
      case Tree.OprApp app -> {
        var op = app.getOpr().getRight();
        if (op == null) {
          var at = getIdentifiedLocation(app);
          var arr = app.getOpr().getLeft().getOperators();
          if (arr.size() > 0 && arr.get(0).codeRepr().equals("=")) {
              var errLoc = arr.size() > 1 ? getIdentifiedLocation(arr.get(1)) : at;
              var err = new IR$Error$Syntax(errLoc.get(), IR$Error$Syntax$UnrecognizedToken$.MODULE$, meta(), diag());
              var name = buildName(app.getLhs());
              yield new IR$Expression$Binding(name, err, at, meta(), diag());
          } else {
              yield new IR$Error$Syntax(at.get(), IR$Error$Syntax$UnrecognizedToken$.MODULE$, meta(), diag());
          }
        }
        yield switch (op.codeRepr()) {
          case "." -> {
            final Option<IdentifiedLocation> loc = getIdentifiedLocation(tree);
            yield buildQualifiedName(app, loc, true);
          }

          case "->" -> {
            // Old-style lambdas; this syntax will be eliminated after the parser transition is complete.
            var arg = app.getLhs();
            var isSuspended = new boolean[1];
            if (arg instanceof Tree.UnaryOprApp susApp && "~".equals(susApp.getOpr().codeRepr())) {
                arg = susApp.getRhs();
                isSuspended[0] = true;
            }
            var defaultValue = new IR.Expression[1];
            IR.Name name = translateOldStyleLambdaArgumentName(arg, isSuspended, defaultValue);
            var arg_ = new IR$DefinitionArgument$Specified(
                    name,
                    Option.empty(),
                    Option.apply(defaultValue[0]),
                    isSuspended[0],
                    getIdentifiedLocation(arg),
                    meta(),
                    diag()
            );
            List<IR.DefinitionArgument> args = cons(arg_, nil());
            var body = translateExpression(app.getRhs(), false);
            if (body == null) {
              body = new IR$Expression$Block(
                nil(), new IR$Name$Blank(Option.empty(), meta(), diag()),
                Option.empty(), true, meta(), diag()
              );
            }
            var at = expandToContain(switch (body) {
              case IR$Expression$Block __ -> getIdentifiedLocation(tree, 0, 1);
              default -> getIdentifiedLocation(tree);
            }, body.location());
            yield new IR$Function$Lambda(args, body, at, true, meta(), diag());
          }
          default -> {
            var lhs = unnamedCallArgument(app.getLhs());
            var rhs = unnamedCallArgument(app.getRhs());
            if ("@".equals(op.codeRepr()) && lhs.value() instanceof IR$Application$Prefix fn) {
                final Option<IdentifiedLocation> where = getIdentifiedLocation(op);
                var err = new IR$Error$Syntax(where.get(), IR$Error$Syntax$UnrecognizedToken$.MODULE$, meta(), diag());
                var errArg = new IR$CallArgument$Specified(Option.empty(), err, where, meta(), diag());
                var args = cons(rhs, cons(errArg, fn.arguments()));
                yield new IR$Application$Prefix(fn.function(), args.reverse(), false, getIdentifiedLocation(app), meta(), diag());
            }
            var name = new IR$Name$Literal(
              op.codeRepr(), true, getIdentifiedLocation(op), meta(), diag()
            );
            var loc = getIdentifiedLocation(app);
            if (lhs == null && rhs == null) {
              // AstToIr doesn't emit a location in this case.
              yield new IR$Application$Operator$Section$Sides(name, Option.empty(), meta(), diag());
            } else if (lhs == null) {
              yield new IR$Application$Operator$Section$Right(name, rhs, loc, meta(), diag());
            } else if (rhs == null) {
              yield new IR$Application$Operator$Section$Left(lhs, name, loc, meta(), diag());
            } else {
              yield new IR$Application$Operator$Binary(lhs, name, rhs, loc,meta(), diag());
            }
          }
        };
      }
      case Tree.Array arr -> {
        List<IR.Expression> items = nil();
        if (arr.getFirst() != null) {
          var exp = translateExpression(arr.getFirst(), false);
          items = cons(exp, items);
          for (var next : arr.getRest()) {
            exp = translateExpression(next.getBody(), false);
            items = cons(exp, items);
          }
        }
        yield new IR$Application$Literal$Sequence(
          items.reverse(),
          getIdentifiedLocation(arr), meta(), diag()
        );
      }
      case Tree.Number n -> translateNumber(n);
      case Tree.Ident id -> translateIdent(id, isMethod);
      case Tree.MultiSegmentApp app -> {
        var fnName = new StringBuilder();
        var sep = "";
        List<IR.CallArgument> args = nil();
        for (var seg : app.getSegments()) {
          var id = seg.getHeader().codeRepr();
          fnName.append(sep);
          fnName.append(id);

          var body = unnamedCallArgument(seg.getBody());
          args = cons(body, args);

          sep = "_";
        }
        var fn = new IR$Name$Literal(fnName.toString(), true, Option.empty(), meta(), diag());
        checkArgs(args);
        yield new IR$Application$Prefix(fn, args.reverse(), false, getIdentifiedLocation(tree), meta(), diag());
      }
      case Tree.BodyBlock body -> {
        var expressions = new java.util.ArrayList<IR.Expression>();
        IR.Expression last = null;
        for (var line : body.getStatements()) {
          Tree expr = line.getExpression();
          if (expr == null) {
            continue;
          }
          if (last != null) {
            expressions.add(last);
          }
          while (expr instanceof Tree.Documented doc) {
            expr = doc.getExpression();
            expressions.add(translateComment(doc, doc.getDocumentation()));
          }
          last = translateExpression(expr, false);
        }
        // If the block ended in a documentation node without an expression, last may be null;
        // the return value of the block is a doc comment.
        // (This is to match the behavior of AstToIr; after the parser transition, we should probably
        // ignore the orphaned documentation and return the last actual expression in the block.)
        if (last == null && expressions.size() > 0) {
          last = expressions.get(expressions.size() - 1);
          expressions.remove(expressions.size()-1);
        }
        var list = CollectionConverters.asScala(expressions.iterator()).toList();
        var locationWithANewLine = getIdentifiedLocation(body, 0, 1);
        if (last != null && last.location().isDefined() && last.location().get().end() != locationWithANewLine.get().end()) {
            var patched = new Location(last.location().get().start(), locationWithANewLine.get().end() - 1);
            var id = new IdentifiedLocation(patched, locationWithANewLine.get().id());
            last = last.setLocation(Option.apply(id));
        }
        yield new IR$Expression$Block(list, last, locationWithANewLine, false, meta(), diag());
      }
      case Tree.Assignment assign -> {
        var name = buildNameOrQualifiedName(assign.getPattern());
        var expr = translateExpression(assign.getExpr(), false);
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
          last = translateExpression(expr, false);
        }
        if (last == null) {
            last = new IR$Name$Blank(Option.empty(), meta(), diag());
        }
        var block = new IR$Expression$Block(expressions.reverse(), last, getIdentifiedLocation(body), false, meta(), diag());
        if (body.getLhs() != null) {
          var fn = translateExpression(body.getLhs(), isMethod);
          List<IR.CallArgument> args = nil();
          for (var line : body.getArguments()) {
            var expr = line.getExpression();
            if (expr != null) {
              continue;
            }
            var call = translateCallArgument(expr);
            args = cons(call, args);
          }
          yield switch (fn) {
            case IR$Application$Prefix pref -> patchPrefixWithBlock(pref, block, args);
            default -> block;
          };
        } else {
          yield block;
        }
      }
      case Tree.TypeAnnotated anno -> translateTypeAnnotated(anno);
      case Tree.Group group -> {
          yield switch (translateExpression(group.getBody(), false)) {
              case null -> new IR$Error$Syntax(getIdentifiedLocation(group).get(), IR$Error$Syntax$EmptyParentheses$.MODULE$, meta(), diag());
              case IR$Application$Prefix pref -> {
                  final Option<IdentifiedLocation> groupWithoutParenthesis = getIdentifiedLocation(group, 1, -1);
                  yield pref.setLocation(groupWithoutParenthesis);
              }
              case IR.Expression in -> in;
          };
      }
      case Tree.TextLiteral txt -> translateLiteral(txt);
      case Tree.CaseOf cas -> {
        var expr = translateExpression(cas.getExpression(), false);
        List<IR$Case$Branch> branches = nil();
        for (var line : cas.getCases()) {
          if (line.getCase() == null) {
            continue;
          }
          var branch = line.getCase();
          if (branch.getDocumentation() != null) {
            var comment = translateComment(cas, branch.getDocumentation());
            var loc = getIdentifiedLocation(cas);
            var doc = new IR$Pattern$Documentation(comment.doc(), loc, meta(), diag());
            var br= new IR$Case$Branch(
                    doc,
                    new IR.Empty(Option.empty(), meta(), diag()),
                    loc, meta(), diag()
            );
            branches = cons(br, branches);
          }
          // A branch with no expression is used to hold any orphaned documentation at the end of the case-of
          // expression, with no case to attach it to.
          if (branch.getExpression() != null) {
            var br = new IR$Case$Branch(
                    translatePattern(branch.getPattern()),
                    translateExpression(branch.getExpression(), false),
                    getIdentifiedLocation(branch.getExpression()), meta(), diag()
            );
            branches = cons(br, branches);
          }
        }
        yield new IR$Case$Expr(expr, branches.reverse(), getIdentifiedLocation(tree), meta(), diag());
      }
      case Tree.Function fun -> {
        var name = buildName(fun.getName());
        yield translateFunction(fun, name, fun.getArgs(), fun.getBody());
      }
      case Tree.OprSectionBoundary bound -> translateExpression(bound.getAst(), false);
      case Tree.UnaryOprApp un when "-".equals(un.getOpr().codeRepr()) ->
        switch (translateExpression(un.getRhs(), false)) {
          // AstToIr doesn't construct negative floating-point literals.
          // Match that behavior for testing during the transition.
          case IR$Literal$Number n when !n.copy$default$2().contains(".") -> n.copy(
            n.copy$default$1(),
            "-" + n.copy$default$2(),
            n.copy$default$3(),
            n.copy$default$4(),
            n.copy$default$5(),
            n.copy$default$6()
          );
          case IR.Expression expr -> {
            var negate = new IR$Name$Literal("negate", true, Option.empty(), meta(), diag());
            var arg = new IR$CallArgument$Specified(Option.empty(), expr, expr.location(), meta(), diag());
            yield new IR$Application$Prefix(negate, cons(arg, nil()), false, expr.location(), meta(), diag());
          }
        };
      case Tree.TypeSignature sig -> {
        var methodName = buildName(sig.getVariable());
        var methodReference = new IR$CallArgument$Specified(
                Option.empty(),
                methodName,
                getIdentifiedLocation(sig),
                meta(), diag()
        );
        var opName = buildName(Option.empty(), sig.getOperator(), true);
        var signature = translateTypeCallArgument(sig.getType());
        yield new IR$Application$Operator$Binary(methodReference, opName, signature, getIdentifiedLocation(sig), meta(), diag());
      }
      case Tree.TemplateFunction templ -> translateExpression(templ.getAst(), false);
      case Tree.Wildcard wild -> new IR$Name$Blank(getIdentifiedLocation(wild), meta(), diag());
      case Tree.Annotated anno -> {
        var ir = new IR$Name$Annotation("@" + anno.getAnnotation().codeRepr(), getIdentifiedLocation(anno), meta(), diag());
        yield translateAnnotation(ir, anno.getExpression(), nil());
      }
      // Documentation can be attached to an expression in a few cases, like if someone documents a line of an
      // `ArgumentBlockApplication`. The documentation is ignored.
      case Tree.Documented docu -> translateExpression(docu.getExpression());
      case Tree.App app -> {
          var fn = translateExpression(app.getFunc(), isMethod);
          var loc = getIdentifiedLocation(app);
          if (app.getArg() instanceof Tree.AutoScope) {
              yield new IR$Application$Prefix(fn, nil(), true, loc, meta(), diag());
          } else {
              yield fn.setLocation(loc);
          }
      }
      default -> throw new UnhandledEntity(tree, "translateExpression");
    };
  }

  // The `insideTypeAscription` argument replicates an AstToIr quirk. Once the parser
  // transition is complete, we should eliminate it, keeping only the `false` branches.
  IR.Expression translateType(Tree tree, boolean insideTypeAscription) {
    return switch (tree) {
      case Tree.App app -> translateTypeApplication(app);
      case Tree.OprApp app -> {
        var op = app.getOpr().getRight();
        yield switch (op.codeRepr()) {
          case "." -> {
            final Option<IdentifiedLocation> loc = getIdentifiedLocation(tree);
            yield buildQualifiedName(app, loc, true);
          }
          case "->" -> {
            var literal = translateType(app.getLhs(), insideTypeAscription);
            var body = translateType(app.getRhs(), insideTypeAscription);
            var args = switch (body) {
              case IR$Type$Function fn -> {
                body = fn.result();
                yield cons(literal, fn.args());
              }
              default -> cons(literal, nil());
            };
            yield new IR$Type$Function(args, body, Option.empty(), meta(), diag());
          }
          default -> {
            var lhs = translateTypeCallArgument(app.getLhs());
            var rhs = translateTypeCallArgument(app.getRhs());
            var name = new IR$Name$Literal(
                    op.codeRepr(), true,
                    getIdentifiedLocation(app),
                    meta(), diag()
            );
            var loc = getIdentifiedLocation(app);
            yield new IR$Application$Operator$Binary(lhs, name, rhs, loc, meta(), diag());
          }
        };
      }
      case Tree.Array arr -> {
        List<IR.Expression> items = nil();
        if (arr.getFirst() != null) {
          var exp = translateType(arr.getFirst(), false);
          items = cons(exp, items);
          for (var next : arr.getRest()) {
            exp = translateType(next.getBody(), insideTypeAscription);
            items = cons(exp, items);
          }
        }
        yield new IR$Application$Literal$Sequence(
                items.reverse(),
                getIdentifiedLocation(arr), meta(), diag()
        );
      }
      case Tree.Ident id when insideTypeAscription -> buildQualifiedName(id, getIdentifiedLocation(id), true);
      case Tree.Ident id -> buildName(getIdentifiedLocation(id), id.getToken(), false);
      case Tree.Group group -> translateType(group.getBody(), insideTypeAscription);
      case Tree.UnaryOprApp un -> translateType(un.getRhs(), insideTypeAscription);
      case Tree.Wildcard wild -> new IR$Name$Blank(getIdentifiedLocation(wild), meta(), diag());
      case Tree.TypeAnnotated anno -> translateTypeAnnotated(anno);
      default -> throw new UnhandledEntity(tree, "translateType");
    };
  }
  IR.Expression translateTypeAnnotated(Tree.TypeAnnotated anno) {
    var type = translateTypeCallArgument(anno.getType());
    var expr = translateCallArgument(anno.getExpression());
    var opName = new IR$Name$Literal(anno.getOperator().codeRepr(), true, Option.empty(), meta(), diag());
    return new IR$Application$Operator$Binary(
            expr,
            opName,
            type,
            getIdentifiedLocation(anno),
            meta(), diag()
    );
  }

  @SuppressWarnings("unchecked")
  private IR$Application$Prefix patchPrefixWithBlock(IR$Application$Prefix pref, IR$Expression$Block block, List<IR.CallArgument> args) {
    if (args.nonEmpty() && args.head() == null) {
        args = (List<IR.CallArgument>) args.tail();
    }
    List<IR.CallArgument> allArgs = (List<IR.CallArgument>) pref.arguments().appendedAll(args.reverse());
    final IR$CallArgument$Specified blockArg = new IR$CallArgument$Specified(Option.empty(), block, block.location(), meta(), diag());
    List<IR.CallArgument> withBlockArgs = (List<IR.CallArgument>) allArgs.appended(blockArg);
    checkArgs(withBlockArgs);
    return new IR$Application$Prefix(pref.function(), withBlockArgs, pref.hasDefaultsSuspended(), pref.location(), meta(), diag());
  }

  private IR$Application$Prefix translateAnnotation(IR$Name$Annotation ir, Tree expr, List<IR.CallArgument> callArgs) {
    return switch (expr) {
      case Tree.App fn -> {
        var fnAsArg = translateCallArgument(fn.getArg());
        yield translateAnnotation(ir, fn.getFunc(), cons(fnAsArg, callArgs));
      }
      case Tree.ArgumentBlockApplication fn -> {
        var fnAsArg = translateCallArgument(fn.getLhs());
        var arg = translateCallArgument(expr);
        callArgs = cons(fnAsArg, cons(arg, callArgs));
        yield translateAnnotation(ir, null, callArgs);
      }
      case null -> {
        yield new IR$Application$Prefix(ir, callArgs, false, ir.location(), meta(), diag());
      }
      default -> {
        var arg = translateCallArgument(expr);
        callArgs = cons(arg, callArgs);
        yield translateAnnotation(ir, null, callArgs);
      }
    };
  }

  IR.Expression translateNumber(Tree.Number ast) {
    var intPart = ast.getInteger();
    final Option<String> base = switch (intPart.getBase()) {
      case Base.Binary b -> Option.apply("2");
      case Base.Hexadecimal b -> Option.apply("16");
      case Base.Octal b -> Option.apply("8");
      case null -> Option.empty();
      default -> Option.empty();
    };
    var fracPart = ast.getFractionalDigits();
    String literal = fracPart != null ? intPart.codeRepr() + "." + fracPart.getDigits().codeRepr() : intPart.codeRepr();
    return new IR$Literal$Number(base, literal, getIdentifiedLocation(ast), meta(), diag());
  }

  IR.Literal translateLiteral(Tree.TextLiteral txt) {
    var stripComments = txt.getOpen().codeRepr().length() > 1;
    // Splices are not yet supported in the IR.
    var value = buildTextConstant(txt.getElements(), stripComments);
    return new IR$Literal$Text(value, getIdentifiedLocation(txt), meta(), diag());
  }
  String buildTextConstant(Iterable elements, boolean stripComments) {
    StringBuilder sb = new StringBuilder();
    for (var t : elements) {
      switch (t) {
        case TextElement.Section s -> {
          var text = s.getText().codeRepr();
          if (stripComments) {
            // Reproduce an AstToIr bug for testing.
            text = text.replaceAll("#[^\n\r]*", "");
          }
          sb.append(text);
        }
        case TextElement.Escape e -> sb.appendCodePoint(e.getToken().getValue());
        default -> throw new UnhandledEntity(t, "buildTextConstant");
      }
    }
    return sb.toString();
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
    var ascribedType = Option.apply(def.getType()).map(ascription -> translateType(ascription.getType(), true));
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

  /** Translates a call-site function argument from its [[AST]] representation
    * into [[IR]].
    *
    * @param arg the argument to translate
    * @return the [[IR]] representation of `arg`
    */
  IR$CallArgument$Specified translateCallArgument(Tree arg) {
    var loc = getIdentifiedLocation(arg);
    return switch (arg) {
      case Tree.NamedApp app -> {
        var expr = translateExpression(app.getArg(), false);
        var id = sanitizeName(buildName(app, app.getName()));
        yield new IR$CallArgument$Specified(Option.apply(id), expr, loc, meta(), diag());
      }
      case null -> null;
      default -> {
        var expr = translateExpression(arg, false);
        yield new IR$CallArgument$Specified(Option.empty(), expr, loc, meta(), diag());
      }
    };
  }
  IR$CallArgument$Specified translateTypeCallArgument(Tree arg) {
    var loc = getIdentifiedLocation(arg);
    var expr = translateType(arg, false);
    return new IR$CallArgument$Specified(Option.empty(), expr, loc, meta(), diag());
  }
  IR$CallArgument$Specified unnamedCallArgument(Tree arg) {
    if (arg == null) {
      return null;
    }
    var loc = getIdentifiedLocation(arg);
    var expr = translateExpression(arg);
    return new IR$CallArgument$Specified(Option.empty(), expr, loc, meta(), diag());
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
      case Tree.Ident id -> sanitizeName(buildName(getIdentifiedLocation(id), id.getToken(), isMethod));
      default -> throw new UnhandledEntity(identifier, "translateIdent");
    };
  }

  /** Translates a pattern in a case expression from its [[AST]] representation
    * into [[IR]].
    *
    * @param block the case pattern to translate
    * @return
    */
  IR.Pattern translatePattern(Tree block) {
    var pattern = maybeManyParensed(block);
    var elements = unrollApp(pattern);
    var lhs = elements.get(0);
    var args = elements.stream().skip(1).map(arg -> translatePattern(arg));
    var fields = CollectionConverters.asScala(args.iterator()).toList();
    return switch (lhs) {
      case Tree.Ident id when id.getToken().isTypeOrConstructor() || !fields.isEmpty() -> {
        yield new IR$Pattern$Constructor(
                sanitizeName(buildName(id)), fields,
                getIdentifiedLocation(id), meta(), diag()
        );
      }
      case Tree.Ident id -> new IR$Pattern$Name(buildName(id), getIdentifiedLocation(id), meta(), diag());
      case Tree.OprApp app when ".".equals(app.getOpr().getRight().codeRepr()) -> {
        var qualifiedName = buildQualifiedName(app);
        yield new IR$Pattern$Constructor(
          qualifiedName, fields, getIdentifiedLocation(app), meta(), diag()
        );
      }
      case Tree.Wildcard wild -> translateWildcardPattern(wild);
      case Tree.TextLiteral lit ->
        new IR$Pattern$Literal(translateLiteral(lit), getIdentifiedLocation(lit), meta(), diag());
      case Tree.Number num ->
        new IR$Pattern$Literal((IR.Literal) translateNumber(num), getIdentifiedLocation(num), meta(), diag());
      case Tree.TypeAnnotated anno -> {
        var type = buildNameOrQualifiedName(maybeManyParensed(anno.getType()));
        var expr = buildNameOrQualifiedName(maybeManyParensed(anno.getExpression()));
        yield new IR$Pattern$Type(expr, type instanceof IR.Name ? (IR.Name) type : null, Option.empty(), meta(), diag());
      }
      case Tree.Group group -> translatePattern(group.getBody());
      default -> throw new UnhandledEntity(pattern, "translatePattern");
    };
  }

  private IR$Pattern$Name translateWildcardPattern(Tree.Wildcard wild) {
      var at = getIdentifiedLocation(wild);
      var blank = new IR$Name$Blank(at, meta(), diag());
      return new IR$Pattern$Name(blank, at, meta(), diag());
  }

  private IR$Name$Qualified buildQualifiedName(Tree t) {
    return buildQualifiedName(t, Option.empty(), true);
  }
  private IR$Name$Qualified buildQualifiedName(Tree t, Option<IdentifiedLocation> loc, boolean fail) {
    return new IR$Name$Qualified(qualifiedNameSegments(t), loc, meta(), diag());
  }
  private IR.Name buildNameOrQualifiedName(Tree t) {
    var segments = qualifiedNameSegments(t);
    if (segments.length() == 1) {
      return segments.head();
    } else {
      return new IR$Name$Qualified(segments, Option.empty(), meta(), diag());
    }
  }
  private java.util.List<Tree> unrollOprRhs(Tree list, String operator) {
    var segments = new java.util.ArrayList<Tree>();
    while (list instanceof Tree.OprApp) {
      var app = (Tree.OprApp)list;
      if (app.getOpr().getRight() == null || !operator.equals(app.getOpr().getRight().codeRepr())) {
        break;
      }
      segments.add(app.getRhs());
      list = app.getLhs();
    }
    segments.add(list);
    java.util.Collections.reverse(segments);
    return segments;
  }
  private java.util.List<Tree> unrollApp(Tree list) {
    var elems = new java.util.ArrayList<Tree>();
    while (list instanceof Tree.App app) {
      elems.add(app.getArg());
      list = app.getFunc();
    }
    elems.add(list);
    java.util.Collections.reverse(elems);
    return elems;
  }
  private IR.Name qualifiedNameSegment(Tree tree) {
    return switch (tree) {
      case Tree.Ident id -> sanitizeName(buildName(id));
      case Tree.Wildcard wild -> new IR$Name$Blank(getIdentifiedLocation(wild.getToken()), meta(), diag());
      default -> throw new UnhandledEntity(tree, "qualifiedNameSegment");
    };
  }
  private List<IR.Name> qualifiedNameSegments(Tree t) {
    java.util.stream.Stream<IR.Name> segments =
            unrollOprRhs(t, ".").stream().map(segment -> qualifiedNameSegment(segment));
    return CollectionConverters.asScala(segments.iterator()).toList();
  }
  private List<IR$Name$Literal> buildNameSequence(Tree t) {
    java.util.stream.Stream<IR$Name$Literal> segments =
            unrollOprRhs(t, ",").stream().map(segment -> buildName(segment));
    return CollectionConverters.asScala(segments.iterator()).toList();
  }

  /** Translates an import statement from its [[AST]] representation into
    * [[IR]].
    *
    * @param imp the import to translate
    * @return the [[IR]] representation of `imp`
    */
  @SuppressWarnings("unchecked")
  IR$Module$Scope$Import translateImport(Tree.Import imp) {
    Option<IR$Name$Literal> rename = Option.apply(imp.getAs()).map(as -> buildName(as.getBody()));
    if (imp.getPolyglot() != null) {
      if (!imp.getPolyglot().getBody().codeRepr().equals("java")) {
        throw new UnhandledEntity(imp, "translateImport");
      }
      List<IR.Name> qualifiedName = qualifiedNameSegments(imp.getImport().getBody());
      StringBuilder pkg = new StringBuilder();
      String cls = extractPackageAndName(qualifiedName, pkg);
      return new IR$Module$Scope$Import$Polyglot(
              new IR$Module$Scope$Import$Polyglot$Java(pkg.toString(), cls),
              rename.map(name -> name.name()), getIdentifiedLocation(imp),
              meta(), diag()
      );
    }
    var isAll = imp.getAll() != null;
    IR$Name$Qualified qualifiedName;
    Option<List<IR$Name$Literal>> onlyNames = Option.empty();
    if (imp.getFrom() != null) {
      qualifiedName = buildQualifiedName(imp.getFrom().getBody());
      if (!isAll) {
        onlyNames = Option.apply(buildNameSequence(imp.getImport().getBody()));
      }
    } else {
      qualifiedName = buildQualifiedName(imp.getImport().getBody());
    }
    Option<List<IR$Name$Literal>> hidingNames = Option.apply(imp.getHiding()).map(
            hiding -> buildNameSequence(hiding.getBody()));
    return new IR$Module$Scope$Import$Module(
      qualifiedName, rename, isAll || onlyNames.isDefined() || hidingNames.isDefined(), onlyNames,
      hidingNames, getIdentifiedLocation(imp), false,
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
    Option<List<IR$Name$Literal>> hidingNames = Option.apply(exp.getHiding()).map(
            hiding -> buildNameSequence(hiding.getBody()));
    IR$Name$Qualified qualifiedName;
    Option<List<IR$Name$Literal>> onlyNames = Option.empty();
    if (exp.getFrom() != null) {
      qualifiedName = buildQualifiedName(exp.getFrom().getBody());
      var onlyBodies = exp.getExport().getBody();
      if (exp.getAll() == null) {
        onlyNames = Option.apply(buildNameSequence(onlyBodies));
      }
    } else {
      qualifiedName = buildQualifiedName(exp.getExport().getBody());
    }
    return new IR$Module$Scope$Export$Module(
      qualifiedName, rename, (exp.getFrom() != null), onlyNames,
      hidingNames, getIdentifiedLocation(exp), false,
      meta(), diag()
      );
  }

  /** Translates a comment from its [[AST]] representation into its [[IR]]
    * representation.
    *
    * @param doc the comment to transform
    * @return the [[IR]] representation of `comment`
    */
  IR$Comment$Documentation translateComment(Tree where, DocComment doc) {
    var text = buildTextConstant(doc.getElements(), true);
    return new IR$Comment$Documentation(text, getIdentifiedLocation(where), meta(), diag());
  }

  private IR$Name$Literal buildName(Token name) {
    return buildName(getIdentifiedLocation(name), name, false);
  }
  private IR$Name$Literal buildName(Tree ident) {
    return switch (ident) {
      case Tree.Ident id -> buildName(getIdentifiedLocation(ident), id.getToken(), false);
      default -> throw new UnhandledEntity(ident, "buildName");
    };
  }

  private IR$Name$Literal buildName(Tree ident, Token id) {
    return buildName(getIdentifiedLocation(ident), id, false);
  }

  private IR$Name$Literal buildName(Option<IdentifiedLocation> loc, Token id, boolean isMethod) {
    final String name = id.codeRepr();
    return new IR$Name$Literal(name, isMethod, loc, meta(), diag());
  }

  private IR.Name sanitizeName(IR$Name$Literal id) {
    return switch (id.name()) {
      case "self" -> new IR$Name$Self(id.location(), false, id.passData(), id.diagnostics());
      case "Self" -> new IR$Name$SelfType(id.location(), id.passData(), id.diagnostics());
      default -> id;
    };
  }
  
  private Option<IdentifiedLocation> expandToContain(Option<IdentifiedLocation> encapsulating, Option<IdentifiedLocation> inner) {
    if (encapsulating.isEmpty() || inner.isEmpty()) {
      return encapsulating;
    }
    var en = encapsulating.get();
    var in = inner.get();
    if (en.start() > in.start() || en.end() < in.end()) {
      var loc = new Location(
        Math.min(en.start(), in.start()),
        Math.max(en.end(), in.end())
      );
      return Option.apply(new IdentifiedLocation(loc, en.id()));
    } else {
      return encapsulating;
    }
  }

  private Option<IdentifiedLocation> getIdentifiedLocation(Tree ast) {
    return getIdentifiedLocation(ast, 0, 0);
  }
  private Option<IdentifiedLocation> getIdentifiedLocation(Tree ast, int b, int e) {
    return Option.apply(switch (ast) {
        case null -> null;
        default -> {
            var begin = Math.toIntExact(ast.getStartCode()) + b;
            var end = Math.toIntExact(ast.getEndCode()) + e;
            var someId = Option.apply(ast.uuid());
            yield new IdentifiedLocation(new Location(begin, end), someId);
        }
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
        case Tree.Group g -> t = g.getBody();
        default -> {
          return t;
        }
      }
    }
  }

    private void checkArgs(List<IR.CallArgument> args) {
        LinearSeq<IR.CallArgument> a = args;
        while (!a.isEmpty()) {
            if (a.head() == null) {
                throw new IllegalStateException("Problem: " + args);
            }
            a = (LinearSeq<IR.CallArgument>) a.tail();
        }
    }
}
