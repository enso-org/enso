package org.enso.compiler;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Application$Literal$Sequence;
import org.enso.compiler.core.IR$Application$Operator$Binary;
import org.enso.compiler.core.IR$Application$Operator$Section$Right;
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
import org.enso.compiler.core.IR$Name$Self;
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
import org.enso.syntax.text.Location;
import org.enso.syntax2.ArgumentDefinition;
import org.enso.syntax2.Case;
import org.enso.syntax2.DocComment;
import org.enso.syntax2.Either;
import org.enso.syntax2.FractionalDigits;
import org.enso.syntax2.Line;
import org.enso.syntax2.MultipleOperatorError;
import org.enso.syntax2.TextElement;
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
          var expr = line.getExpression();
          if (expr instanceof Tree.Documented doc) {
            var comment = translateComment(doc, doc.getDocumentation());
            // In this case, which usually includes top-level documentation for a module,
            // the old parser dropped the comment. For now, we copy that behavior for compatibility.
            expr = doc.getExpression();
          }
          switch (expr) {
            case Tree.Import imp -> {
              imports = cons(translateImport(imp), imports);
            }
            case Tree.Export exp -> {
              exports = cons(translateExport(exp), exports);
            }
            case null -> {}
            default -> {
              bindings = translateModuleSymbol(expr, bindings);
            }

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
        var translatedConstructorsIt = def.getConstructors().stream().map((c) -> {
          var cExpr = c.getExpression();
          if (cExpr == null) {
            return null;
          }
          var constructorName = buildName(inputAst, cExpr.getConstructor());
          List<IR.DefinitionArgument> args = translateArgumentsDefinition(cExpr.getArguments());
          var cAt = getIdentifiedLocation(inputAst);
          return new IR$Module$Scope$Definition$Data(constructorName, args, cAt, meta(), diag());
        }).filter((e) -> e != null).iterator();
        var translatedConstructors = CollectionConverters.asScala(translatedConstructorsIt).toList();
        // type
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

        var binding = new IR$Module$Scope$Definition$Method$Binding(
          methodRef,
          args,
          body,
          getIdentifiedLocation(inputAst),
          meta(), diag()
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
        var binding = new IR$Module$Scope$Definition$Method$Binding(
          reference,
          nil(),
          translateExpression(a.getExpr()),
          getIdentifiedLocation(a),
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
    /*
    inputAst match {
      case AST.Ident.Annotation.any(ann) =>
        IR.Name.Annotation(ann.name, getIdentifiedLocation(ann))
      case AST.Ident.Cons.any(include) => translateIdent(include)
      */
      case Tree.TypeDef def -> {
        yield translateModuleSymbol(def, (List) appendTo);
      }
      case Tree.ArgumentBlockApplication app -> {
//        if (app.getLhs() instanceof Tree.Comment comment) {
//          var doc = new StringBuilder();
//          doc.append(comment.getToken().codeRepr());
//          yield new IR$Comment$Documentation(
//            doc.toString(), getIdentifiedLocation(comment), meta(), diag()
//          );
//        }
        yield appendTo;
      }
      case Tree.OperatorTypeSignature sig -> {
        var typeName = buildName(getIdentifiedLocation(sig.getOperator()), sig.getOperator(), true);
        var ir = translateTypeSignature(sig, sig.getType(), typeName);
        yield cons(ir, appendTo);
      }
      case Tree.TypeSignature sig -> {
        var typeName = translateExpression(sig.getVariable(), false);
        var ir = translateTypeSignature(sig, sig.getType(), typeName);
        yield cons(ir, appendTo);
      }
      case Tree.OperatorFunction fun -> {
        var name = buildName(getIdentifiedLocation(fun.getName()), fun.getName(), true);
        var ir = translateFunction(fun, name, fun.getArgs(), fun.getBody());
        yield cons(ir, appendTo);
      }
      case Tree.Function fun -> {
        var name = buildName(fun.getName());
        var ir = translateFunction(fun, name, fun.getArgs(), fun.getBody());
        yield cons(ir, appendTo);
      }
      // This is a `Function` in IR, but an `Assignment` in Tree.
      // See: https://discord.com/channels/401396655599124480/1001476608957349917
      case Tree.Assignment assignment -> {
        var name = buildName(assignment.getPattern());
        java.util.List<ArgumentDefinition> args = java.util.Collections.emptyList();
        var ir = translateFunction(assignment, name, args, assignment.getExpr());
        yield cons(ir, appendTo);
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

  private IR.Expression translateCall(Tree tree) {
    var args = new java.util.ArrayList<IR.CallArgument>();
    var hasDefaultsSuspended = false;
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
          if (tree instanceof Tree.OprApp
                  && ((Tree.OprApp)tree).getOpr().getRight() != null
                  && ".".equals(((Tree.OprApp)tree).getOpr().getRight().codeRepr())
                  && ((Tree.OprApp)tree).getRhs() instanceof Tree.Ident) {
            var app = (Tree.OprApp)tree;
            var self = translateExpression(app.getLhs(), false);
            var loc = getIdentifiedLocation(app.getLhs());
            args.add(new IR$CallArgument$Specified(Option.empty(), self, loc, meta(), diag()));
            func = translateExpression(app.getRhs(), true);
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
                  getIdentifiedLocation(tree),
                  meta(),
                  diag()
          );
        }
      }
    }
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
        yield switch (op.codeRepr()) {
          case "." -> {
            final Option<IdentifiedLocation> loc = getIdentifiedLocation(tree);
            yield buildQualifiedName(app, loc, true);
          }

          case "->" -> {
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
            if (body == null) {
              body = new IR$Expression$Block(
                nil(), new IR$Name$Blank(Option.empty(), meta(), diag()),
                Option.empty(), true, meta(), diag()
              );
            }
            yield new IR$Function$Lambda(args, body, getIdentifiedLocation(tree), true, meta(), diag());
          }
          default -> {
            var lhs = unnamedCallArgument(app.getLhs());
            var rhs = unnamedCallArgument(app.getRhs());
            var name = new IR$Name$Literal(
              op.codeRepr(), true, getIdentifiedLocation(app), meta(), diag()
            );
            var loc = getIdentifiedLocation(app);
            yield lhs != null ? new IR$Application$Operator$Binary(
              lhs,name,rhs,
              loc,meta(), diag()
            ) : new IR$Application$Operator$Section$Right(
              name, rhs,
              loc, meta(), diag()
            );
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
      case Tree.Number n -> translateDecimalLiteral(n);
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
          if (expr instanceof Tree.Documented) {
            var doc = (Tree.Documented)expr;
            expressions.add(translateComment(doc, doc.getDocumentation()));
            expr = doc.getExpression();
          }
          last = translateExpression(expr, false);
        }
        var list = CollectionConverters.asScala(expressions.iterator()).toList();
        yield new IR$Expression$Block(list, last, getIdentifiedLocation(body), false, meta(), diag());
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
      case Tree.Group group -> translateExpression(group.getBody(), false);
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
      case Tree.OprSectionBoundary bound -> {
        var ast = translateExpression(bound.getAst(), false);
        yield switch (ast) {
          case IR$Application$Prefix p -> p.function();
          default -> ast;
        };
      }
      case Tree.UnaryOprApp un -> {
        var expr = translateExpression(un.getRhs(), false);
        var negate = new IR$Name$Literal("negate", true, Option.empty(), meta(), diag());
        var arg = new IR$CallArgument$Specified(Option.empty(), expr, expr.location(), meta(), diag());
        yield new IR$Application$Prefix(negate, cons(arg, nil()), false, expr.location(), meta(), diag());
      }
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
      default -> throw new UnhandledEntity(tree, "translateExpression");
    };
  }

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
          var exp = translateType(arr.getFirst(), insideTypeAscription);
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
    List<IR.CallArgument> allArgs = (List<IR.CallArgument>) pref.arguments().appendedAll(args.reverse());
    final IR$CallArgument$Specified blockArg = new IR$CallArgument$Specified(Option.empty(), block, block.location(), meta(), diag());
    List<IR.CallArgument> withBlockArgs = (List<IR.CallArgument>) allArgs.appended(blockArg);
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
    StringBuilder sb = new StringBuilder();
    for (var t : txt.getElements()) {
      switch (t) {
          case TextElement.Section s -> sb.append(s.getText().codeRepr());
          case TextElement.Escape e -> sb.appendCodePoint(e.getToken().getValue());
          default -> throw new UnhandledEntity(t, "translateLiteral");
      }
    }
    return new IR$Literal$Text(sb.toString(), getIdentifiedLocation(txt), meta(), diag());
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
    var name = buildQualifiedName(arg, Option.empty(), false);
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
                buildName(id), fields,
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
        new IR$Pattern$Literal((IR.Literal) translateDecimalLiteral(num), getIdentifiedLocation(num), meta(), diag());
      case Tree.TypeAnnotated anno -> {
        var type = buildNameOrQualifiedName(maybeManyParensed(anno.getType()));
        var expr = buildNameOrQualifiedName(maybeManyParensed(anno.getExpression()));
        yield new IR$Pattern$Type(expr, type instanceof IR.Name ? (IR.Name) type : null, Option.empty(), meta(), diag());
      }
      default -> throw new UnhandledEntity(pattern, "translatePattern");
    };
  }

  private IR$Pattern$Name translateWildcardPattern(Tree.Wildcard wild) {
      var at = getIdentifiedLocation(wild);
      var blank = new IR$Name$Blank(at, meta(), diag());
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
      case Tree.Ident id -> buildName(id);
      case Tree.Wildcard wild -> new IR$Name$Blank(getIdentifiedLocation(wild.getToken()), meta(), diag());
      default -> throw new UnhandledEntity(tree, "qualifiedNameSegment");
    };
  }
  private List<IR.Name> qualifiedNameSegments(Tree t) {
    java.util.stream.Stream<IR.Name> segments =
            unrollOprRhs(t, ".").stream().map(segment -> qualifiedNameSegment(segment));
    return CollectionConverters.asScala(segments.iterator()).toList();
  }
  private List<IR.Name> buildNameSequence(Tree t) {
    java.util.stream.Stream<IR.Name> segments =
            unrollOprRhs(t, ",").stream().map(segment -> buildNameOrQualifiedName(segment));
    return CollectionConverters.asScala(segments.iterator()).toList();
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
      List<IR.Name> qualifiedName = qualifiedNameSegments(imp.getImport().getBody());
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
        Option.apply((List<IR$Name$Literal>) (Object)buildNameSequence(onlyBodies));

      var hidingList = exp.getHiding() == null ? nil() : buildNameSequence(exp.getHiding().getBody());
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
    * @param doc the comment to transform
    * @return the [[IR]] representation of `comment`
    */
  IR$Comment$Documentation translateComment(Tree where, DocComment doc) {
      var msg = new StringBuilder();
      for (var t : doc.getElements()) {
        switch (t) {
          case TextElement.Section s -> {
            var whitespace = s.getText().getWhitespace();
            var txt = s.getText().codeRepr();
            if (whitespace.length() > 0) {
              whitespace = whitespace.subSequence(1, whitespace.length());
            }
            msg.append(whitespace);
            msg.append(txt.replaceAll("\n", ""));
          }

          default -> throw new UnhandledEntity(t, "translateComment");
        }
      }
      return new IR$Comment$Documentation(msg.toString(), getIdentifiedLocation(where), meta(), diag());
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
      default -> id;
    };
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
        case Tree.Group g -> t = g.getBody();
        default -> {
          return t;
        }
      }
    }
  }
}
