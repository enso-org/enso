package org.enso.compiler.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Empty;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Comment;
import org.enso.compiler.core.ir.expression.Foreign;
import org.enso.compiler.core.ir.expression.Operator;
import org.enso.compiler.core.ir.expression.Section;
import org.enso.compiler.core.ir.expression.errors.Syntax;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.syntax2.ArgumentDefinition;
import org.enso.syntax2.Base;
import org.enso.syntax2.DocComment;
import org.enso.syntax2.Line;
import org.enso.syntax2.Parser;
import org.enso.syntax2.TextElement;
import org.enso.syntax2.Token;
import org.enso.syntax2.Tree;
import org.enso.syntax2.Tree.Invalid;
import org.enso.syntax2.Tree.Private;

import scala.Option;
import scala.collection.immutable.LinearSeq;
import scala.collection.immutable.List;
import scala.jdk.javaapi.CollectionConverters;

final class TreeToIr {

  static final TreeToIr MODULE = new TreeToIr();
  static final String SKIP_MACRO_IDENTIFIER = "SKIP";
  static final String FREEZE_MACRO_IDENTIFIER = "FREEZE";

  private final Map<Location, UUID> idMap;

  private TreeToIr() {
    this.idMap = Collections.emptyMap();
  }

  public TreeToIr(Map<Location, UUID> idMap) {
    this.idMap = idMap;
  }

  /**
   * Translates a program represented in the parser {@link Tree} to the compiler's {@link IR}.
   *
   * @param ast the tree representing the program to translate
   * @return the IR representation of `inputAST`
   */
  Module translate(Tree ast) {
    return translateModule(ast);
  }

  Expression.Block translateBlock(Tree.BodyBlock ast) {
    return translateBodyBlock(ast, false);
  }

  /**
   * Translates an inline program expression represented in the parser {@link Tree} to the
   * compiler's {@link IR} representation.
   * <p>
   * Inline expressions must <b>only</b> be expressions, and may not contain any type of
   * definition.
   *
   * @param ast The tree representing the expression to translate.
   * @return The {@link IR} representation of the given ast if it is valid, otherwise
   * {@link Option#empty()}.
   */
  Option<Expression> translateInline(Tree ast) {
    return switch (ast) {
      case Tree.BodyBlock b -> {
        List<Expression> expressions = nil();
        java.util.List<IdentifiedLocation> locations = new ArrayList<>();
        for (Line statement : b.getStatements()) {
          Tree exprTree = statement.getExpression();
          Expression expr = switch (exprTree) {
            case null -> null;
            case Tree.Export x -> null;
            case Tree.Import x -> null;
            case Tree.Invalid x -> null;
            case Tree.TypeSignature sig -> {
             Expression methodReference;
             try {
               methodReference = translateMethodReference(sig.getVariable(), true);
             } catch (SyntaxException ex) {
               methodReference = ex.toError();
             }
              var signature = translateType(sig.getType());
              var ascription =
                  new Type.Ascription(
                      methodReference,
                      signature,
                      Option.empty(),
                      getIdentifiedLocation(sig),
                      meta());
              yield ascription;
            }
            case Tree.TypeAnnotated anno -> translateTypeAnnotated(anno);
            default -> translateExpression(exprTree);
          };
          if (expr != null) {
            expressions = join(expr, expressions);
            if (expr.location().isDefined()) {
              locations.add(expr.location().get());
            }
          }
        }
        yield switch (expressions.size()) {
          case 0 -> Option.empty();
          case 1 -> Option.apply(expressions.head());
          default -> {
            IdentifiedLocation combinedLocation;
            if (locations.isEmpty()) {
              combinedLocation = null;
            } else {
              combinedLocation =
                  new IdentifiedLocation(
                      new Location(
                          locations.get(1).start(),
                          locations.get(locations.size() - 1).end()
                      ),
                      null
                  );
            }
            var returnValue = expressions.head();
            @SuppressWarnings("unchecked")
            var statements = ((List<Expression>) expressions.tail()).reverse();
            yield Option.apply(new Expression.Block(
                statements,
                returnValue,
                combinedLocation,
                false,
                meta()
            ));
          }
        };
      }
      default -> {
        throw new IllegalStateException();
      }
    };
  }

  /**
   * Translate a top-level Enso module into [[IR]].
   *
   * @param module the [[AST]] representation of the module to translate
   * @return the [[IR]] representation of `module`
   */
  Module translateModule(Tree module) {
    return switch (module) {
      case Tree.BodyBlock b -> {
        boolean isPrivate = false;
        List<Definition> bindings = nil();
        List<Import> imports = nil();
        List<Export> exports = nil();
        List<Diagnostic> diag = nil();
        for (Line line : b.getStatements()) {
          var expr = line.getExpression();
          // Documentation found among imports/exports or at the top of the module (if it starts with imports) is
          // placed in `bindings` by AstToIr.
          while (expr instanceof Tree.Documented doc) {
            Definition c;
            try {
              c = translateComment(doc, doc.getDocumentation());
            } catch (SyntaxException ex) {
              c = ex.toError();
            }
            bindings = join(c, bindings);
            expr = doc.getExpression();
          }
          // Private keyword with an empty body is valid - that is how a private module is declared.
          if (expr instanceof Private priv && priv.getBody() == null) {
            if (isPrivate) {
              var error = translateSyntaxError(priv, new Syntax.UnsupportedSyntax(
                  "Private token specified more than once in the module"));
              diag = join(error, diag);
            }
            isPrivate = true;
            continue;
          }
          switch (expr) {
            case Tree.Import imp -> imports = join(translateImport(imp), imports);
            case Tree.Export exp -> exports = join(translateExport(exp), exports);
            case null -> {
            }
            default -> bindings = translateModuleSymbol(expr, bindings);
          }
        }
        if (diag.isEmpty()) {
          yield new Module(imports.reverse(), exports.reverse(), bindings.reverse(), isPrivate,
            getIdentifiedLocation(module), meta(), new DiagnosticStorage(diag));
        } else {
          yield new Module(imports.reverse(), exports.reverse(), bindings.reverse(), isPrivate,
              getIdentifiedLocation(module), meta());
        }
      }
      default -> new Module(
          nil(), nil(),
          join(translateSyntaxError(module, new Syntax.UnsupportedSyntax("translateModule")),
              nil()),
          false,
          getIdentifiedLocation(module),
          meta()
      );
    };
  }

  /**
   * Translates a module-level definition from its [[AST]] representation into [[IR]].
   *
   * @param inputAst the definition to be translated
   * @param appendTo list of already collected definitions
   * @return the [[IR]] representation of `inputAST` appended
   */
  List<Definition> translateModuleSymbol(Tree inputAst, List<Definition> appendTo) {
    try {
      return translateModuleSymbolImpl(inputAst, appendTo);
    } catch (SyntaxException ex) {
      return join(ex.toError(), appendTo);
    }
  }

  private List<Definition> translateModuleSymbolImpl(Tree inputAst, List<Definition> appendTo)
      throws SyntaxException {
    return switch (inputAst) {
      case null -> appendTo;

      case Tree.Private priv when priv.getBody() == null -> {
        var err = translateSyntaxError(priv, new Syntax.UnsupportedSyntax(
            "Private declaration without body at unexpected location. " +
                "Note that empty private declaration is allowed only at the top of the module"));
        yield join(err, appendTo);
      }

      case Tree.Private priv -> {
        assert priv.getBody() != null;
        var privBody = priv.getBody();
        switch (privBody) {
          case Tree.Function func -> {
            var binding = translateMethodBinding(func, true);
            yield join(binding, appendTo);
          }
          default -> {
            var err = translateSyntaxError(privBody,
                new Syntax.UnsupportedSyntax("Unexpected private entity"));
            yield join(err, appendTo);
          }
        }
      }

      case Tree.TypeDef def -> {
        var typeName = buildName(def.getName(), true);
        List<IR> irBody = nil();
        for (var line : def.getBody()) {
          irBody = translateTypeBodyExpression(line.getExpression(), irBody);
        }
        List<DefinitionArgument> args = translateArgumentsDefinition(def.getParams());
        var type = new Definition.SugaredType(
            typeName,
            args,
            irBody.reverse(),
            getIdentifiedLocation(inputAst),
            meta()
        );
        yield join(type, appendTo);
      }

      case Tree.Function fn -> {
        var binding = translateMethodBinding(fn, false);
        yield join(binding, appendTo);
      }

      case Tree.ForeignFunction fn when fn.getBody() instanceof Tree.TextLiteral body -> {
        var name = fn.getName();
        var nameLoc = getIdentifiedLocation(name);
        var methodRef = new Name.MethodReference(Option.empty(), buildName(name), nameLoc, meta());
        var args = translateArgumentsDefinition(fn.getArgs());
        var languageName = fn.getLanguage().codeRepr();
        var language = languageName;
        if (language == null) {
          var message = "Language '" + languageName + "' is not a supported polyglot language.";
          var error = translateSyntaxError(inputAst, new Syntax.InvalidForeignDefinition(message));
          yield join(error, appendTo);
        }
        var text = buildTextConstant(body, body.getElements());
        var def =
            new Foreign.Definition(language, text, getIdentifiedLocation(fn.getBody()), meta());
        // Foreign functions are always considered private
        var binding = new Method.Binding(
            methodRef, args, true, def, getIdentifiedLocation(inputAst), meta());
        yield join(binding, appendTo);
      }

      case Tree.AnnotatedBuiltin anno -> {
        var annotation = new Name.BuiltinAnnotation("@" + anno.getAnnotation().codeRepr(),
            getIdentifiedLocation(anno), meta());
        yield translateModuleSymbol(anno.getExpression(), join(annotation, appendTo));
      }

      case Tree.Annotated anno -> {
        if (anno.getArgument() == null) {
          yield join(translateSyntaxError(anno, Syntax.UnexpectedExpression$.MODULE$), appendTo);
        } else {
          var annotationArgument = translateExpression(anno.getArgument());
          var annotation = new Name.GenericAnnotation(anno.getAnnotation().codeRepr(),
              annotationArgument, getIdentifiedLocation(anno), meta());
          yield translateModuleSymbol(anno.getExpression(), join(annotation, appendTo));
        }
      }

      case Tree.Documented doc -> {
        var comment = translateComment(doc, doc.getDocumentation());
        yield translateModuleSymbol(doc.getExpression(), join(comment, appendTo));
      }

      case Tree.TypeSignature sig -> {
        var methodReference = translateMethodReference(sig.getVariable(), true);
        var signature = translateType(sig.getType());
        var ascription = new Type.Ascription(methodReference, signature, Option.empty(),
            getIdentifiedLocation(sig), meta());
        yield join(ascription, appendTo);
      }

      default -> {
        var error = translateSyntaxError(inputAst, Syntax.UnexpectedExpression$.MODULE$);
        yield join(error, appendTo);
      }
    };
  }

  private List<DefinitionArgument> translateArgumentsDefinition(
      java.util.List<ArgumentDefinition> args) throws SyntaxException {
    List<DefinitionArgument> res = nil();
    for (var p : args) {
      var d = translateArgumentDefinition(p);
      res = join(d, res);
    }
    return res.reverse();
  }

  IR translateConstructorDefinition(Tree.ConstructorDefinition cons, Tree inputAst,
      boolean isPrivate) {
    try {
      var constructorName = buildName(inputAst, cons.getConstructor());
      List<DefinitionArgument> args = translateArgumentsDefinition(cons.getArguments());
      var cAt = getIdentifiedLocation(inputAst);
      return new Definition.Data(constructorName, args, nil(), isPrivate, cAt, meta());
    } catch (SyntaxException ex) {
      return ex.toError();
    }
  }

  /**
   * Translates any expression that can be found in the body of a type declaration from [[AST]] into
   * [[IR]].
   *
   * @param exp the expression to be translated
   * @return the [[IR]] representation of `maybeParensedInput`
   */
  @SuppressWarnings("unchecked")
  private List<IR> translateTypeBodyExpression(Tree exp, List<IR> appendTo) {
    try {
      return translateTypeBodyExpressionImpl(exp, appendTo);
    } catch (SyntaxException ex) {
      return join(ex.toError(), appendTo);
    }
  }

  private List<IR> translateTypeBodyExpressionImpl(Tree exp, List<IR> appendTo)
      throws SyntaxException {
    var inputAst = maybeManyParensed(exp);
    return switch (inputAst) {
      case null -> appendTo;

      // Only private constructors and methods are supported.
      case Tree.Private priv -> {
        switch (priv.getBody()) {
          case Tree.ConstructorDefinition consDef -> {
            var translated = translateConstructorDefinition(consDef, priv, true);
            yield join(translated, appendTo);
          }
          case Tree.Function func -> {
            var translated = translateMethodInType(func, true);
            yield join(translated, appendTo);
          }
          default -> {
            var errorIr = translateSyntaxError(priv, Syntax.UnexpectedDeclarationInType$.MODULE$);
            yield join(errorIr, appendTo);
          }
        }
      }

      case Tree.ConstructorDefinition cons ->
          join(translateConstructorDefinition(cons, inputAst, false), appendTo);

      case Tree.TypeDef def -> {
        var ir = translateSyntaxError(def, Syntax.UnexpectedDeclarationInType$.MODULE$);
        yield join(ir, appendTo);
      }

      case Tree.ArgumentBlockApplication app -> {
        var ir = translateSyntaxError(app, Syntax.UnexpectedDeclarationInType$.MODULE$);
        yield join(ir, appendTo);
      }

      case Tree.TypeSignature sig -> {
        var isMethod = false;
        if (sig.getVariable() instanceof Tree.Ident ident) {
          isMethod = ident.getToken().isOperatorLexically();
        }
        var typeName = translateExpression(sig.getVariable(), isMethod);
        var ir = translateTypeSignature(sig, sig.getType(), typeName);
        yield join(ir, appendTo);
      }

      case Tree.Function fun -> {
        var ir = translateMethodInType(fun, false);
        yield join(ir, appendTo);
      }

      case Tree.ForeignFunction fn when fn.getBody() instanceof Tree.TextLiteral body -> {
        var name = buildName(fn.getName());
        var args = translateArgumentsDefinition(fn.getArgs());
        var languageName = fn.getLanguage().codeRepr();
        var language = languageName;
        if (language == null) {
          var message = "Language '" + languageName + "' is not a supported polyglot language.";
          var error = translateSyntaxError(inputAst, new Syntax.InvalidForeignDefinition(message));
          yield join(error, appendTo);
        }
        var text = buildTextConstant(body, body.getElements());
        var def =
            new Foreign.Definition(language, text, getIdentifiedLocation(fn.getBody()), meta());
        var binding =
            new Function.Binding(name, args, def, false, getIdentifiedLocation(fn), true, meta());
        yield join(binding, appendTo);
      }
      case Tree.Documented doc -> {
        var irDoc = translateComment(doc, doc.getDocumentation());
        yield translateTypeBodyExpression(doc.getExpression(), join(irDoc, appendTo));
      }

      case Tree.AnnotatedBuiltin anno -> {
        var ir = new Name.BuiltinAnnotation("@" + anno.getAnnotation().codeRepr(),
            getIdentifiedLocation(anno), meta());
        var annotation = translateAnnotation(ir, anno.getExpression(), nil());
        yield join(annotation, appendTo);
      }

      case Tree.Annotated anno -> {
        var annotationArgument = translateExpression(anno.getArgument());
        var annotation = new Name.GenericAnnotation(anno.getAnnotation().codeRepr(),
            annotationArgument, getIdentifiedLocation(anno), meta());
        yield translateTypeBodyExpression(anno.getExpression(), join(annotation, appendTo));
      }

      default -> {
        var ir = translateSyntaxError(inputAst, Syntax.UnexpectedDeclarationInType$.MODULE$);
        yield join(ir, appendTo);
      }
    };
  }

  private Expression translateMethodInType(Tree.Function func, boolean isPrivate)
      throws SyntaxException {
    Name name;
    boolean isOperator = false;
    if (func.getName() instanceof Tree.Ident ident) {
      isOperator = ident.getToken().isOperatorLexically();
      name = buildName(getIdentifiedLocation(func.getName()), ident.getToken(), isOperator);
    } else {
      name = buildNameOrQualifiedName(func.getName());
    }
    var ir = translateFunction(func, name, isOperator, func.getArgs(), func.getBody(),
        resolveReturnTypeSignature(func), isPrivate);
    return ir;
  }

  @SuppressWarnings("unchecked")
  private Application translateTypeApplication(Tree.App app) throws SyntaxException {
    List<CallArgument> args = nil();
    Tree t = app;
    Name.Literal in = null;
    while (t instanceof Tree.App tApp) {
      var typeArg = translateTypeCallArgument(tApp.getArg());
      if (typeArg.value() instanceof Name.Literal l && "in".equals(l.name())) {
        in = l.copy(
            l.copy$default$1(),
            true,
            l.copy$default$3(),
            l.copy$default$4(),
            l.copy$default$5(),
            l.copy$default$6(),
            l.copy$default$7()
        );
      } else {
        args = join(typeArg, args);
      }
      t = tApp.getFunc();
    }
    var fullQualifiedNames = qualifiedNameSegments(t, false).reverse();
    var segments = fullQualifiedNames.length();
    var type = switch (segments) {
      case 1 -> fullQualifiedNames.head();
      default -> {
        var name = fullQualifiedNames.head();
        name = new Name.Literal(name.name(), true, name.identifiedLocation(), Option.empty(), name.passData());
        List<Name> tail = (List<Name>) fullQualifiedNames.tail();
        tail = tail.reverse();
        final IdentifiedLocation loc = getIdentifiedLocation(app);
        Name arg;
        if (segments == 2) {
          arg = tail.head();
        } else {
          arg = new Name.Qualified(tail, loc, meta());
        }
        var ca = new CallArgument.Specified(Option.empty(), arg, loc, meta());
        args = join(ca, args);
        yield name;
      }
    };
    if (in == null) {
      return new Application.Prefix(type, args, false, getIdentifiedLocation(app), meta());
    } else {
      var fn = new CallArgument.Specified(Option.empty(), type, getIdentifiedLocation(app), meta());
      return new Operator.Binary(fn, in, args.head(), getIdentifiedLocation(app), meta());
    }
  }

  private Definition translateMethodBinding(Tree.Function fn, boolean isPrivate)
      throws SyntaxException {
    var methodRef = translateMethodReference(fn.getName(), false);
    var args = translateArgumentsDefinition(fn.getArgs());
    var body = translateExpression(fn.getBody());
    var bodyLoc = expandToContain(getIdentifiedLocation(fn.getBody()), body.identifiedLocation());
    body = body.setLocation(Option.apply(bodyLoc));
    var loc = getIdentifiedLocation(fn, 0, 0, null);
    var returnSignature = resolveReturnTypeSignature(fn);
    if (body == null) {
      IdentifiedLocation emptyLocation = null;
      if (methodRef.identifiedLocation() != null) {
        var identifiedLocation = methodRef.identifiedLocation();
        var location = new Location(identifiedLocation.end(), identifiedLocation.end());
        emptyLocation = new IdentifiedLocation(location, identifiedLocation.uuid());
      }
      body = new Empty(emptyLocation, meta());
    }

    String functionName = fn.getName().codeRepr();
    var ascribedBody = addTypeAscription(functionName, body, returnSignature, loc);
    var binding = new Method.Binding(
        methodRef,
        args,
        isPrivate,
        ascribedBody,
        loc,
        meta()
    );
    return binding;
  }

  private Expression translateFunction(
      Tree fun, Name name, boolean isOperator,
      java.util.List<ArgumentDefinition> arguments, final Tree treeBody, Expression returnType,
      boolean isPrivate
  ) {
    List<DefinitionArgument> args;
    try {
      args = translateArgumentsDefinition(arguments);
    } catch (SyntaxException ex) {
      return ex.toError();
    }

    var loc = getIdentifiedLocation(fun);
    var body = translateExpression(treeBody);
    String functionName = name.name();
    if (args.isEmpty()) {
      if (body instanceof Expression.Block block) {
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
      if (body == null) {
        body = translateSyntaxError(fun, Syntax.UnexpectedExpression$.MODULE$);
      }

      var ascribedBody = addTypeAscription(functionName, body, returnType, loc);
      return new Expression.Binding(name, ascribedBody, loc, meta());
    } else {
      if (body == null) {
        return translateSyntaxError(fun, Syntax.UnexpectedDeclarationInType$.MODULE$);
      }
      if (isOperator && args.size() != 2) {
        return translateSyntaxError(fun, Syntax.InvalidOperator$.MODULE$);
      }

      var ascribedBody = addTypeAscription(functionName, body, returnType, loc);
      return new Function.Binding(name, args, ascribedBody, isPrivate, loc, true, meta());
    }
  }

  /**
   * Returns the return type of a function, if it was specified inline.
   */
  private Expression resolveReturnTypeSignature(Tree.Function fun) {
    var returnSignature = fun.getReturns();
    if (returnSignature == null) {
      return null;
    }

    return translateType(returnSignature.getType());
  }

  /**
   * Wraps a body expression in a type ascription that will ensure that the type of the body is
   * checked to match the provided type.
   * <p>
   * If the type is {@code null}, the body is returned unchanged.
   */
  private Expression addTypeAscription(String functionName, Expression body, Expression type, IdentifiedLocation loc) {
    if (type == null) {
      return body;
    }

    String comment = "the result of `" + functionName + "`";
    return new Type.Ascription(body, type, Option.apply(comment), loc, meta());
  }

  private Type.Ascription translateTypeSignature(Tree sig, Tree type, Expression typeName) {
    var fn = translateType(type);
    return new Type.Ascription(typeName, fn, Option.empty(), getIdentifiedLocation(sig), meta());
  }


  /**
   * Translates a method reference from [[AST]] into [[IR]].
   *
   * @param sig the method reference to translate
   * @return the [[IR]] representation of `inputAst`
   */
  Name.MethodReference translateMethodReference(Tree sig, boolean alwaysLocation)
      throws SyntaxException {
    Name method;
    Option<Name> type;
    IdentifiedLocation loc;
    switch (sig) {
      case Tree.Ident id -> {
        type = Option.empty();
        method = buildName(id);
        loc = getIdentifiedLocation(sig);
      }
      case Tree.OprApp app when isDotOperator(app.getOpr().getRight()) -> {
        var typeLoc = getIdentifiedLocation(app.getLhs());
        type = Option.apply(buildQualifiedName(app.getLhs(), typeLoc, false));
        method = buildName(app.getRhs());
        if (alwaysLocation) {
          loc = getIdentifiedLocation(sig);
        } else {
          loc = null;
        }
      }
      default -> throw translateEntity(sig, "translateMethodReference");
    }
    return new Name.MethodReference(type, method, loc, meta());
  }

  private Expression translateCall(Tree ast, boolean isMethod) throws SyntaxException {
    var args = new java.util.ArrayList<CallArgument>();
    var hasDefaultsSuspended = false;
    var tree = ast;
    for (; ; ) {
      switch (tree) {
        case Tree.App app when app.getArg() instanceof Tree.SuspendedDefaultArguments -> {
          hasDefaultsSuspended = true;
          tree = app.getFunc();
        }
        case Tree.App app -> {
          var expr = translateExpression(app.getArg(), false);
          var loc = getIdentifiedLocation(app.getArg());
          args.add(new CallArgument.Specified(Option.empty(), expr, loc, meta()));
          tree = app.getFunc();
        }
        case Tree.NamedApp app -> {
          var expr = translateExpression(app.getArg(), false);
          var loc = getIdentifiedLocation(app.getArg());
          var id = buildName(app, app.getName());
          args.add(new CallArgument.Specified(Option.apply(id), expr, loc, meta()));
          tree = app.getFunc();
        }
        case Tree.OperatorBlockApplication app -> {
          var at = args.size();
          var self = translateExpression(app.getLhs(), false);
          for (var l : app.getExpressions()) {
            if (l.getExpression() == null) {
              continue;
            }
            var invoke = isDotOperator(l.getExpression().getOperator().getRight());
            if (self == null || !invoke) {
              return null;
            }
            var expr = switch (translateExpression(l.getExpression().getExpression(), true)) {
              case Application.Prefix pref -> {
                var arg = new CallArgument.Specified(Option.empty(), self, self.identifiedLocation(), meta());
                yield new Application.Prefix(pref.function(), join(arg, pref.arguments()), false, pref.identifiedLocation(), meta());
              }
              case Expression any -> {
                var arg = new CallArgument.Specified(Option.empty(), self, self.identifiedLocation(), meta());
                yield new Application.Prefix(any, join(arg, nil()), false, any.identifiedLocation(), meta());
              }
            };
            var loc = getIdentifiedLocation(l.getExpression().getExpression());
            args.add(at, new CallArgument.Specified(Option.empty(), expr, loc, meta()));
            self = expr;
          }
          return self;
        }
        default -> {
          Expression func;
          if (tree instanceof Tree.OprApp oprApp
              && isDotOperator(oprApp.getOpr().getRight())
              && oprApp.getRhs() instanceof Tree.Ident) {
            func = translateExpression(oprApp.getRhs(), true);
            if (oprApp.getLhs() == null && args.isEmpty()) {
              return func;
            }
            if (oprApp.getLhs() != null) {
              var self = translateExpression(oprApp.getLhs(), isMethod);
              var loc = getIdentifiedLocation(oprApp.getLhs());
              args.add(new CallArgument.Specified(Option.empty(), self, loc, meta()));
            }
          } else if (args.isEmpty()) {
            return null;
          } else {
            func = translateExpression(tree, isMethod);
          }
          java.util.Collections.reverse(args);
          var argsList = CollectionConverters.asScala(args.iterator()).toList();
          return new Application.Prefix(
              func, argsList,
              hasDefaultsSuspended,
              getIdentifiedLocation(ast),
              meta()
          );
        }
      }
    }
  }

  private Name translateOldStyleLambdaArgumentName(Tree arg, boolean[] suspended,
      Expression[] defaultValue) throws SyntaxException {
    return switch (arg) {
      case Tree.Group g ->
          translateOldStyleLambdaArgumentName(g.getBody(), suspended, defaultValue);
      case Tree.Wildcard wild ->
          new Name.Blank(getIdentifiedLocation(wild.getToken()), meta());
      case Tree.OprApp app when "=".equals(app.getOpr().getRight().codeRepr()) -> {
        if (defaultValue != null) {
          defaultValue[0] = translateExpression(app.getRhs(), false);
        }
        yield translateOldStyleLambdaArgumentName(app.getLhs(), suspended, null);
      }
      case Tree.Ident id -> {
        Expression identifier = translateIdent(id, false);
        yield switch (identifier) {
          case Name name_ -> name_;
          default -> throw translateEntity(id, "translateOldStyleLambdaArgumentName");
        };
      }
      case Tree.UnaryOprApp app when "~".equals(app.getOpr().codeRepr()) -> {
        if (suspended != null) {
          suspended[0] = true;
        }
        yield translateOldStyleLambdaArgumentName(app.getRhs(), null, defaultValue);
      }
      default -> throw translateEntity(arg, "translateOldStyleLambdaArgumentName");
    };
  }

  /**
   * Translates an arbitrary program expression from {@link Tree} into {@link IR}.
   *
   * @param tree the expression to be translated
   * @return the {@link IR} representation of `tree`
   */
  Expression translateExpression(Tree tree) {
    return translateExpression(tree, false);
  }

  Expression translateExpression(Tree tree, boolean isMethod) {
    try {
      return translateExpressionImpl(tree, isMethod);
    } catch (SyntaxException ex) {
      return ex.toError();
    }
  }

  private static <T> T useOrElse(T one, T other) {
    return one != null ? one : other;
  }

  private Expression translateExpressionImpl(Tree tree, boolean isMethod) throws SyntaxException {
    if (tree == null) {
      return null;
    }
    var callExpression = translateCall(tree, isMethod);
    if (callExpression != null) {
      return callExpression;
    }
    return switch (tree) {
      case Tree.Lambda lambda -> {
        List<DefinitionArgument> args;
        try {
          args = translateArgumentsDefinition(lambda.getArguments());
        } catch (SyntaxException ex) {
          yield ex.toError();
        }
        var body = translateExpression(lambda.getBody(), false);
        var at = getIdentifiedLocation(lambda);
        yield new Function.Lambda(args, body, at, true, meta());
      }
      case Tree.OprApp app -> {
        var op = app.getOpr().getRight();
        if (op == null) {
          var at = getIdentifiedLocation(app);
          var arr = app.getOpr().getLeft().getOperators();
          if (arr.size() > 0 && arr.get(0).codeRepr().equals("=")) {
            var errLoc = arr.size() > 1 ? getIdentifiedLocation(arr.get(1)) : at;
            var err = translateSyntaxError(errLoc, Syntax.UnrecognizedToken$.MODULE$);
            var name = buildName(app.getLhs());
            yield new Expression.Binding(name, err, at, meta());
          } else {
            yield translateSyntaxError(at, Syntax.UnrecognizedToken$.MODULE$);
          }
        }
        yield switch (op.codeRepr()) {
          case "." -> {
            final IdentifiedLocation loc = getIdentifiedLocation(tree);
            try {
              yield buildQualifiedName(app, loc, false);
            } catch (SyntaxException ex) {
              yield ex.toError();
            }
          }

          case "->" -> {
            // Old-style lambdas; this syntax will be eliminated after the parser transition is complete.
            var arg = app.getLhs();
            if (arg == null) {
              yield translateSyntaxError(app, Syntax.UnexpectedExpression$.MODULE$);
            }
            var isSuspended = new boolean[1];
            if (arg instanceof Tree.UnaryOprApp susApp && "~".equals(susApp.getOpr().codeRepr())) {
              arg = susApp.getRhs();
              isSuspended[0] = true;
            }
            var defaultValue = new Expression[1];
            Name name = translateOldStyleLambdaArgumentName(arg, isSuspended, defaultValue);
            var arg_ = new DefinitionArgument.Specified(
                name,
                Option.empty(),
                Option.apply(defaultValue[0]),
                isSuspended[0],
                getIdentifiedLocation(arg),
                meta()
            );
            List<DefinitionArgument> args = join(arg_, nil());
            var body = translateExpression(app.getRhs(), false);
            if (body == null) {
              body = new Expression.Block(
                  nil(),
                  new Name.Blank(null, meta()),
                  null,
                  true,
                  meta()
              );
            }
            var at = expandToContain(switch (body) {
              case Expression.Block __ -> getIdentifiedLocation(tree, 0, 1, null);
              default -> getIdentifiedLocation(tree);
            }, body.identifiedLocation());
            yield new Function.Lambda(args, body, at, true, meta());
          }
          default -> {
            var lhs = unnamedCallArgument(app.getLhs());
            var rhs = unnamedCallArgument(app.getRhs());
            var loc = getIdentifiedLocation(app);
            var ir = applyOperator(op, lhs, rhs, loc);
            attachTranslatedWarnings(ir, app);
            yield ir;
          }
        };
      }
      case Tree.OperatorBlockApplication app -> {
        Expression expr = null;
        var lhs = unnamedCallArgument(app.getLhs());
        for (var l : app.getExpressions()) {
          var op = l.getExpression().getOperator().getRight();
          if (op == null || isDotOperator(op)) {
            yield translateSyntaxError(l.getExpression().getExpression(),
                Syntax.UnexpectedExpression$.MODULE$);
          }
          var rhs = unnamedCallArgument(l.getExpression().getExpression());
          var loc = getIdentifiedLocation(app);
          var both = applyOperator(op, lhs, rhs, loc);
          expr = both;
          lhs = new CallArgument.Specified(Option.empty(), expr, loc, meta());
        }
        yield expr;
      }
      case Tree.Array arr -> {
        List<Expression> items = nil();
        if (arr.getFirst() != null) {
          var exp = translateExpression(arr.getFirst(), false);
          items = join(exp, items);
          for (var next : arr.getRest()) {
            exp = translateExpression(next.getBody(), false);
            if (exp == null) {
              yield translateSyntaxError(arr, Syntax.UnexpectedExpression$.MODULE$);
            }
            items = join(exp, items);
          }
        }
        yield new Application.Sequence(
            items.reverse(),
            getIdentifiedLocation(arr),
            meta()
        );
      }
      case Tree.Number n -> translateNumber(n);
      case Tree.Ident id -> translateIdent(id, isMethod);
      case Tree.MultiSegmentApp app -> {
        var fnName = new StringBuilder();
        var sep = "";
        List<CallArgument> args = nil();
        for (var seg : app.getSegments()) {
          var id = seg.getHeader().codeRepr();
          fnName.append(sep);
          fnName.append(id);

          var body = unnamedCallArgument(seg.getBody());
          args = join(body, args);

          sep = "_";
        }
        var fullName = fnName.toString();
        if (fullName.equals(FREEZE_MACRO_IDENTIFIER)) {
          yield translateExpression(app.getSegments().get(0).getBody(), false);
        } else if (fullName.equals(SKIP_MACRO_IDENTIFIER)) {
          var body = app.getSegments().get(0).getBody();
          var subexpression = useOrElse(applySkip(body), body);
          yield translateExpression(subexpression, false);
        }
        var fn = new Name.Literal(fullName, true, null, Option.empty(), meta());
        if (!checkArgs(args)) {
          yield translateSyntaxError(app, Syntax.UnexpectedExpression$.MODULE$);
        }
        yield new Application.Prefix(fn, args.reverse(), false, getIdentifiedLocation(tree), meta());
      }
      case Tree.BodyBlock body -> translateBodyBlock(body, false);
      case Tree.Assignment assign -> {
        var name = buildNameOrQualifiedName(assign.getPattern());
        var expr = translateExpression(assign.getExpr(), false);
        if (expr == null) {
          expr = translateSyntaxError(assign, Syntax.UnexpectedExpression$.MODULE$);
        }
        yield new Expression.Binding(name, expr, getIdentifiedLocation(tree), meta());
      }
      case Tree.ArgumentBlockApplication body -> {
        List<Expression> expressions = nil();
        Expression last = null;
        for (var line : body.getArguments()) {
          final Tree expr = line.getExpression();
          if (expr == null) {
            continue;
          }
          if (last != null) {
            expressions = join(last, expressions);
          }
          last = translateExpression(expr, false);
        }
        if (last == null) {
          last = new Name.Blank(null, meta());
        }
        var block =
            new Expression.Block(expressions.reverse(), last, getIdentifiedLocation(body), false, meta());
        if (body.getLhs() != null) {
          var fn = translateExpression(body.getLhs(), isMethod);
          List<CallArgument> args = nil();
          for (var line : body.getArguments()) {
            var expr = line.getExpression();
            if (expr instanceof Tree.Ident) {
              var call = translateCallArgument(expr);
              args = join(call, args);
            }
          }
          yield switch (fn) {
            case Application.Prefix pref -> patchPrefixWithBlock(pref, block, args);
            default -> block;
          };
        } else {
          yield block;
        }
      }
      case Tree.TypeAnnotated anno -> translateTypeAnnotated(anno);
      case Tree.Group group -> {
        yield switch (translateExpression(group.getBody(), false)) {
          case null -> translateSyntaxError(group, Syntax.EmptyParentheses$.MODULE$);
          case Application.Prefix pref -> {
            final IdentifiedLocation groupWithoutParenthesis = getIdentifiedLocation(group, 1, -1, pref.getExternalId());
            yield pref.setLocation(Option.apply(groupWithoutParenthesis));
          }
          case Expression in -> in;
        };
      }
      case Tree.TextLiteral txt -> {
        try {
          yield translateLiteral(txt);
        } catch (SyntaxException ex) {
          yield ex.toError();
        }
      }
      case Tree.CaseOf cas -> {
        var expr = translateExpression(cas.getExpression(), false);
        List<Case.Branch> branches = nil();
        for (var line : cas.getCases()) {
          if (line.getCase() == null) {
            continue;
          }
          var branch = line.getCase();
          if (branch.getDocumentation() != null) {
            var comment = translateComment(cas, branch.getDocumentation());
            var loc = getIdentifiedLocation(cas);
            var doc = new Pattern.Documentation(comment.doc(), loc, meta());
            var br = new Case.Branch(
                doc,
                new Empty(null, meta()), true, loc, meta()
            );
            branches = join(br, branches);
          }
          // A branch with no expression is used to hold any orphaned documentation at the end of the case-of
          // expression, with no case to attach it to.
          if (branch.getExpression() != null) {
            var br = new Case.Branch(
                translatePattern(branch.getPattern()),
                translateExpression(branch.getExpression(), false),
                true,
                getIdentifiedLocation(branch.getExpression()), meta()
            );
            branches = join(br, branches);
          }
        }
        yield new Case.Expr(expr, branches.reverse(), false, getIdentifiedLocation(tree), meta());
      }
      case Tree.Function fun -> {
        var name = buildName(fun.getName());
        boolean isOperator = false;
        if (fun.getName() instanceof Tree.Ident ident) {
          isOperator = ident.getToken().isOperatorLexically();
        }
        yield translateFunction(fun, name, isOperator, fun.getArgs(), fun.getBody(),
            resolveReturnTypeSignature(fun), false);
      }
      case Tree.OprSectionBoundary bound -> translateExpression(bound.getAst(), false);
      case Tree.UnaryOprApp un when "-".equals(un.getOpr().codeRepr()) ->
          switch (translateExpression(un.getRhs(), false)) {
            case Literal.Number n -> n.copy(
                n.copy$default$1(),
                "-" + n.copy$default$2(),
                n.copy$default$3(),
                n.copy$default$4(),
                n.copy$default$5(),
                n.copy$default$6()
            );
            case Expression expr -> {
              var negate = new Name.Literal("negate", true, null, Option.empty(), meta());
              var arg = new CallArgument.Specified(Option.empty(), expr, expr.identifiedLocation(), meta());
              yield new Application.Prefix(negate, join(arg, nil()), false, expr.identifiedLocation(), meta());
            }
            case null ->
                translateSyntaxError(tree, new Syntax.UnsupportedSyntax("Strange unary -"));
          };
      case Tree.TypeSignature sig -> {
        var methodName = buildName(sig.getVariable());
        var methodReference = new CallArgument.Specified(
            Option.empty(),
            methodName,
            methodName.identifiedLocation(),
            meta()
        );
        var opName = buildName(null, sig.getOperator(), true);
        var signature = translateTypeCallArgument(sig.getType());
        yield new Operator.Binary(methodReference, opName, signature, getIdentifiedLocation(sig), meta());
      }
      case Tree.TemplateFunction templ -> translateExpression(templ.getAst(), false);
      case Tree.Wildcard wild -> new Name.Blank(getIdentifiedLocation(wild), meta());
      case Tree.AnnotatedBuiltin anno -> {
        var ir = new Name.BuiltinAnnotation("@" + anno.getAnnotation().codeRepr(),
            getIdentifiedLocation(anno), meta());
        yield translateAnnotation(ir, anno.getExpression(), nil());
      }
      // Documentation can be attached to an expression in a few cases, like if someone documents a line of an
      // `ArgumentBlockApplication`. The documentation is ignored.
      case Tree.Documented docu -> translateExpression(docu.getExpression());
      case Tree.App app -> {
        var fn = translateExpression(app.getFunc(), isMethod);
        var loc = getIdentifiedLocation(app);
        if (app.getArg() instanceof Tree.SuspendedDefaultArguments) {
          yield new Application.Prefix(fn, nil(), true, loc, meta());
        } else {
          yield fn.setLocation(Option.apply(loc));
        }
      }
      case Tree.AutoscopedIdentifier autoscopedIdentifier -> {
        var methodName = buildName(autoscopedIdentifier.getIdent());
        yield new Name.MethodReference(
            Option.empty(),
            methodName,
            getIdentifiedLocation(autoscopedIdentifier),
            meta()
        );
      }
      case Tree.Private __ -> translateSyntaxError(tree, Syntax.UnexpectedExpression$.MODULE$);
      case Tree.Invalid __ -> translateSyntaxError(tree, Syntax.UnexpectedExpression$.MODULE$);
      default -> translateSyntaxError(tree, new Syntax.UnsupportedSyntax("translateExpression"));
    };
  }

  private Expression.Block translateBodyBlock(Tree.BodyBlock body, boolean suspended) {
    var expressions = new java.util.ArrayList<Expression>();
    Expression last = null;
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
        Expression commentIr;
        try {
          commentIr = translateComment(doc, doc.getDocumentation());
        } catch (SyntaxException ex) {
          commentIr = ex.toError();
        }
        expressions.add(commentIr);
      }
      last = translateExpression(expr, false);
    }
    var locationWithANewLine = getIdentifiedLocation(body, 0, 0, null);
    if (last == null) {
      if (expressions.isEmpty()) {
        last = new Empty(locationWithANewLine, meta());
      } else {
        last = expressions.get(expressions.size() - 1);
        expressions.remove(expressions.size() - 1);
      }
    }
    var list = CollectionConverters.asScala(expressions.iterator()).toList();
    if (last != null
            && last.location().isDefined()
            && last.location().get().end() != locationWithANewLine.end()) {
      int start = last.location().get().start();
      int end = locationWithANewLine.end() - 1;
      var id = new IdentifiedLocation(start, end, last.location().get().uuid());
      last = last.setLocation(Option.apply(id));
    }
    return new Expression.Block(list, last, locationWithANewLine, suspended, meta());
  }

  private void attachTranslatedWarnings(IR ir, Tree tree) {
    for (var warning : tree.getWarnings()) {
      var message = Parser.getWarningMessage(warning);
      var irWarning = new Warning.Syntax(ir, message);
      ir.getDiagnostics().add(irWarning);
    }
  }

  private Operator applyOperator(Token.Operator op, CallArgument lhs, CallArgument rhs, IdentifiedLocation loc) {
    var name = new Name.Literal(
        op.codeRepr(), true, getIdentifiedLocation(op), Option.empty(), meta()
    );
    if (lhs == null && rhs == null) {
      return new Section.Sides(name, loc, meta());
    } else if (lhs == null) {
      return new Section.Right(name, rhs, loc, meta());
    } else if (rhs == null) {
      return new Section.Left(lhs, name, loc, meta());
    } else {
      return new Operator.Binary(lhs, name, rhs, loc, meta());
    }
  }

  Tree applySkip(Tree tree) {
    // Termination:
    // Every iteration either breaks, or reduces [`tree`] to a substructure of [`tree`].
    var done = false;
    while (!done && tree != null) {
      tree = switch (tree) {
        case Tree.MultiSegmentApp app
            when FREEZE_MACRO_IDENTIFIER.equals(app.getSegments().get(0).getHeader().codeRepr()) ->
            app.getSegments().get(0).getBody();
        case Tree.Invalid ignored -> null;
        case Tree.BodyBlock ignored -> null;
        case Tree.Number ignored -> null;
        case Tree.Wildcard ignored -> null;
        case Tree.SuspendedDefaultArguments ignored -> null;
        case Tree.ForeignFunction ignored -> null;
        case Tree.Import ignored -> null;
        case Tree.Export ignored -> null;
        case Tree.TypeDef ignored -> null;
        case Tree.TypeSignature ignored -> null;
        case Tree.ArgumentBlockApplication app -> app.getLhs();
        case Tree.OperatorBlockApplication app -> app.getLhs();
        case Tree.OprApp app -> app.getLhs();
        case Tree.Ident ident when ident.getToken().isTypeOrConstructor() -> null;
        case Tree.Ident ignored -> {
          done = true;
          yield tree;
        }
        case Tree.Group ignored -> {
          done = true;
          yield tree;
        }
        case Tree.UnaryOprApp app -> app.getRhs();
        case Tree.OprSectionBoundary section -> section.getAst();
        case Tree.TemplateFunction function -> function.getAst();
        case Tree.AnnotatedBuiltin annotated -> annotated.getExpression();
        case Tree.Documented documented -> documented.getExpression();
        case Tree.Assignment assignment -> assignment.getExpr();
        case Tree.TypeAnnotated annotated -> annotated.getExpression();
        case Tree.App app when isApplication(app.getFunc()) -> app.getFunc();
        case Tree.NamedApp app when isApplication(app.getFunc()) -> app.getFunc();
        case Tree.App app -> useOrElse(applySkip(app.getFunc()), app.getArg());
        case Tree.NamedApp app -> useOrElse(applySkip(app.getFunc()), app.getArg());
        case Tree.MultiSegmentApp ignored -> null;
        case Tree.TextLiteral ignored -> null;
        case Tree.Function ignored -> null;
        case Tree.Lambda ignored -> null;
        case Tree.CaseOf ignored -> null;
        case Tree.Array ignored -> null;
        case Tree.Tuple ignored -> null;
        default -> null;
      };
    }
    return tree;
  }

  boolean isApplication(Tree tree) {
    return switch (tree) {
      case Tree.App ignored -> true;
      case Tree.NamedApp ignored -> true;
      default -> false;
    };
  }

  Expression translateType(Tree tree) {
    return switch (tree) {
      case null -> null;
      case Tree.App app -> {
        try {
          yield translateTypeApplication(app);
        } catch (SyntaxException ex) {
          yield ex.toError();
        }
      }
      case Tree.OprApp app -> {
        var op = app.getOpr().getRight();
        if (op == null) {
          yield translateSyntaxError(app, Syntax.UnexpectedExpression$.MODULE$);
        }
        yield switch (op.codeRepr()) {
          case "." -> {
            final IdentifiedLocation loc = getIdentifiedLocation(tree);
            try {
              yield buildQualifiedName(app, loc, false);
            } catch (SyntaxException ex) {
              yield ex.toError();
            }
          }
          case "->" -> {
            var literal = translateType(app.getLhs());
            var body = translateType(app.getRhs());
            if (body == null) {
              yield new Syntax(getIdentifiedLocation(app), Syntax.UnexpectedExpression$.MODULE$, meta());
            }
            var args = switch (body) {
              case Type.Function fn -> {
                body = fn.result();
                yield join(literal, fn.args());
              }
              default -> join(literal, nil());
            };
            yield new Type.Function(args, body, null, meta());
          }
          default -> {
            var lhs = translateTypeCallArgument(app.getLhs());
            var rhs = translateTypeCallArgument(app.getRhs());
            var name = new Name.Literal(
                op.codeRepr(), true,
                getIdentifiedLocation(app),
                Option.empty(),
                meta()
            );
            var loc = getIdentifiedLocation(app);
            yield new Operator.Binary(lhs, name, rhs, loc, meta());
          }
        };
      }
      case Tree.Array arr -> {
        List<Expression> items = nil();
        if (arr.getFirst() != null) {
          var exp = translateType(arr.getFirst());
          items = join(exp, items);
          for (var next : arr.getRest()) {
            exp = translateType(next.getBody());
            items = join(exp, items);
          }
        }
        yield new Application.Literal.Sequence(
            items.reverse(),
            getIdentifiedLocation(arr),
            meta()
        );
      }
      case Tree.Ident id -> buildName(getIdentifiedLocation(id), id.getToken(), false);
      case Tree.Group group -> translateType(group.getBody());
      case Tree.UnaryOprApp un -> translateType(un.getRhs());
      case Tree.Wildcard wild -> new Name.Blank(getIdentifiedLocation(wild), meta());
      case Tree.TypeAnnotated anno -> translateTypeAnnotatedToOperator(anno);
      default -> translateSyntaxError(tree, new Syntax.UnsupportedSyntax("translateType"));
    };
  }

  /**
   * Translate a type-annotated expression.
   */
  Expression translateTypeAnnotated(Tree.TypeAnnotated anno) {
    var type = translateType(anno.getType());
    var expr = translateExpression(anno.getExpression());
    return new Type.Ascription(expr, type, Option.empty(), getIdentifiedLocation(anno), meta());
  }

  /**
   * Translate a type-annotated expression in a context where the IR is a generic binary operator.
   */
  Expression translateTypeAnnotatedToOperator(Tree.TypeAnnotated anno) {
    var type = translateTypeCallArgument(anno.getType());
    var expr = translateCallArgument(anno.getExpression());
    var opName = new Name.Literal(anno.getOperator().codeRepr(), true, null, Option.empty(), meta());
    return new Operator.Binary(
        expr,
        opName,
        type,
        getIdentifiedLocation(anno),
        meta());
  }

  @SuppressWarnings("unchecked")
  private Expression patchPrefixWithBlock(Application.Prefix pref, Expression.Block block,
      List<CallArgument> args) {
    if (block.expressions().isEmpty() && block.returnValue() instanceof Name.Blank) {
      return pref;
    }
    if (args.nonEmpty() && args.head() == null) {
      args = (List<CallArgument>) args.tail();
    }
    List<CallArgument> allArgs = (List<CallArgument>) pref.arguments().appendedAll(args.reverse());
    final CallArgument.Specified blockArg = new CallArgument.Specified(Option.empty(), block, block.identifiedLocation(), meta());
    List<CallArgument> withBlockArgs = (List<CallArgument>) allArgs.appended(blockArg);
    if (!checkArgs(withBlockArgs)) {
      return translateSyntaxError(pref.location().get(), Syntax.UnexpectedExpression$.MODULE$);
    }
    return new Application.Prefix(pref.function(), withBlockArgs, pref.hasDefaultsSuspended(), pref.identifiedLocation(), meta());
  }

  private Application.Prefix translateAnnotation(Name.BuiltinAnnotation ir, Tree expr,
      List<CallArgument> callArgs) {
    return switch (expr) {
      case Tree.App fn -> {
        var fnAsArg = translateCallArgument(fn.getArg());
        yield translateAnnotation(ir, fn.getFunc(), join(fnAsArg, callArgs));
      }
      case Tree.NamedApp fn -> {
        var fnAsArg = translateCallArgument(fn);
        yield translateAnnotation(ir, fn.getFunc(), join(fnAsArg, callArgs));
      }
      case Tree.ArgumentBlockApplication fn -> {
        var fnAsArg = translateCallArgument(fn.getLhs());
        var arg = translateCallArgument(expr);
        callArgs = join(fnAsArg, join(arg, callArgs));
        yield translateAnnotation(ir, null, callArgs);
      }
      case null -> {
        yield new Application.Prefix(ir, callArgs, false, ir.identifiedLocation(), meta());
      }
      default -> {
        var arg = translateCallArgument(expr);
        callArgs = join(arg, callArgs);
        yield translateAnnotation(ir, null, callArgs);
      }
    };
  }

  Expression translateNumber(Tree.Number ast) {
    var intPart = ast.getInteger();
    final Option<String> base = switch (intPart.getBase()) {
      case Base.Binary b -> Option.apply("2");
      case Base.Hexadecimal b -> Option.apply("16");
      case Base.Octal b -> Option.apply("8");
      case null -> Option.empty();
      default -> Option.empty();
    };
    var fracPart = ast.getFractionalDigits();
    String literal = fracPart != null ? intPart.codeRepr() + "." + fracPart.getDigits().codeRepr()
        : intPart.codeRepr();
    return new Literal.Number(base, literal, getIdentifiedLocation(ast), meta());
  }

  Literal translateLiteral(Tree.TextLiteral txt) throws SyntaxException {
    if (txt.getClose() == null) {
      if (txt.getOpen() == null || switch (txt.getOpen().codeRepr()) {
        case "'''" -> false;
        case "\"\"\"" -> false;
        default -> true;
      }) {
        throw new SyntaxException(txt, Syntax.UnclosedTextLiteral$.MODULE$);
      }
    }
    // Splices are not yet supported in the IR.
    var value = buildTextConstant(txt, txt.getElements());
    return new Literal.Text(value, getIdentifiedLocation(txt), meta());
  }

  private String buildTextConstant(Tree at, Iterable<TextElement> elements) throws SyntaxException {
    var sb = new StringBuilder();
    TextElement error = null;
    for (var t : elements) {
      switch (t) {
        case TextElement.Section s -> sb.append(s.getText().codeRepr());
        case TextElement.Escape e -> {
          var val = e.getToken().getValue();
          if (val == -1) {
            error = t;
          } else {
            sb.appendCodePoint(val);
          }
        }
        case TextElement.Newline n -> sb.append('\n');
        default -> throw translateEntity(at, "buildTextConstant");
      }
    }
    if (error != null) {
      throw translateEntity(at, new Syntax.InvalidEscapeSequence(sb.toString()));
    }
    return sb.toString();
  }


  /**
   * Translates an argument definition from [[AST]] into [[IR]].
   *
   * @param def the argument to translate
   * @return the [[IR]] representation of `arg`
   */
  DefinitionArgument translateArgumentDefinition(ArgumentDefinition def) throws SyntaxException {
    Tree pattern = def.getPattern();
    Name name = switch (pattern) {
      case Tree.Wildcard wild ->
          new Name.Blank(getIdentifiedLocation(wild.getToken()), meta());
      case Tree.Ident id -> {
        Expression identifier = translateIdent(id, false);
        yield switch (identifier) {
          case Name name_ -> name_;
          // TODO: Other types of pattern. Needs IR support.
          default -> throw translateEntity(pattern, "translateArgumentDefinition");
        };
      }
      // TODO: Other types of pattern. Needs IR support.
      default -> throw translateEntity(pattern, "translateArgumentDefinition");
    };
    boolean isSuspended = def.getSuspension() != null;
    var ascribedType = Option.apply(def.getType())
        .map(ascription -> translateType(ascription.getType()));
    var defaultValue = Option.apply(def.getDefault())
        .map(default_ -> translateExpression(default_.getExpression(), false));
    return new DefinitionArgument.Specified(
        name,
        ascribedType,
        defaultValue,
        isSuspended,
        getIdentifiedLocation(def),
        meta()
    );
  }

  /**
   * Translates a call-site function argument from its [[AST]] representation into [[IR]].
   *
   * @param arg the argument to translate
   * @return the [[IR]] representation of `arg`
   */
  CallArgument.Specified translateCallArgument(Tree arg) {
    var loc = getIdentifiedLocation(arg);
    return switch (arg) {
      case Tree.NamedApp app -> {
        var expr = translateExpression(app.getArg(), false);
        var id = sanitizeName(buildName(app, app.getName()));
        yield new CallArgument.Specified(Option.apply(id), expr, loc, meta());
      }
      case null -> null;
      default -> {
        var expr = translateExpression(arg, false);
        yield new CallArgument.Specified(Option.empty(), expr, loc, meta());
      }
    };
  }

  CallArgument.Specified translateTypeCallArgument(Tree arg) {
    var loc = getIdentifiedLocation(arg);
    var expr = translateType(arg);
    return new CallArgument.Specified(Option.empty(), expr, loc, meta());
  }

  CallArgument.Specified unnamedCallArgument(Tree arg) {
    if (arg == null) {
      return null;
    }
    var loc = getIdentifiedLocation(arg);
    var expr = translateExpression(arg);
    return new CallArgument.Specified(Option.empty(), expr, loc, meta());
  }

  /**
   * Translates an arbitrary program identifier from its [[AST]] representation into [[IR]].
   *
   * @param identifier the identifier to translate
   * @return the [[IR]] representation of `identifier`
   */
  Expression translateIdent(Tree identifier, boolean isMethod) {
    return switch (identifier) {
      case null -> null;
      case Tree.Ident id ->
          sanitizeName(buildName(getIdentifiedLocation(id), id.getToken(), isMethod));
      default -> translateSyntaxError(identifier, new Syntax.UnsupportedSyntax("translateIdent"));
    };
  }

  /**
   * Translates a pattern in a case expression from its [[AST]] representation into [[IR]].
   *
   * @param block the case pattern to translate
   */
  Pattern translatePattern(Tree block) throws SyntaxException {
    var pattern = maybeManyParensed(block);
    var elements = unrollApp(pattern);
    var fields = translatePatternFields(elements.subList(1, elements.size()));
    return switch (elements.get(0)) {
      case Tree.Ident id when id.getToken().isTypeOrConstructor() || !fields.isEmpty() -> {
        yield new Pattern.Constructor(
            sanitizeName(buildName(id)), fields,
            getIdentifiedLocation(id), meta()
        );
      }
      case Tree.Ident id ->
          new Pattern.Name(buildName(id), getIdentifiedLocation(id), meta());
      case Tree.OprApp app when isDotOperator(app.getOpr().getRight()) -> {
        var qualifiedName = buildQualifiedName(app);
        yield new Pattern.Constructor(
            qualifiedName, fields, getIdentifiedLocation(app), meta()
        );
      }
      case Tree.Wildcard wild -> translateWildcardPattern(wild);
      case Tree.TextLiteral lit ->
          new Pattern.Literal(translateLiteral(lit), getIdentifiedLocation(lit), meta());
      case Tree.Number num ->
          new Pattern.Literal((Literal) translateNumber(num), getIdentifiedLocation(num), meta());
      case Tree.UnaryOprApp num when num.getOpr().codeRepr().equals("-") -> {
        var n = (Literal.Number) translateExpression(num.getRhs());
        var t = n.copy(
            n.copy$default$1(),
            "-" + n.copy$default$2(),
            n.copy$default$3(),
            n.copy$default$4(),
            n.copy$default$5(),
            n.copy$default$6()
        );
        yield new Pattern.Literal(t, getIdentifiedLocation(num), meta());
      }
      case Tree.TypeAnnotated anno -> {
        var type = buildNameOrQualifiedName(maybeManyParensed(anno.getType()));
        var expr = buildNameOrQualifiedName(maybeManyParensed(anno.getExpression()));
        yield new Pattern.Type(expr, type instanceof Name ? (Name) type : null, null, meta());
      }
      case Tree.Group group -> translatePattern(group.getBody());
      default -> throw translateEntity(pattern, "translatePattern");
    };
  }

  private List<Pattern> translatePatternFields(java.util.List<Tree> tail) throws SyntaxException {
    List<Pattern> args = nil();
    for (var t : tail) {
      var p = translatePattern(t);
      args = join(p, args);
    }
    var fields = args.reverse();
    return fields;
  }

  private Pattern.Name translateWildcardPattern(Tree.Wildcard wild) {
    var at = getIdentifiedLocation(wild);
    var blank = new Name.Blank(at, meta());
    return new Pattern.Name(blank, at, meta());
  }

  private Name.Qualified buildQualifiedName(Tree t) throws SyntaxException {
    return buildQualifiedName(t, null, false);
  }

  private Name.Qualified buildQualifiedName(Tree t, IdentifiedLocation loc, boolean generateId) throws SyntaxException {
    return new Name.Qualified(qualifiedNameSegments(t, generateId), loc, meta());
  }

  private Name buildNameOrQualifiedName(Tree t) throws SyntaxException {
    return buildNameOrQualifiedName(t, null);
  }

  private Name buildNameOrQualifiedName(Tree t, IdentifiedLocation loc) throws SyntaxException {
    var segments = qualifiedNameSegments(t, false);
    if (segments.length() == 1) {
      return segments.head();
    } else {
      return new Name.Qualified(segments, loc, meta());
    }
  }

  private java.util.List<Tree> unrollOprRhs(Tree list, String operator) throws SyntaxException {
    var segments = new java.util.ArrayList<Tree>();
    while (list instanceof Tree.OprApp) {
      var app = (Tree.OprApp) list;
      if (app.getOpr().getRight() == null || !operator.equals(app.getOpr().getRight().codeRepr())) {
        break;
      }
      if (app.getRhs() != null) {
        segments.add(app.getRhs());
      } else {
        throw translateEntity(app, Syntax.UnexpectedExpression$.MODULE$);
      }
      if (app.getLhs() != null) {
        list = app.getLhs();
      } else {
        throw translateEntity(app, Syntax.UnexpectedExpression$.MODULE$);
      }
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

  private Name qualifiedNameSegment(Tree tree, boolean generateId) throws SyntaxException {
    return switch (tree) {
      case Tree.Ident id -> sanitizeName(buildName(id, generateId));
      case Tree.Wildcard wild ->
          new Name.Blank(getIdentifiedLocation(wild.getToken(), generateId), meta());
      default -> throw translateEntity(tree, "qualifiedNameSegment");
    };
  }

  private List<Name> qualifiedNameSegments(Tree t, boolean generateId) throws SyntaxException {
    List<Name> result = nil();
    var first = true;
    for (var segment : unrollOprRhs(t, ".")) {
      var qns = switch (qualifiedNameSegment(segment, generateId)) {
        case Name.Blank underscore -> {
          if (first) {
            yield underscore;
          } else {
            throw new SyntaxException(segment, Syntax.InvalidUnderscore$.MODULE$);
          }
        }
        case Name any -> any;
      };
      result = join(qns, result);
      first = false;
    }
    return result.reverse();
  }

  private List<Name.Literal> buildNameSequence(Tree t) throws SyntaxException {
    List<Name.Literal> res = nil();
    for (var segment : unrollOprRhs(t, ",")) {
      var n = buildName(segment, true);
      res = join(n, res);
    }
    return res.reverse();
  }

  /**
   * Translates an import statement from its [[AST]] representation into [[IR]].
   *
   * @param imp the import to translate
   * @return the [[IR]] representation of `imp`
   */
  @SuppressWarnings("unchecked")
  Import translateImport(Tree.Import imp) {
    try {
      Option<Name.Literal> rename;
      if (imp.getAs() == null) {
        rename = Option.empty();
      } else {
        rename = Option.apply(buildName(imp.getAs().getBody(), true));
      }
      if (imp.getPolyglot() != null) {
        if (!imp.getPolyglot().getBody().codeRepr().equals("java")) {
          return translateSyntaxError(imp, Syntax.UnrecognizedToken$.MODULE$);
        }
        List<Name> qualifiedName = qualifiedNameSegments(imp.getImport().getBody(), true);
        StringBuilder pkg = new StringBuilder();
        String cls = extractPackageAndName(qualifiedName, pkg);
        return new Polyglot(
            new Polyglot.Java(pkg.toString(), cls),
            rename.map(name -> name.name()),
            getIdentifiedLocation(imp),
            meta()
        );
      }
      var isAll = imp.getAll() != null;
      Name.Qualified qualifiedName;
      Option<List<Name.Literal>> onlyNames = Option.empty();
      if (imp.getFrom() != null) {
        qualifiedName = buildQualifiedName(imp.getFrom().getBody(), null, true);
        if (!isAll) {
          onlyNames = Option.apply(buildNameSequence(imp.getImport().getBody()));
        }
      } else {
        qualifiedName = buildQualifiedName(imp.getImport().getBody(), null, true);
      }
      Option<List<Name.Literal>> hidingNames;
      if (imp.getHiding() == null) {
        hidingNames = Option.empty();
      } else {
        hidingNames = Option.apply(buildNameSequence(imp.getHiding().getBody()));
      }
      return new Import.Module(
          qualifiedName, rename, isAll || onlyNames.isDefined() || hidingNames.isDefined(),
          onlyNames,
          hidingNames, getIdentifiedLocation(imp), false,
          meta()
      );
    } catch (SyntaxException err) {
      if (err.where instanceof Invalid invalid) {
        return err.toError(invalidImportReason(invalid.getError()));
      } else {
        return err.toError(invalidImportReason(null));
      }
    }
  }

  private Syntax.Reason invalidImportReason(String msg) {
    return new Syntax.InvalidImport(
        useOrElse(msg, "Imports must have a valid module path"));
  }

  private Syntax.Reason invalidExportReason(String msg) {
    return new Syntax.InvalidExport(
        useOrElse(msg, "Exports must have a valid module path"));
  }

  @SuppressWarnings("unchecked")
  private String extractPackageAndName(List<Name> qualifiedName, StringBuilder pkg) {
    String cls = null;
    for (List<Name> next = qualifiedName; !next.isEmpty(); ) {
      if (cls != null) {
        if (pkg.length() != 0) {
          pkg.append(".");
        }
        pkg.append(cls);
      }
      cls = next.head().name();
      next = (List<Name>) next.tail();
    }
    return cls;
  }

  /**
   * Translates an export statement from its [[AST]] representation into [[IR]].
   *
   * @param exp the export to translate
   * @return the [[IR]] representation of `imp`
   */
  @SuppressWarnings("unchecked")
  Export translateExport(Tree.Export exp) {
    try {
      Option<Name.Literal> rename;
      if (exp.getAs() == null) {
        rename = Option.empty();
      } else {
        rename = Option.apply(buildName(exp.getAs().getBody(), true));
      }
      Name.Qualified qualifiedName;
      Option<List<Name.Literal>> onlyNames = Option.empty();
      if (exp.getFrom() != null) {
        qualifiedName = buildQualifiedName(exp.getFrom().getBody(), null, true);
        var onlyBodies = exp.getExport().getBody();
        onlyNames = Option.apply(buildNameSequence(onlyBodies));
      } else {
        qualifiedName = buildQualifiedName(exp.getExport().getBody(), null, true);
      }
      return new Export.Module(
          qualifiedName, rename, onlyNames,
          getIdentifiedLocation(exp), false,
          meta()
      );
    } catch (SyntaxException err) {
      if (err.where instanceof Invalid invalid) {
        return err.toError(invalidExportReason(invalid.getError()));
      } else {
        return err.toError(invalidExportReason(null));
      }
    }
  }

  /**
   * Translates a comment from its [[AST]] representation into its [[IR]] representation.
   *
   * @param doc the comment to transform
   * @return the [[IR]] representation of `comment`
   */
  Comment.Documentation translateComment(Tree where, DocComment doc) throws SyntaxException {
    var text = buildTextConstant(where, doc.getElements());
    return new Comment.Documentation(text, getIdentifiedLocation(where), meta());
  }

  Syntax translateSyntaxError(Tree where, Syntax.Reason reason) {
    var at = getIdentifiedLocation(where);
    return new Syntax(at, reason, meta());
  }

  Syntax translateSyntaxError(IdentifiedLocation where, Syntax.Reason reason) {
    return new Syntax(where, reason, meta());
  }

  SyntaxException translateEntity(Tree where, String msg) throws SyntaxException {
    var reason = new Syntax.UnsupportedSyntax(msg);
    throw new SyntaxException(where, reason);
  }

  SyntaxException translateEntity(Tree where, Syntax.Reason reason) throws SyntaxException {
    throw new SyntaxException(where, reason);
  }

  private Name.Literal buildName(Token name) {
    return buildName(getIdentifiedLocation(name), name, false);
  }

  private Name.Literal buildName(Token name, boolean generateId) {
    return buildName(getIdentifiedLocation(name, generateId), name, false);
  }

  private Name.Literal buildName(Tree ident) throws SyntaxException {
    return buildName(ident, false);
  }

  private Name.Literal buildName(Tree ident, boolean generateId) throws SyntaxException {
    return switch (ident) {
      case Tree.Ident id ->
          buildName(getIdentifiedLocation(ident, generateId), id.getToken(), false);
      default -> throw translateEntity(ident, "buildName");
    };
  }

  private Name.Literal buildName(Tree ident, Token id) {
    return buildName(getIdentifiedLocation(ident), id, false);
  }

  private Name.Literal buildName(IdentifiedLocation loc, Token id, boolean isMethod) {
    final String name = id.codeRepr();
    return new Name.Literal(name, isMethod, loc, Option.empty(), meta());
  }

  private Name sanitizeName(Name.Literal id) {
    return switch (id.name()) {
      case "self" -> new Name.Self(id.location(), false, id.passData(), id.diagnostics());
      case "Self" -> new Name.SelfType(id.location(), id.passData(), id.diagnostics());
      default -> id;
    };
  }

  private IdentifiedLocation expandToContain(IdentifiedLocation encapsulating, IdentifiedLocation inner) {
    if (encapsulating == null || inner == null) {
      return encapsulating;
    }

    if (encapsulating.start() > inner.start() || encapsulating.end() < inner.end()) {
      var start = Math.min(encapsulating.start(), inner.start());
      var end = Math.max(encapsulating.end(), inner.end());
      return new IdentifiedLocation(start, end, encapsulating.uuid());
    } else {
      return encapsulating;
    }
  }

  private IdentifiedLocation getIdentifiedLocation(Tree ast) {
    var someId = Option.apply(ast.uuid());
    return getIdentifiedLocation(ast, 0, 0, someId);
  }

  private IdentifiedLocation getIdentifiedLocation(Tree ast, boolean generateId) {
    var someId = Option.apply(ast.uuid() == null && generateId ? UUID.randomUUID() : ast.uuid());
    return getIdentifiedLocation(ast, 0, 0, someId);
  }

  private IdentifiedLocation getIdentifiedLocation(Tree ast, int b, int e, Option<UUID> someId) {
    return switch (ast) {
      case null -> null;
      default -> {
        var begin = castToInt(ast.getStartCode()) + b;
        var end = castToInt(ast.getEndCode()) + e;
        var location = new Location(begin, end);
        var uuid = idMap.getOrDefault(location, someId == null ? ast.uuid() : someId.getOrElse(() -> null));
        yield new IdentifiedLocation(begin, end, uuid);
      }
    };
  }

  private IdentifiedLocation getIdentifiedLocation(ArgumentDefinition ast) {
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
    int begin_ = castToInt(begin);
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
    int end_ = castToInt(end);

    var location = new Location(begin_, end_);
    var uuid = idMap.get(location);
    return new IdentifiedLocation(begin_, end_, uuid);
  }

  private IdentifiedLocation getIdentifiedLocation(Token ast) {
    return getIdentifiedLocation(ast, false);
  }

  private IdentifiedLocation getIdentifiedLocation(Token ast, boolean generateId) {
    return switch (ast) {
      case null -> null;
      default -> {
        int begin = castToInt(ast.getStartCode());
        int end = castToInt(ast.getEndCode());
        var id = generateId ? UUID.randomUUID() : null;
        yield new IdentifiedLocation(new Location(begin, end), id);
      }
    };
  }

  private MetadataStorage meta() {
    return new MetadataStorage();
  }

  private static int castToInt(long presumablyInt) {
    int value = (int) presumablyInt;
    if (value != presumablyInt) {
      throw new ClassCastException("Huge int: " + presumablyInt);
    }
    return value;
  }

  @SuppressWarnings("unchecked")
  private static final <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }

  private static final <T> scala.collection.immutable.List<T> join(T head,
      scala.collection.immutable.List<T> tail) {
    return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
  }

  private static boolean isDotOperator(Token.Operator op) {
    return op != null && ".".equals(op.codeRepr());
  }

  private static Tree maybeManyParensed(Tree t) {
    for (; ; ) {
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

  private boolean checkArgs(List<CallArgument> args) {
    LinearSeq<CallArgument> a = args;
    while (!a.isEmpty()) {
      if (a.head() == null) {
        return false;
      }
      a = (LinearSeq<CallArgument>) a.tail();
    }
    return true;
  }

  private final class SyntaxException extends Exception {

    final Tree where;
    final Syntax.Reason reason;

    SyntaxException(Tree where, Syntax.Reason r) {
      this.where = where;
      this.reason = r;
    }

    Syntax toError() {
      return translateSyntaxError(where, reason);
    }

    Syntax toError(Syntax.Reason r) {
      return translateSyntaxError(where, r);
    }
  }
}
