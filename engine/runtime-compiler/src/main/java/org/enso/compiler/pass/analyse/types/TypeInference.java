package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.context.NameResolutionAlgorithm;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Operator;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.analyse.alias.Graph;
import org.enso.compiler.pass.analyse.alias.Info;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.persist.Persistance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.CollectionConverters$;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import static org.enso.compiler.MetadataInteropHelpers.getMetadata;
import static org.enso.compiler.MetadataInteropHelpers.getOptionalMetadata;

public final class TypeInference implements IRPass {
  public static final TypeInference INSTANCE = new TypeInference();
  private static final Logger logger = LoggerFactory.getLogger(TypeInference.class);
  private UUID uuid;
  private BuiltinTypes builtinTypes = new BuiltinTypes();
  TypeCompatibility checker = new TypeCompatibility(builtinTypes);

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public UUID key() {
    return uuid;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    List<IRPass> passes = List.of(
        BindingAnalysis$.MODULE$,
        GlobalNames$.MODULE$,
        FullyQualifiedNames$.MODULE$,
        TypeNames$.MODULE$,
        Patterns$.MODULE$,
        TypeSignatures$.MODULE$
    );
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRPass> invalidatedPasses() {
    return (Seq<IRPass>) Seq$.MODULE$.empty();
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    var ctx = new InlineContext(
        moduleContext,
        moduleContext.compilerConfig(),
        Option.empty(),
        Option.empty(),
        moduleContext.freshNameSupply(),
        moduleContext.passConfiguration(),
        moduleContext.pkgRepo()
    );

    BindingsMap bindingsMap = getMetadata(ir, BindingAnalysis$.MODULE$, BindingsMap.class);
    var mappedBindings = ir.bindings().map((def) -> switch (def) {
      case Method.Explicit b -> {
        var mapped = def.mapExpressions(
            (expression) -> analyzeExpression(expression, ctx, LocalBindingsTyping.create(), bindingsMap)
        );

        var methodBodyType = getInferredType(b.body());
        if (methodBodyType != null) {
          setInferredType(b, methodBodyType);
        }

        yield mapped;
      }
      case Definition.Type typ -> typ;
      default -> {
        logger.trace("UNEXPECTED definition {}", def.getClass().getCanonicalName());
        yield def;
      }
    });

    return ir.copy(ir.imports(), ir.exports(), mappedBindings, ir.location(), ir.passData(), ir.diagnostics(), ir.id());
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return analyzeExpression(ir, inlineContext, LocalBindingsTyping.create(), inlineContext.bindingsAnalysis());
  }

  private Expression analyzeExpression(Expression ir, InlineContext inlineContext, LocalBindingsTyping localBindingsTyping, BindingsMap bindingsMap) {
    // We first run the inner expressions, as most basic inference is propagating types in a bottom-up manner.
    var mappedIr = switch (ir) {
      case Function.Lambda lambda -> {
        for (var arg : CollectionConverters$.MODULE$.asJava(lambda.arguments())) {
          if (arg.ascribedType().isDefined()) {
            var type = resolveTypeExpression(arg.ascribedType().get());
            registerBinding(arg, type, localBindingsTyping);
          }
        }
        var newBody = analyzeExpression(lambda.body(), inlineContext, localBindingsTyping, bindingsMap);
        yield lambda.copy(lambda.arguments(), newBody, lambda.location(), lambda.canBeTCO(), lambda.passData(), lambda.diagnostics(), lambda.id());
      }
      case Case.Expr caseExpr -> {
        var newScrutinee = analyzeExpression(caseExpr.scrutinee(), inlineContext, localBindingsTyping, bindingsMap);
        List<Case.Branch> newBranches = CollectionConverters$.MODULE$.asJava(caseExpr.branches()).stream().map((branch) -> {
          // TODO once we will be implementing type equality constraints*, we will need to copy localBindingsTyping here, to ensure independent typing of branches
          //  (*) (case x of _ : Integer -> e) ==> x : Integer within e
          var myBranchLocalBindingsTyping = localBindingsTyping;
          registerPattern(branch.pattern(), myBranchLocalBindingsTyping);
          var newExpression = analyzeExpression(branch.expression(), inlineContext, myBranchLocalBindingsTyping, bindingsMap);
          return branch.copy(branch.pattern(), newExpression, branch.terminalBranch(), branch.location(), branch.passData(), branch.diagnostics(), branch.id());
        }).toList();
        yield caseExpr.copy(newScrutinee, CollectionConverters$.MODULE$.asScala(newBranches).toSeq(), caseExpr.isNested(), caseExpr.location(), caseExpr.passData(), caseExpr.diagnostics(), caseExpr.id());
      }
      default -> ir.mapExpressions(
          (expression) -> analyzeExpression(expression, inlineContext, localBindingsTyping, bindingsMap)
      );
    };

    TypeRepresentation inferredType = processTypePropagation(mappedIr, localBindingsTyping);
    if (inferredType != null) {
      setInferredType(mappedIr, inferredType);
    }

    if (mappedIr instanceof Expression.Binding b) {
      registerBinding(b, inferredType, localBindingsTyping);
    }

    // The ascriptions are processed later, because we want them to _overwrite_ any type that was inferred.
    processTypeAscription(mappedIr);
    return mappedIr;
  }

  private void processTypeAscription(Expression ir) {
    Optional<TypeSignatures.Signature> ascribedSignature =
        getOptionalMetadata(ir, TypeSignatures$.MODULE$, TypeSignatures.Signature.class);
    if (ascribedSignature.isPresent()) {
      TypeSignatures.Signature s = ascribedSignature.get();
      TypeRepresentation ascribedType = resolveTypeExpression(s.signature());
      if (ascribedType != null) {
        var previouslyInferredType = getInferredType(ir);
        if (previouslyInferredType != null) {
          if (previouslyInferredType.equals(ascribedType)) {
            logger.debug("redundant type ascription: {} - confirming inferred type {}", ir.showCode(), previouslyInferredType);
          } else {
            logger.debug("type ascription: {} - overwriting inferred type {}", ir.showCode(), previouslyInferredType);
          }

          checkTypeCompatibility(ir, ascribedType, previouslyInferredType);
        }

        setInferredType(ir, ascribedType);
      }
    }
  }

  private TypeRepresentation processTypePropagation(Expression ir, LocalBindingsTyping localBindingsTyping) {
    return switch (ir) {
      case Name.Literal l -> processName(l, localBindingsTyping);
      case Application.Force f -> getInferredType(f.target());
      case Application.Prefix p -> {
        var functionType = getInferredType(p.function());
        if (functionType == null) {
          yield null;
        } else {
          yield processApplication(functionType, p.arguments(), p);
        }
      }
      case Expression.Binding b -> getInferredType(b.expression());
      case Expression.Block b -> getInferredType(b.returnValue());
      case Function.Lambda f -> buildLambdaType(f);
      case Literal l -> processLiteral(l);
      case Application.Sequence sequence -> builtinTypes.VECTOR;
      case Case.Expr caseExpr -> {
        List<TypeRepresentation> innerTypes =
            CollectionConverters$.MODULE$.asJava(caseExpr.branches())
                .stream()
                .map(branch -> {
                  var innerType = getInferredType(branch.expression());
                  if (innerType != null) {
                    return innerType;
                  } else {
                    return TypeRepresentation.UNKNOWN;
                  }
                })
                .toList();
        yield TypeRepresentation.buildSimplifiedSumType(innerTypes);
      }
      default -> {
        logger.trace("type propagation: UNKNOWN branch: {}", ir.getClass().getCanonicalName());
        yield null;
      }
    };
  }

  private void registerBinding(IR binding, TypeRepresentation type, LocalBindingsTyping localBindingsTyping) {
    var metadata = getMetadata(binding, AliasAnalysis$.MODULE$, Info.Occurrence.class);
    var occurrence = metadata.graph().getOccurrence(metadata.id());
    if (occurrence.isEmpty()) {
      logger.debug("registerBinding {}: missing occurrence in graph for {}", binding.showCode(), metadata);
      return;
    }

    if (occurrence.get() instanceof org.enso.compiler.pass.analyse.alias.Graph$Occurrence$Def def) {
      localBindingsTyping.registerBindingType(metadata.graph(), def.id(), type);
    } else {
      throw new CompilerError("Alias analysis occurrence has unexpected type: " + occurrence.get().getClass().getCanonicalName());
    }
  }

  private void registerPattern(Pattern pattern, LocalBindingsTyping localBindingsTyping) {
    switch (pattern) {
      case Pattern.Type typePattern -> {
        var type = resolveTypeExpression(typePattern.tpe());
        registerBinding(typePattern.name(), type, localBindingsTyping);
      }
      case Pattern.Constructor constructorPattern -> {
        for (var innerPattern : CollectionConverters$.MODULE$.asJava(constructorPattern.fields())) {
          registerPattern(innerPattern, localBindingsTyping);
        }
      }
      default -> {
      }
    }
  }

  private TypeRepresentation processName(Name.Literal literalName, LocalBindingsTyping localBindingsTyping) {
    var resolver = new CompilerNameResolution(localBindingsTyping);
    return resolver.resolveName(literalName);
  }

  private TypeRepresentation.TypeObject resolvedTypeAsTypeObject(BindingsMap.ResolvedType resolvedType) {
    return new TypeRepresentation.TypeObject(resolvedType.qualifiedName(), resolvedType.tp());
  }

  private TypeRepresentation resolvedTypeAsAtomType(BindingsMap.ResolvedType resolvedType) {
    return resolvedTypeAsTypeObject(resolvedType).instantiate();
  }

  private TypeRepresentation processLiteral(Literal literal) {
    return switch (literal) {
      case Literal.Number number -> number.isFractional() ? builtinTypes.FLOAT : builtinTypes.INTEGER;
      case Literal.Text text -> builtinTypes.TEXT;
      // This branch is needed only because Java is unable to infer that the match is exhaustive
      default ->
          throw new IllegalStateException("Impossible - unknown literal type: " + literal.getClass().getCanonicalName());
    };
  }

  @SuppressWarnings("unchecked")
  private TypeRepresentation processApplication(TypeRepresentation functionType, scala.collection.immutable.List<CallArgument> arguments, Application.Prefix relatedIR) {
    if (arguments.isEmpty()) {
      logger.warn("processApplication: {} - unexpected - no arguments in a function application", relatedIR.showCode());
      return functionType;
    }

    var firstArgument = arguments.head();
    var firstResult = processSingleApplication(functionType, firstArgument, relatedIR);
    if (firstResult == null) {
      return null;
    }

    if (arguments.length() == 1) {
      return firstResult;
    } else {
      return processApplication(firstResult, (scala.collection.immutable.List<CallArgument>) arguments.tail(), relatedIR);
    }
  }

  private TypeRepresentation processSingleApplication(TypeRepresentation functionType, CallArgument argument, Application.Prefix relatedIR) {
    if (argument.name().isDefined()) {
      // TODO named arguments are not yet supported
      return null;
    }

    switch (functionType) {
      case TypeRepresentation.ArrowType arrowType -> {
        var argumentType = getInferredType(argument.value());
        if (argumentType != null) {
          checkTypeCompatibility(argument, arrowType.argType(), argumentType);
        }
        return arrowType.resultType();
      }

      case TypeRepresentation.UnresolvedSymbol unresolvedSymbol -> {
        return processUnresolvedSymbolApplication(unresolvedSymbol, argument.value());
      }

      case TypeRepresentation.TopType() -> {
        // we ignore this branch - Any type can be whatever, it could be a function, so we cannot emit a 'guaranteed' error
      }

      default -> {
        relatedIR.diagnostics().add(new Warning.NotInvokable(relatedIR.location(), functionType.toString()));
      }
    }

    return null;
  }

  private TypeRepresentation processUnresolvedSymbolApplication(TypeRepresentation.UnresolvedSymbol function, Expression argument) {
    var argumentType = getInferredType(argument);
    if (argumentType == null) {
      return null;
    }

    switch (argumentType) {
      case TypeRepresentation.TypeObject typeObject -> {
        Option<BindingsMap.Cons> ctorCandidate = typeObject.typeDescription().members().find((ctor) -> ctor.name().equals(function.name()));
        if (ctorCandidate.isDefined()) {
          return buildAtomConstructorType(typeObject, ctorCandidate.get());
        } else {
          // TODO if no ctor found, we should search static methods, but that is not implemented currently; so we cannot report an error either - just do nothing
          return null;
        }
      }

      default -> {
        return null;
      }
    }
  }

  private TypeRepresentation buildAtomConstructorType(TypeRepresentation.TypeObject parentType, BindingsMap.Cons constructor) {
    if (constructor.anyFieldsDefaulted()) {
      // TODO implement handling of default arguments - not only ctors will need this!
      return null;
    }

    var arguments = constructor.arguments().map((arg) -> arg.typ().map(this::resolveTypeExpression).getOrElse(() -> TypeRepresentation.UNKNOWN));
    var resultType = parentType.instantiate();
    return TypeRepresentation.buildFunction(CollectionConverters.asJava(arguments), resultType);
  }

  /**
   * Builds the type of a lambda, based on available type information of its parts.
   * <p>
   * The return type is inferred based on the body, and expected argument types are based on type ascriptions of these
   * arguments (currently no upwards propagation of constraints yet). Even if the types are not known, we may fall back
   * to a default unknown type, but we may at least infer the minimum arity of the function.
   */
  private TypeRepresentation buildLambdaType(Function.Lambda f) {
    boolean hasAnyDefaults = f.arguments().find((arg) -> arg.defaultValue().isDefined()).isDefined();
    if (hasAnyDefaults) {
      // Inferring function types with default arguments is not supported yet.
      // TODO we will need to mark defaults in the TypeRepresentation to know when they may be FORCEd
      return null;
    }

    scala.collection.immutable.List<TypeRepresentation> argTypesScala =
        f.arguments()
            .filter((arg) -> !(arg.name() instanceof Name.Self))
            .map((arg) -> {
                  if (arg.ascribedType().isDefined()) {
                    Expression t = arg.ascribedType().get();
                    return resolveTypeExpression(t);
                  } else {
                    return TypeRepresentation.UNKNOWN;
                  }
                }
            );

    TypeRepresentation inferredReturnType = getInferredType(f.body());

    if (inferredReturnType == null && argTypesScala.isEmpty()) {
      // If the return type is unknown and we have no arguments, we do not infer anything useful - so we withdraw.
      return null;
    }

    TypeRepresentation returnType =
        inferredReturnType == null ? TypeRepresentation.UNKNOWN : inferredReturnType;

    return TypeRepresentation.buildFunction(
        CollectionConverters.asJava(argTypesScala),
        returnType
    );
  }

  private void setInferredType(IR ir, TypeRepresentation type) {
    Objects.requireNonNull(type, "type must not be null");
    ir.passData().update(this, new InferredType(type));
  }

  private TypeRepresentation getInferredType(Expression expression) {
    Option<ProcessingPass.Metadata> r = expression.passData().get(this);
    if (r.isDefined()) {
      InferredType metadata = (InferredType) r.get();
      return metadata.type();
    } else {
      return null;
    }
  }

  private TypeRepresentation resolveTypeExpression(Persistance.Reference<Expression> ref) {
    return resolveTypeExpression(ref.get(Expression.class));
  }

  private TypeRepresentation resolveTypeExpression(Expression type) {
    return switch (type) {
      case Name.Literal name -> {
        Optional<BindingsMap.Resolution> resolutionOptional =
            getOptionalMetadata(name, TypeNames$.MODULE$, BindingsMap.Resolution.class);

        if (resolutionOptional.isEmpty()) {
          // As fallback, try getting from the Patterns pass.
          resolutionOptional =
              getOptionalMetadata(name, Patterns$.MODULE$, BindingsMap.Resolution.class);
        }

        if (resolutionOptional.isPresent()) {
          BindingsMap.ResolvedName target = resolutionOptional.get().target();
          yield switch (target) {
            case BindingsMap.ResolvedType resolvedType -> resolvedTypeAsAtomType(resolvedType);
            case BindingsMap.ResolvedPolyglotSymbol polyglotSymbol -> {
              // for now type inference is not able to deal with polyglot types, so we treat them as unknown
              yield TypeRepresentation.UNKNOWN;
            }
            default -> {
              logger.warn("resolveTypeExpression: {} - unexpected resolved name type {}", name.showCode(), target.getClass().getCanonicalName());
              yield TypeRepresentation.UNKNOWN;
            }
          };
        } else {
          // TODO investigate - these seem to unexpectedly come up when compiling Standard.Base
          logger.warn("resolveTypeExpression: {} - Missing expected TypeName resolution metadata", type.showCode());
          yield TypeRepresentation.UNKNOWN;
        }
      }

      case Set.Union union -> {
        var operands = union.operands().map(this::resolveTypeExpression);
        yield TypeRepresentation.buildSimplifiedSumType(CollectionConverters.asJava(operands));
      }

      case Set.Intersection intersection -> {
        var lhs = resolveTypeExpression(intersection.left());
        var rhs = resolveTypeExpression(intersection.right());
        yield new TypeRepresentation.IntersectionType(List.of(lhs, rhs));
      }

      // We could extract more info form function, but we deliberately do not.
      // This is because our ascriptions (x : A -> B) only check (x.is_a Function), so all we get is that it is a
      // function with at least one argument (and we can't even tell its full arity).
      // Later, we could extract this as some kind of secondary metadata, but currently we do not because it could be
      // misleading - this property is _not_ guaranteed at runtime as other ascriptions are. Functions not matching
      // this type will still be allowed. That's why we return the more generic type that covers everything that the
      // check actually lets through.
      case Type.Function function -> new TypeRepresentation.ArrowType(
          TypeRepresentation.UNKNOWN,
          TypeRepresentation.ANY
      );

      // We just ignore the error part for now as it's not really checked anywhere.
      case Type.Error error -> resolveTypeExpression(error.typed());

      case Name.SelfType selfType -> {
        // TODO to be handled in further iterations
        yield TypeRepresentation.UNKNOWN;
      }

      case Name.Qualified qualified -> {
        // TODO to be handled in further iterations
        yield TypeRepresentation.UNKNOWN;
      }

      case Operator.Binary binaryOp -> {
        // TODO to be handled in further iterations
        //  This is mostly sum-type ascriptions: A | B
        yield TypeRepresentation.UNKNOWN;
      }

      case Application.Prefix app -> {
        // An application at type-level means higher order types, like `Vector Text`.
        //  Currently, to be consistent with how this works at runtime these are erased, resulting in just `Vector`.
        yield resolveTypeExpression(app.function());
      }

      default -> {
        logger.warn("resolveTypeExpression: UNKNOWN BRANCH {}", type.getClass().getCanonicalName());
        yield TypeRepresentation.UNKNOWN;
      }
    };
  }

  private void checkTypeCompatibility(IR relatedIr, TypeRepresentation expected, TypeRepresentation provided) {
    TypeCompatibility.Compatibility compatibility = checker.computeTypeCompatibility(expected, provided);
    if (compatibility == TypeCompatibility.Compatibility.NEVER_COMPATIBLE) {
      relatedIr.diagnostics().add(new Warning.TypeMismatch(relatedIr.location(), expected.toString(), provided.toString()));
    }
  }

  private class CompilerNameResolution extends NameResolutionAlgorithm<TypeRepresentation, CompilerNameResolution.LinkInfo> {
    private final LocalBindingsTyping localBindingsTyping;

    private CompilerNameResolution(LocalBindingsTyping localBindingsTyping) {
      this.localBindingsTyping = localBindingsTyping;
    }

    @Override
    protected Option<LinkInfo> findLocalLink(Info.Occurrence occurrenceMetadata) {
      return occurrenceMetadata.graph()
          .defLinkFor(occurrenceMetadata.id())
          .map((link) -> new LinkInfo(occurrenceMetadata.graph(), link));
    }

    @Override
    protected TypeRepresentation resolveLocalName(LinkInfo localLink) {
      return localBindingsTyping.getBindingType(localLink.graph, localLink.link.target());
    }

    @Override
    protected TypeRepresentation resolveGlobalName(BindingsMap.ResolvedName resolvedName) {
      return switch (resolvedName) {
        // TODO check when do these appear?? I did not yet see them in the wild
        case BindingsMap.ResolvedConstructor ctor ->
            buildAtomConstructorType(resolvedTypeAsTypeObject(ctor.tpe()), ctor.cons());

        case BindingsMap.ResolvedType tpe -> resolvedTypeAsTypeObject(tpe);

        default -> {
          logger.trace("processGlobalName: global scope reference to {} - currently global inference is unsupported", resolvedName);
          yield null;
        }
      };
    }

    @Override
    protected TypeRepresentation resolveFromConversion() {
      // TODO currently from conversions are not supported
      //  we will probably create a sibling type to UnresolvedSymbol for that purpose
      return null;
    }

    @Override
    protected TypeRepresentation resolveUnresolvedSymbol(String symbolName) {
      return new TypeRepresentation.UnresolvedSymbol(symbolName);
    }

    private record LinkInfo(Graph graph, Graph.Link link) {
    }
  }
}
