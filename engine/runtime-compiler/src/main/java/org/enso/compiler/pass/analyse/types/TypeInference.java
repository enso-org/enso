package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ConstantsNames;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.*;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.analyse.JavaInteropHelpers;
import org.enso.compiler.pass.resolve.*;
import org.enso.persist.Persistance;
import org.enso.pkg.QualifiedName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.CollectionConverters$;
import scala.util.Either;
import scala.util.Right;

import java.util.*;

public final class TypeInference implements IRPass {
  public static final TypeInference INSTANCE = new TypeInference();
  private UUID uuid;

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
        Option.empty(),
        Option.empty(),
        Option.empty()
    );

    BindingsMap bindingsMap = getMetadata(ir, BindingAnalysis$.MODULE$, BindingsMap.class);
    var mappedBindings = ir.bindings().map((def) -> switch (def) {
        case Method.Explicit b -> {
          var mapped = def.mapExpressions(
              (expression) -> analyzeExpression(expression, ctx, LocalBindingsTyping.create(), bindingsMap)
          );

          var inferredType = getInferredType(b.body());
          if (inferredType != null) {
            setInferredType(b, inferredType);
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

    processTypePropagation(mappedIr, bindingsMap, localBindingsTyping);

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
            logger.debug("redundant type ascription: {} - confirming inferred type {}", ir.showCode(), previouslyInferredType.type());
          } else {
            logger.debug("type ascription: {} - overwriting inferred type {}", ir.showCode(), previouslyInferredType.type());
          }

          TypeRepresentation expected = ascribedType;
          TypeRepresentation provided = previouslyInferredType.type();
          checkTypeCompatibility(ir, expected, provided);
        }

        setInferredType(ir, new InferredType(ascribedType));
      }
    }
  }

  private void processTypePropagation(Expression ir, BindingsMap bindingsMap, LocalBindingsTyping localBindingsTyping) {
    switch (ir) {
      case Name.Literal l -> processName(l, localBindingsTyping);
      case Application.Force f -> {
        var innerType = getInferredType(f.target());
        if (innerType != null) {
          setInferredType(f, innerType);
        }
      }
      case Application.Prefix p -> {
        var functionType = getInferredType(p.function());
        if (functionType != null) {
          var inferredType = processApplication(functionType.type(), p.arguments(), p, bindingsMap);
          if (inferredType != null) {
            setInferredType(p, new InferredType(inferredType));
          }
        }
      }
      case Expression.Binding b -> {
        var innerType = getInferredType(b.expression());
        if (innerType != null) {
          registerBinding(b, innerType.type(), localBindingsTyping);
        }
      }
      case Expression.Block b -> {
        var innerType = getInferredType(b.returnValue());
        if (innerType != null) {
          setInferredType(b, innerType);
        }
      }
      case Function.Lambda f -> {
        var type = buildLambdaType(f);
        if (type != null) {
          setInferredType(f, type);
        }
      }
      case Literal l -> processLiteral(l);
      case Application.Sequence sequence -> setInferredType(sequence, new InferredType(TypeRepresentation.VECTOR));
      case Case.Expr caseExpr -> {
        List<TypeRepresentation> innerTypes =
            CollectionConverters$.MODULE$.asJava(caseExpr.branches())
                .stream()
                .map(branch -> {
                  var innerType = getInferredType(branch.expression());
                  if (innerType != null) {
                    return innerType.type();
                  } else {
                    return TypeRepresentation.UNKNOWN;
                  }
                })
                .toList();
        setInferredType(caseExpr, new InferredType(TypeRepresentation.buildSimplifiedSumType(innerTypes)));
      }
      default -> {
        logger.trace("type propagation: UNKNOWN branch: {}", ir.getClass().getCanonicalName());
      }
    }
  }

  private void registerBinding(IR binding, TypeRepresentation type, LocalBindingsTyping localBindingsTyping) {
    var metadata = JavaInteropHelpers.getAliasAnalysisOccurrenceMetadata(binding);
    var occurrence = metadata.graph().getOccurrence(metadata.id());
    if (occurrence.isEmpty()) {
      logger.debug("registerBinding {}: missing occurrence in graph for {}", binding.showCode(), metadata);
      return;
    }

    var def = JavaInteropHelpers.occurrenceAsDef(occurrence.get());
    localBindingsTyping.registerBindingType(metadata.graph(), def.id(), type);
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

  private void processName(Name.Literal literalName, LocalBindingsTyping localBindingsTyping) {
    // This should reproduce IrToTruffle::processName logic
    var occurrenceMetadata = JavaInteropHelpers.getAliasAnalysisOccurrenceMetadata(literalName);
    Optional<BindingsMap.Resolution> global =
        getOptionalMetadata(literalName, GlobalNames$.MODULE$, BindingsMap.Resolution.class);
    var localLink = occurrenceMetadata.graph().defLinkFor(occurrenceMetadata.id());
    if (localLink.isDefined() && global.isPresent()) {
      logger.debug("processName: {} - BOTH DEFINED AND GLOBAL - WHAT TO DO HERE? {}", literalName.showCode(), occurrenceMetadata);
    }

    boolean isLocalReference = localLink.isDefined();
    if (isLocalReference) {
      int target = localLink.get().target();
      TypeRepresentation type = localBindingsTyping.getBindingType(occurrenceMetadata.graph(), target);
      if (type != null) {
        setInferredType(literalName, new InferredType(type));
      }
    } else if (global.isPresent()) {
      BindingsMap.ResolvedName resolution = global.get().target();
      processGlobalName(literalName, resolution);
    } else if (literalName.name().equals(ConstantsNames.FROM_MEMBER)) {
      // TODO support from conversions
    } else {
      var type = new TypeRepresentation.UnresolvedSymbol(literalName.name());
      setInferredType(literalName, new InferredType(type));
    }
  }

  private void processGlobalName(Name.Literal literalName, BindingsMap.ResolvedName resolution) {
    switch (resolution) {
      case BindingsMap.ResolvedConstructor ctor -> {
        // TODO check when do these appear?? I did not yet see them in the wild
        var constructorFunctionType = buildAtomConstructorType(resolvedTypeAsTypeObject(ctor.tpe()), ctor.cons());
        if (constructorFunctionType != null) {
          setInferredType(literalName, new InferredType(constructorFunctionType));
        }
      }

      case BindingsMap.ResolvedType tpe -> {
        var type = resolvedTypeAsTypeObject(tpe);
        setInferredType(literalName, new InferredType(type));
      }
      default ->
          logger.trace("processGlobalName: {} - global scope reference to {} - currently global inference is unsupported", literalName.showCode(), resolution);
    }
  }

  private TypeRepresentation.TypeObject resolvedTypeAsTypeObject(BindingsMap.ResolvedType resolvedType) {
    return new TypeRepresentation.TypeObject(resolvedType.qualifiedName());
  }

  private void processLiteral(Literal literal) {
    TypeRepresentation type = switch (literal) {
      case Literal.Number number -> number.isFractional() ? TypeRepresentation.FLOAT : TypeRepresentation.INTEGER;
      case Literal.Text text -> TypeRepresentation.TEXT;
      // This branch is needed only because Java is unable to infer that the match is exhaustive
      default ->
          throw new IllegalStateException("Impossible - unknown literal type: " + literal.getClass().getCanonicalName());
    };
    setInferredType(literal, new InferredType(type));
  }

  @SuppressWarnings("unchecked")
  private TypeRepresentation processApplication(TypeRepresentation functionType, scala.collection.immutable.List<CallArgument> arguments, Application.Prefix relatedIR, BindingsMap bindingsMap) {
    if (arguments.isEmpty()) {
      logger.warn("processApplication: {} - unexpected - no arguments in a function application", relatedIR.showCode());
      return functionType;
    }

    var firstArgument = arguments.head();
    var firstResult = processSingleApplication(functionType, firstArgument, relatedIR, bindingsMap);
    if (firstResult == null) {
      return null;
    }

    if (arguments.length() == 1) {
      return firstResult;
    } else {
      return processApplication(firstResult, (scala.collection.immutable.List<CallArgument>) arguments.tail(), relatedIR, bindingsMap);
    }
  }

  private TypeRepresentation processSingleApplication(TypeRepresentation functionType, CallArgument argument, Application.Prefix relatedIR, BindingsMap bindingsMap) {
    if (argument.name().isDefined()) {
      // TODO named arguments are not yet supported
      return null;
    }

    switch (functionType) {
      case TypeRepresentation.ArrowType arrowType -> {
        var argumentType = getInferredType(argument.value());
        if (argumentType != null) {
          checkTypeCompatibility(argument, arrowType.argType(), argumentType.type());
        }
        return arrowType.resultType();
      }

      case TypeRepresentation.UnresolvedSymbol unresolvedSymbol -> {
        return processUnresolvedSymbolApplication(bindingsMap, unresolvedSymbol, argument.value());
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

  private BindingsMap.Type findResolvedType(BindingsMap bindingsMap, QualifiedName typeName) {
    var resolved = switch (bindingsMap.resolveQualifiedName(typeName.fullPath())) {
      case Right<BindingsMap.ResolutionError, BindingsMap.ResolvedName> right ->
          right.value();
      default -> throw new IllegalStateException("Internal error: type signature contained _already resolved_ reference to type " + typeName + ", but now that type cannot be found in the bindings map.");
    };

    return switch (resolved) {
      case BindingsMap.ResolvedType resolvedType -> resolvedType.tp();
      default -> throw new IllegalStateException("Internal error: type signature contained _already resolved_ reference to type " + typeName + ", but now that type is not a type, but " + resolved + ".");
    };
  }

  private TypeRepresentation processUnresolvedSymbolApplication(BindingsMap bindingsMap, TypeRepresentation.UnresolvedSymbol function, Expression argument) {
    var argumentType = getInferredType(argument);
    if (argumentType == null) {
      return null;
    }

    switch (argumentType.type()) {
      case TypeRepresentation.TypeObject typeObject -> {
        var typeDescription = findResolvedType(bindingsMap, typeObject.name());
        Option<BindingsMap.Cons> ctorCandidate = typeDescription.members().find((ctor) -> ctor.name().equals(function.name()));
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
  private InferredType buildLambdaType(Function.Lambda f) {
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

    InferredType inferredReturnType = getInferredType(f.body());

    if (inferredReturnType == null && argTypesScala.isEmpty()) {
      // If the return type is unknown and we have no arguments, we do not infer anything useful - so we withdraw.
      return null;
    }

    TypeRepresentation returnType =
        inferredReturnType == null ? TypeRepresentation.ANY : inferredReturnType.type();

    TypeRepresentation arrowType = TypeRepresentation.buildFunction(
        CollectionConverters.asJava(argTypesScala),
        returnType
    );
    return new InferredType(arrowType);
  }

  private void setInferredType(IR ir, InferredType type) {
    Objects.requireNonNull(type, "type must not be null");
    ir.passData().update(this, type);
  }

  private InferredType getInferredType(Expression expression) {
    Option<ProcessingPass.Metadata> r = expression.passData().get(this);
    if (r.isDefined()) {
      return (InferredType) r.get();
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
          yield TypeRepresentation.fromQualifiedName(target.qualifiedName());
        } else {
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

      default -> {
        logger.warn("resolveTypeExpression: {} UNKNOWN BRANCH", type);
        yield TypeRepresentation.UNKNOWN;
      }
    };
  }

  private void checkTypeCompatibility(IR relatedIr, TypeRepresentation expected, TypeRepresentation provided) {
    TypeCompatibility compatibility = TypeCompatibility.computeTypeCompatibility(expected, provided);
    if (compatibility == TypeCompatibility.NEVER_COMPATIBLE) {
      relatedIr.diagnostics().add(new Warning.TypeMismatch(relatedIr.location(), expected.toString(), provided.toString()));
    }
  }

  private <T> Optional<T> getOptionalMetadata(IR ir, IRPass pass, Class<T> expectedType) {
    Option<ProcessingPass.Metadata> option = ir.passData().get(pass);
    if (option.isDefined()) {
      try {
        return Optional.of(expectedType.cast(option.get()));
      } catch (ClassCastException exception) {
        throw new IllegalStateException("Unexpected metadata type " + option.get().getClass().getCanonicalName() + " " +
            "for " + pass, exception);
      }
    } else {
      return Optional.empty();
    }
  }

  private <T> T getMetadata(IR ir, IRPass pass, Class<T> expectedType) {
    Optional<T> optional = getOptionalMetadata(ir, pass, expectedType);
    if (optional.isEmpty()) {
      throw new IllegalStateException("Missing expected " + pass + " metadata for " + ir + ".");
    }

    return optional.get();
  }

  private static final Logger logger = LoggerFactory.getLogger(TypeInference.class);
}
