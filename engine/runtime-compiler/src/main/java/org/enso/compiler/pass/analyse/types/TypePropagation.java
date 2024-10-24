package org.enso.compiler.pass.analyse.types;

import static org.enso.compiler.MetadataInteropHelpers.getMetadata;

import java.util.List;
import org.enso.compiler.MetadataInteropHelpers;
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
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.alias.AliasMetadata;
import org.enso.compiler.pass.analyse.alias.graph.Graph;
import org.enso.compiler.pass.analyse.alias.graph.GraphOccurrence;
import org.enso.compiler.pass.analyse.types.scope.ModuleResolver;
import org.enso.compiler.pass.analyse.types.scope.StaticModuleScope;
import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters$;

/**
 * A helper class providing the logic of propagating types through the IR.
 *
 * <p>Currently, this propagation is completely bottom-up, meaning that we first assign types to
 * leaves in the tree and then propagate them up. This will not work for cases like recursion. That
 * will be handled in future iterations, most likely by extending the {@link TypeRepresentation}
 * with 'variables' that will be later solved by unification.
 */
abstract class TypePropagation {
  private static final Logger logger = LoggerFactory.getLogger(TypePropagation.class);
  private final TypeResolver typeResolver;
  private final TypeCompatibility compatibilityChecker;
  private final BuiltinTypes builtinTypes;
  private final MethodTypeResolver methodTypeResolver;

  TypePropagation(
      TypeResolver typeResolver,
      TypeCompatibility compatibilityChecker,
      BuiltinTypes builtinTypes,
      Module currentModule,
      ModuleResolver moduleResolver) {
    this.typeResolver = typeResolver;
    this.compatibilityChecker = compatibilityChecker;
    this.builtinTypes = builtinTypes;

    var currentModuleScope = StaticModuleScope.forIR(currentModule);
    this.methodTypeResolver =
        new MethodTypeResolver(moduleResolver, currentModuleScope, builtinTypes);
  }

  /**
   * The callback that is called when two types are being reconciled and are deemed incompatible by
   * the {@link TypeCompatibility} checker.
   *
   * @param relatedIr the IR element that caused the reconciliation, e.g. an argument of a function
   *     call
   * @param expected the type that is expected in this place (e.g. argument type expected by a
   *     function)
   * @param provided the actual (inferred) type of the encountered element
   */
  protected abstract void encounteredIncompatibleTypes(
      IR relatedIr, TypeRepresentation expected, TypeRepresentation provided);

  /**
   * The callback that is called when a value of a non-function type is being invoked as a function.
   */
  protected abstract void encounteredInvocationOfNonFunctionType(
      IR relatedIr, TypeRepresentation type);

  /**
   * The callback that is called when a method is being invoked on a type that does not have such a
   * method.
   */
  protected abstract void encounteredNoSuchMethod(
      IR relatedIr, TypeRepresentation type, String methodName, MethodCallKind kind);

  enum MethodCallKind {
    MEMBER,
    STATIC,
    MODULE
  }

  void checkTypeCompatibility(
      IR relatedIr, TypeRepresentation expected, TypeRepresentation provided) {
    TypeCompatibility.Compatibility compatibility =
        compatibilityChecker.computeTypeCompatibility(expected, provided);
    if (compatibility == TypeCompatibility.Compatibility.NEVER_COMPATIBLE) {
      encounteredIncompatibleTypes(relatedIr, expected, provided);
    }
  }

  /**
   * The main entry point to the type propagation logic.
   *
   * <p>It tries to infer the type of the given expression, based on already inferred types of its
   * parts.
   *
   * @param expression the expression whose type we want to infer
   * @param localBindingsTyping map of registered known types of local bindings
   * @return the inferred type of the given expression, or null if it could not be inferred
   */
  TypeRepresentation tryInferringType(
      Expression expression, LocalBindingsTyping localBindingsTyping) {
    TypeRepresentation inferredType =
        switch (expression) {
          case Name.Literal l -> processName(l, localBindingsTyping);
          case Application.Force f -> tryInferringType(f.target(), localBindingsTyping);
          case Application.Prefix p -> {
            var functionType = tryInferringType(p.function(), localBindingsTyping);
            if (functionType == null) {
              yield null;
            } else {
              yield processApplication(functionType, p.arguments(), p, localBindingsTyping);
            }
          }
          case Expression.Binding b -> {
            var bindingType = tryInferringType(b.expression(), localBindingsTyping);
            if (bindingType != null) {
              registerBinding(b, bindingType, localBindingsTyping);
              TypeInferencePropagation.setInferredType(b, bindingType);
            }
            yield bindingType;
          }
          case Expression.Block b -> {
            // Even though we discard the result, we run the type inference on each expression to
            // ensure any bindings inside of it get registered:
            b.expressions().foreach((expr) -> tryInferringType(expr, localBindingsTyping));
            yield tryInferringType(b.returnValue(), localBindingsTyping);
          }
          case Function.Lambda f -> processLambda(f, localBindingsTyping);
          case Literal l -> processLiteral(l);
          case Application.Sequence sequence -> builtinTypes.VECTOR;
          case Case.Expr caseExpr -> processCaseExpression(caseExpr, localBindingsTyping);
          default -> {
            logger.trace(
                "type propagation: UNKNOWN branch: {}", expression.getClass().getCanonicalName());
            yield null;
          }
        };

    TypeRepresentation ascribedType = typeResolver.getTypeAscription(expression);
    checkInferredAndAscribedTypeCompatibility(expression, inferredType, ascribedType);

    // We now override the inferred type on the expression, preferring the ascribed type if it is
    // present.
    return ascribedType != null ? ascribedType : inferredType;
  }

  private TypeRepresentation processCaseExpression(
      Case.Expr caseExpr, LocalBindingsTyping localBindingsTyping) {
    List<TypeRepresentation> innerTypes =
        CollectionConverters$.MODULE$.asJava(caseExpr.branches()).stream()
            .map(
                branch -> {
                  // Fork the bindings map for each branch, as in the future type equality
                  // constraints may add constraints on bindings outside of the switch that should
                  // be local to the branch only. We will also need to be careful about unification
                  // here.
                  var myBranchLocalBindingsTyping = localBindingsTyping.fork();
                  registerPattern(branch.pattern(), myBranchLocalBindingsTyping);

                  var innerType =
                      tryInferringType(branch.expression(), myBranchLocalBindingsTyping);
                  return innerType != null ? innerType : TypeRepresentation.UNKNOWN;
                })
            .toList();
    return TypeRepresentation.buildSimplifiedSumType(innerTypes);
  }

  private TypeRepresentation processName(
      Name.Literal literalName, LocalBindingsTyping localBindingsTyping) {
    var resolver = new CompilerNameResolution(localBindingsTyping);
    var occMeta =
        MetadataInteropHelpers.getMetadataOrNull(
            literalName, AliasAnalysis$.MODULE$, AliasMetadata.Occurrence.class);
    return resolver.resolveName(literalName, occMeta);
  }

  private TypeRepresentation processLiteral(Literal literal) {
    return switch (literal) {
      case Literal.Number number -> number.isFractional()
          ? builtinTypes.FLOAT
          : builtinTypes.INTEGER;
      case Literal.Text text -> builtinTypes.TEXT;
        // This branch is needed only because Java is unable to infer that the match is exhaustive
      default -> throw new IllegalStateException(
          "Impossible - unknown literal type: " + literal.getClass().getCanonicalName());
    };
  }

  /**
   * Tries to infer the type of a lambda, based on available type information of its parts.
   *
   * <p>The return type is inferred based on the body, and expected argument types are based on type
   * ascriptions of these arguments (currently no upwards propagation of constraints yet). Even if
   * the types are not known, we may fall back to a default unknown type, but we may at least infer
   * the minimum arity of the function.
   */
  private TypeRepresentation processLambda(
      Function.Lambda lambda, LocalBindingsTyping localBindingsTyping) {
    boolean hasAnyDefaults =
        lambda.arguments().find((arg) -> arg.defaultValue().isDefined()).isDefined();
    if (hasAnyDefaults) {
      // Inferring function types with default arguments is not supported yet.
      // TODO we will need to mark defaults in the TypeRepresentation to know when they may be
      // FORCEd
      return null;
    }

    scala.collection.immutable.List<TypeRepresentation> argTypesScala =
        lambda
            .arguments()
            .filter((arg) -> !(arg.name() instanceof Name.Self))
            .map(
                (arg) -> {
                  if (arg.ascribedType().isDefined()) {
                    Expression typeExpression = arg.ascribedType().get();
                    var resolvedTyp = typeResolver.resolveTypeExpression(typeExpression);
                    if (resolvedTyp != null) {
                      // We register the type of the argument in the local bindings map, so that it
                      // can be used by expressions that refer to this argument.
                      // No need to fork it, because there is just one code path.
                      registerBinding(arg, resolvedTyp, localBindingsTyping);
                      return resolvedTyp;
                    }
                  }

                  return TypeRepresentation.UNKNOWN;
                });

    TypeRepresentation returnType = tryInferringType(lambda.body(), localBindingsTyping);

    if (returnType == null && argTypesScala.isEmpty()) {
      // If the return type is unknown and we have no arguments, we do not infer anything useful -
      // so we withdraw.
      return null;
    }

    if (returnType == null) {
      returnType = TypeRepresentation.UNKNOWN;
    }

    return TypeRepresentation.buildFunction(
        CollectionConverters$.MODULE$.asJava(argTypesScala), returnType);
  }

  @SuppressWarnings("unchecked")
  private TypeRepresentation processApplication(
      TypeRepresentation functionType,
      scala.collection.immutable.List<CallArgument> arguments,
      Application.Prefix relatedIR,
      LocalBindingsTyping localBindingsTyping) {
    if (arguments.isEmpty()) {
      logger.debug(
          "processApplication: {} - unexpected - no arguments in a function application",
          relatedIR.showCode());
      return functionType;
    }

    var firstArgument = arguments.head();
    var firstResult =
        processSingleApplication(functionType, firstArgument, relatedIR, localBindingsTyping);
    if (firstResult == null) {
      return null;
    }

    if (arguments.length() == 1) {
      return firstResult;
    } else {
      return processApplication(
          firstResult,
          (scala.collection.immutable.List<CallArgument>) arguments.tail(),
          relatedIR,
          localBindingsTyping);
    }
  }

  private TypeRepresentation processSingleApplication(
      TypeRepresentation functionType,
      CallArgument argument,
      Application.Prefix relatedIR,
      LocalBindingsTyping localBindingsTyping) {
    if (argument.name().isDefined()) {
      // TODO named arguments are not yet supported
      return null;
    }

    switch (functionType) {
      case TypeRepresentation.ArrowType arrowType -> {
        var argumentType = tryInferringType(argument.value(), localBindingsTyping);
        if (argumentType != null) {
          checkTypeCompatibility(argument, arrowType.argType(), argumentType);
        }
        return arrowType.resultType();
      }

      case TypeRepresentation.UnresolvedSymbol unresolvedSymbol -> {
        return processUnresolvedSymbolApplication(
            unresolvedSymbol, argument.value(), localBindingsTyping, relatedIR);
      }

      default -> {
        if (compatibilityChecker.mayBeFunctionLike(functionType)) {
          // if the type is Any or a sum type that _may_ be a function type (but may not),
          // we can neither warn (because the computation may succeed)
          // nor infer a type (because the type may be incorrect, depending on the runtime value),
          // we really cannot tell anything
          return null;
        } else {
          // If the type surely is not function-like, we report the warning.
          encounteredInvocationOfNonFunctionType(relatedIR, functionType);
        }
      }
    }

    return null;
  }

  private TypeRepresentation processUnresolvedSymbolApplication(
      TypeRepresentation.UnresolvedSymbol function,
      Expression argument,
      LocalBindingsTyping localBindingsTyping,
      IR relatedWholeApplicationIR) {
    var argumentType = tryInferringType(argument, localBindingsTyping);
    if (argumentType == null) {
      argumentType = TypeRepresentation.ANY;
    }

    switch (argumentType) {
      case TypeRepresentation.TypeObject typeObject -> {
        if (isConstructorOrType(function.name())) {
          var ctorCandidate =
              typeObject.typeInterface().constructors().stream()
                  .filter(ctor -> ctor.name().equals(function.name()))
                  .findFirst();
          if (ctorCandidate.isPresent()) {
            return typeResolver.buildAtomConstructorType(typeObject, ctorCandidate.get());
          } else {
            // TODO we could report that no valid constructor was found
            return null;
          }
        } else {
          // We resolve static calls on the eigen type. It should also contain registrations of the
          // static variants of member methods, so we don't need to inspect member scope.
          var staticScope = TypeScopeReference.atomEigenType(typeObject.name());
          var resolvedStaticMethod = methodTypeResolver.resolveMethod(staticScope, function.name());
          if (resolvedStaticMethod == null) {
            encounteredNoSuchMethod(
                relatedWholeApplicationIR, argumentType, function.name(), MethodCallKind.STATIC);
          }
          return resolvedStaticMethod;
        }
      }

      case TypeRepresentation.ModuleReference moduleReference -> {
        var typeScope = TypeScopeReference.moduleAssociatedType(moduleReference.name());

        if (isConstructorOrType(function.name())) {
          // This is a special case when we are accessing a type inside a module, e.g. Mod.Type
          // 'call' should resolve to the type
          // TODO
          return null;
        } else {
          var resolvedModuleMethod = methodTypeResolver.resolveMethod(typeScope, function.name());
          if (resolvedModuleMethod == null) {
            encounteredNoSuchMethod(
                relatedWholeApplicationIR, argumentType, function.name(), MethodCallKind.MODULE);
          }
          return resolvedModuleMethod;
        }
      }

      case TypeRepresentation.AtomType atomInstanceType -> {
        var typeScope = TypeScopeReference.atomType(atomInstanceType.fqn());
        var resolvedMemberMethod = methodTypeResolver.resolveMethod(typeScope, function.name());
        if (resolvedMemberMethod == null) {
          encounteredNoSuchMethod(
              relatedWholeApplicationIR, argumentType, function.name(), MethodCallKind.MEMBER);
        }
        return resolvedMemberMethod;
      }

      case TypeRepresentation.TopType topType -> {
        // We don't report not found methods here, because the top type can be anything, so the call
        // 'may' be valid and we only want to report guaranteed failures
        return methodTypeResolver.resolveMethod(TypeScopeReference.ANY, function.name());
      }

        // This is not calling this function, instead it is calling the _method_ represented by the
        // UnresolvedSymbol on this Function object.
      case TypeRepresentation.ArrowType functionAsObject -> {
        var typeScope = TypeScopeReference.atomType(functionAsObject.getAssociatedType());
        var resolvedMethod = methodTypeResolver.resolveMethod(typeScope, function.name());
        if (resolvedMethod == null) {
          encounteredNoSuchMethod(
              relatedWholeApplicationIR, functionAsObject, function.name(), MethodCallKind.MEMBER);
        }
        return resolvedMethod;
      }

      default -> {
        // TODO calling on sum types, intersection types, etc.
        return null;
      }
    }
  }

  private boolean isConstructorOrType(String name) {
    assert !name.isEmpty();
    char firstCharacter = name.charAt(0);
    return Character.isUpperCase(firstCharacter);
  }

  private class CompilerNameResolution
      extends NameResolutionAlgorithm<
          TypeRepresentation, CompilerNameResolution.LinkInfo, AliasMetadata.Occurrence> {
    private final LocalBindingsTyping localBindingsTyping;

    private CompilerNameResolution(LocalBindingsTyping localBindingsTyping) {
      this.localBindingsTyping = localBindingsTyping;
    }

    @Override
    protected Option<LinkInfo> findLocalLink(AliasMetadata.Occurrence occurrenceMetadata) {
      return occurrenceMetadata
          .graph()
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
          // TODO investigate when do these appear?? I did not yet see them in the wild
        case BindingsMap.ResolvedConstructor ctor -> {
          var constructorInterface =
              new AtomTypeInterfaceFromBindingsMap.ConstructorFromBindingsMap(ctor.cons());
          yield typeResolver.buildAtomConstructorType(
              typeResolver.resolvedTypeAsTypeObject(ctor.tpe()), constructorInterface);
        }

        case BindingsMap.ResolvedType tpe -> typeResolver.resolvedTypeAsTypeObject(tpe);

        case BindingsMap.ResolvedModule mod -> new TypeRepresentation.ModuleReference(
            mod.qualifiedName());

        default -> {
          logger.trace(
              "resolveGlobalName: reference to {} - is currently not being resolved in static"
                  + " analysis",
              resolvedName);
          yield null;
        }
      };
    }

    @Override
    protected TypeRepresentation resolveFromConversion() {
      // TODO currently from conversions are not supported
      //  we will probably create a sibling type representation to UnresolvedSymbol for that purpose
      return null;
    }

    @Override
    protected TypeRepresentation resolveUnresolvedSymbol(String symbolName) {
      return new TypeRepresentation.UnresolvedSymbol(symbolName);
    }

    private record LinkInfo(Graph graph, Graph.Link link) {}
  }

  /**
   * Registers the type associated with the bound value in the local bindings map, so that it can
   * later be used by expressions that refer to this binding.
   */
  private void registerBinding(
      IR binding, TypeRepresentation type, LocalBindingsTyping localBindingsTyping) {
    var metadata = getMetadata(binding, AliasAnalysis$.MODULE$, AliasMetadata.Occurrence.class);
    var occurrence = metadata.graph().getOccurrence(metadata.id());
    if (occurrence.isEmpty()) {
      logger.warn(
          "registerBinding {}: missing occurrence in graph for {}", binding.showCode(), metadata);
      return;
    }

    if (occurrence.get() instanceof GraphOccurrence.Def def) {
      localBindingsTyping.registerBindingType(metadata.graph(), def.id(), type);
    } else {
      throw new CompilerError(
          "Alias analysis occurrence has unexpected type: "
              + occurrence.get().getClass().getCanonicalName());
    }
  }

  /**
   * Registers the type associated with a value bound withing a pattern match in the local bindings
   * map.
   */
  private void registerPattern(Pattern pattern, LocalBindingsTyping localBindingsTyping) {
    switch (pattern) {
      case Pattern.Type typePattern -> {
        var type = typeResolver.resolveTypeExpression(typePattern.tpe());
        registerBinding(typePattern.name(), type, localBindingsTyping);
      }
      case Pattern.Constructor constructorPattern -> {
        for (var innerPattern : CollectionConverters$.MODULE$.asJava(constructorPattern.fields())) {
          registerPattern(innerPattern, localBindingsTyping);
        }
      }
      default -> {}
    }
  }

  private void checkInferredAndAscribedTypeCompatibility(
      Expression ir, TypeRepresentation inferredType, TypeRepresentation ascribedType) {
    if (ascribedType != null && inferredType != null) {
      if (!inferredType.equals(ascribedType)) {
        logger.trace(
            "type ascription: {} - overwriting inferred type {}", ir.showCode(), inferredType);
      }

      // If the inferred type implies the ascription will fail at runtime, we can report a warning
      // here.
      checkTypeCompatibility(ir, ascribedType, inferredType);
    }
  }
}
