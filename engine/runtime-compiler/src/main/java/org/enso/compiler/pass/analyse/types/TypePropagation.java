package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.context.NameResolutionAlgorithm;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.alias.Graph;
import org.enso.compiler.pass.analyse.alias.Info;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters$;

import java.util.List;

import static org.enso.compiler.pass.analyse.types.CommonTypeHelpers.getInferredType;

/**
 * A helper class providing the logic of propagating types through the IR.
 * <p>
 * It implements a single step of the propagation - it looks at a given IR element,
 * and based on types of its _immediate_ children (if they are available), infers a type for the current element.
 * <p>
 * Traversing the whole IR tree, in the right order, is managed by the {@link TypeInference} pass.
 * <p>
 * Currently, this propagation is completely bottom-up, meaning that we first assign types to leaves in the tree
 * and then propagate them up. This will not work for cases like recursion. That will be handled in future iterations,
 * most likely by extending the {@link TypeRepresentation} with 'variables' that will be later solved by unification.
 */
abstract class TypePropagation {
  private static final Logger logger = LoggerFactory.getLogger(TypePropagation.class);
  private final TypeResolver typeResolver;
  private final BuiltinTypes builtinTypes;

  TypePropagation(TypeResolver typeResolver, BuiltinTypes builtinTypes) {
    this.typeResolver = typeResolver;
    this.builtinTypes = builtinTypes;
  }

  protected abstract void checkTypeCompatibility(IR relatedIr, TypeRepresentation expected, TypeRepresentation provided);

  protected abstract void encounteredInvocationOfNonFunctionType(IR relatedIr, TypeRepresentation type);

  TypeRepresentation tryInferringType(Expression ir, LocalBindingsTyping localBindingsTyping) {
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
      case Function.Lambda f -> typeResolver.buildLambdaType(f);
      case Literal l -> processLiteral(l);
      case Application.Sequence sequence -> builtinTypes.VECTOR;
      case Case.Expr caseExpr -> {
        List<TypeRepresentation> innerTypes = CollectionConverters$.MODULE$.asJava(caseExpr.branches()).stream().map(branch -> {
          var innerType = getInferredType(branch.expression());
          if (innerType != null) {
            return innerType;
          } else {
            return TypeRepresentation.UNKNOWN;
          }
        }).toList();
        yield TypeRepresentation.buildSimplifiedSumType(innerTypes);
      }
      default -> {
        logger.trace("type propagation: UNKNOWN branch: {}", ir.getClass().getCanonicalName());
        yield null;
      }
    };
  }

  private TypeRepresentation processName(Name.Literal literalName, LocalBindingsTyping localBindingsTyping) {
    var resolver = new CompilerNameResolution(localBindingsTyping);
    return resolver.resolveName(literalName);
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
        encounteredInvocationOfNonFunctionType(relatedIR, functionType);
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
          return typeResolver.buildAtomConstructorType(typeObject, ctorCandidate.get());
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

  private class CompilerNameResolution extends NameResolutionAlgorithm<TypeRepresentation, CompilerNameResolution.LinkInfo> {
    private final LocalBindingsTyping localBindingsTyping;

    private CompilerNameResolution(LocalBindingsTyping localBindingsTyping) {
      this.localBindingsTyping = localBindingsTyping;
    }

    @Override
    protected Option<LinkInfo> findLocalLink(Info.Occurrence occurrenceMetadata) {
      return occurrenceMetadata.graph().defLinkFor(occurrenceMetadata.id()).map((link) -> new LinkInfo(occurrenceMetadata.graph(), link));
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
            typeResolver.buildAtomConstructorType(typeResolver.resolvedTypeAsTypeObject(ctor.tpe()), ctor.cons());

        case BindingsMap.ResolvedType tpe -> typeResolver.resolvedTypeAsTypeObject(tpe);

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
