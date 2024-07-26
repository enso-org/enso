package org.enso.compiler.pass.analyse.types;

import static org.enso.compiler.MetadataInteropHelpers.getMetadataOrNull;

import java.util.List;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Operator;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.persist.Persistance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.jdk.javaapi.CollectionConverters;

/**
 * A helper class for resolving expressions in type position into a {@link TypeRepresentation}, and
 * building complex types from parts - e.g. function type.
 */
public class TypeResolver {
  private static final Logger logger = LoggerFactory.getLogger(TypeResolver.class);

  public TypeRepresentation resolveTypeExpression(Expression type) {
    return switch (type) {
      case Name.Literal name -> getResolvedTypeFromBindingsMap(name);
      case Name.SelfType selfType -> getResolvedTypeFromBindingsMap(selfType);

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
        // This is because our ascriptions (x : A -> B) only check (x.is_a Function), so all we get
        // is that it is a
        // function with at least one argument (and we can't even tell its full arity).
        // Later, we could extract this as some kind of secondary metadata, but currently we do not
        // because it could be
        // misleading - this property is _not_ guaranteed at runtime as other ascriptions are.
        // Functions not matching
        // this type will still be allowed. That's why we return the more generic type that covers
        // everything that the
        // check actually lets through.
      case Type.Function function -> new TypeRepresentation.ArrowType(
          TypeRepresentation.UNKNOWN, TypeRepresentation.ANY);

        // We just ignore the error part for now as it's not really checked anywhere.
      case Type.Error error -> resolveTypeExpression(error.typed());

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
        //  Currently, to be consistent with how this works at runtime these are erased, resulting
        // in just `Vector`.
        yield resolveTypeExpression(app.function());
      }

      default -> {
        logger.debug(
            "resolveTypeExpression: UNKNOWN BRANCH {}", type.getClass().getCanonicalName());
        yield TypeRepresentation.UNKNOWN;
      }
    };
  }

  private TypeRepresentation getResolvedTypeFromBindingsMap(Name name) {
    BindingsMap.Resolution resolutionOrNull =
        getMetadataOrNull(name, TypeNames$.MODULE$, BindingsMap.Resolution.class);

    if (resolutionOrNull == null) {
      // As fallback, try getting from the Patterns pass.
      resolutionOrNull = getMetadataOrNull(name, Patterns$.MODULE$, BindingsMap.Resolution.class);
    }

    if (resolutionOrNull != null) {
      BindingsMap.ResolvedName target = resolutionOrNull.target();
      return switch (target) {
        case BindingsMap.ResolvedType resolvedType -> resolvedTypeAsAtomType(resolvedType);
        case BindingsMap.ResolvedPolyglotSymbol polyglotSymbol -> {
          // for now type inference is not able to deal with polyglot types, so we treat them as
          // unknown
          yield TypeRepresentation.UNKNOWN;
        }
        default -> {
          logger.debug(
              "resolveTypeExpression: {} - unexpected resolved name type {}",
              name.showCode(),
              target.getClass().getCanonicalName());
          yield TypeRepresentation.UNKNOWN;
        }
      };
    } else {
      // TODO investigate - these seem to unexpectedly come up when compiling Standard.Base
      logger.debug(
          "resolveTypeExpression: {} - Missing expected TypeName resolution metadata",
          name.showCode());
      return TypeRepresentation.UNKNOWN;
    }
  }

  TypeRepresentation.TypeObject resolvedTypeAsTypeObject(BindingsMap.ResolvedType resolvedType) {
    var iface = new AtomTypeInterfaceFromBindingsMap(resolvedType.tp());
    return new TypeRepresentation.TypeObject(resolvedType.qualifiedName(), iface);
  }

  TypeRepresentation resolvedTypeAsAtomType(BindingsMap.ResolvedType resolvedType) {
    return resolvedTypeAsTypeObject(resolvedType).instanceType();
  }

  TypeRepresentation buildAtomConstructorType(
      TypeRepresentation.TypeObject parentType, AtomTypeInterface.Constructor constructor) {
    boolean hasAnyDefaults =
        constructor.arguments().stream().anyMatch(AtomTypeInterface.Argument::hasDefaultValue);
    if (hasAnyDefaults) {
      // TODO implement handling of default arguments - not only ctors will need this!
      return null;
    }

    var arguments =
        constructor.arguments().stream()
            .map(
                (arg) -> {
                  var typ = arg.getType(this);
                  return typ != null ? typ : TypeRepresentation.UNKNOWN;
                })
            .toList();
    var resultType = parentType.instanceType();
    return TypeRepresentation.buildFunction(arguments, resultType);
  }

  private TypeRepresentation resolveTypeExpression(Persistance.Reference<Expression> ref) {
    return resolveTypeExpression(ref.get(Expression.class));
  }

  /** Returns the type ascribed to the given expression, if any. */
  TypeRepresentation getTypeAscription(Expression ir) {
    TypeSignatures.Signature ascribedSignature =
        getMetadataOrNull(ir, TypeSignatures$.MODULE$, TypeSignatures.Signature.class);
    if (ascribedSignature != null) {
      return resolveTypeExpression(ascribedSignature.signature());
    } else {
      return null;
    }
  }
}
