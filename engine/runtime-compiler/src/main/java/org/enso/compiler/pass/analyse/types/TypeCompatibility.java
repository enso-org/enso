package org.enso.compiler.pass.analyse.types;

/** A class that helps with computing compatibility between types. */
class TypeCompatibility {
  TypeCompatibility(MethodTypeResolver methodTypeResolver) {
    this.conversionResolver = methodTypeResolver;
  }

  private final MethodTypeResolver conversionResolver;

  /** Denotes if a given provided type can fit into an expected type. */
  enum Compatibility {
    /**
     * Indicates that the provided type will always fit the expected type.
     *
     * <p>For example, `Integer` will always fit into `Any`.
     */
    ALWAYS_COMPATIBLE,

    /**
     * Indicates that the provided type will never fit the expected type.
     *
     * <p>For example, `Integer` will never fit into `Text` (if no conversions are in scope).
     */
    NEVER_COMPATIBLE,

    /**
     * Indicates that it is unknown whether the provided type will fit the expected type or not.
     *
     * <p>For example, a value of type `Any` may or may not fit into `Integer` - depending on the
     * actual runtime value.
     */
    UNKNOWN;
  }

  Compatibility computeTypeCompatibility(TypeRepresentation expected, TypeRepresentation provided) {
    // Exact type match is always OK.
    if (expected.equals(provided)) {
      return Compatibility.ALWAYS_COMPATIBLE;
    }

    // If the expected type is Any, it will match any type.
    if (expected.equals(TypeRepresentation.ANY)) {
      return Compatibility.ALWAYS_COMPATIBLE;
    }

    // If the expected type was _not_ Any, but provided type may be Any - the compatibility is
    // unknown, as the value may be anything (good or bad).
    if (provided.equals(TypeRepresentation.ANY)) {
      return Compatibility.UNKNOWN;
    }

    if (expected.equals(BuiltinTypes.NUMBER)) {
      if (provided.equals(BuiltinTypes.INTEGER) || provided.equals(BuiltinTypes.FLOAT)) {
        return Compatibility.ALWAYS_COMPATIBLE;
      }
    }

    if (provided.equals(BuiltinTypes.NUMBER)) {
      if (expected.equals(BuiltinTypes.INTEGER) || expected.equals(BuiltinTypes.FLOAT)) {
        return Compatibility.UNKNOWN;
      }
    }

    // This is a proof of concept. There is not much sense in implementing these branches until we
    // can handle conversions anyway.
    // So these TODOs will be addressed in future iterations.
    if (expected instanceof TypeRepresentation.SumType) {
      // TODO
      return Compatibility.UNKNOWN;
    }

    if (expected instanceof TypeRepresentation.IntersectionType) {
      // TODO
      return Compatibility.UNKNOWN;
    }

    if (provided instanceof TypeRepresentation.SumType) {
      // TODO
      return Compatibility.UNKNOWN;
    }

    if (provided instanceof TypeRepresentation.IntersectionType) {
      // TODO
      return Compatibility.UNKNOWN;
    }

    if (expected instanceof TypeRepresentation.TypeObject
        && provided instanceof TypeRepresentation.TypeObject) {
      // If both are type objects, but they were not == above, that means they are not compatible.
      return Compatibility.NEVER_COMPATIBLE;
    }

    if (expected instanceof TypeRepresentation.AtomType expectedAtom
        && provided instanceof TypeRepresentation.AtomType providedAtom) {
      assert !expected.equals(provided)
          : "Equal types should already have been handled by one of conditions above.";
      boolean existsConversionInScope =
          conversionResolver.findConversion(providedAtom, expectedAtom);
      return existsConversionInScope
          ? Compatibility.ALWAYS_COMPATIBLE
          : Compatibility.NEVER_COMPATIBLE;
    }

    boolean gotFunction = isFunctionLike(provided);
    boolean expectingFunction = isFunctionLike(expected);
    if (expectingFunction != gotFunction) {
      // If we are matching a function-like type with a non-function-like type, they are not
      // compatible.
      // TODO later check: this may not work well with a function that has all-default arguments

      if (gotFunction && expected instanceof TypeRepresentation.AtomType expectedAtom) {
        return isConvertibleToFunction(expectedAtom)
            ? Compatibility.ALWAYS_COMPATIBLE
            : Compatibility.NEVER_COMPATIBLE;
      }

      if (expectingFunction && provided instanceof TypeRepresentation.AtomType providedAtom) {
        // TODO for later: can we return ALWAYS_COMPATIBLE here?
        return isConvertibleFromFunction(providedAtom)
            ? Compatibility.UNKNOWN
            : Compatibility.NEVER_COMPATIBLE;
      }

      return Compatibility.NEVER_COMPATIBLE;
    }

    return Compatibility.UNKNOWN;
  }

  private boolean isConvertibleToFunction(TypeRepresentation.AtomType type) {
    return conversionResolver.findConversion(type, BuiltinTypes.functionTypeAsAtomType);
  }

  private boolean isConvertibleFromFunction(TypeRepresentation.AtomType type) {
    return conversionResolver.findConversion(BuiltinTypes.functionTypeAsAtomType, type);
  }

  /** Checks if a given type is function-like, i.e. it can be used as a target of an application. */
  boolean isFunctionLike(TypeRepresentation type) {
    // TODO think what to do with IntersectionType here
    return type instanceof TypeRepresentation.ArrowType
        || type instanceof TypeRepresentation.UnresolvedSymbol;
  }

  /**
   * A broader variant of {@code isFunctionLike}, that returns true for types that _may or may not_
   * contain a function value in the runtime.
   *
   * <p>For example a sum type or Any type may contain a function.
   */
  boolean mayBeFunctionLike(TypeRepresentation type) {
    if (isFunctionLike(type)) {
      return true;
    }

    return switch (type) {
      case TypeRepresentation.TopType top -> true;
      case TypeRepresentation.SumType sumType ->
          sumType.types().stream().anyMatch(this::mayBeFunctionLike);
      case TypeRepresentation.IntersectionType intersectionType ->
          intersectionType.types().stream().anyMatch(this::mayBeFunctionLike);
      default -> false;
    };
  }
}
