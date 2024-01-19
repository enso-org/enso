package org.enso.compiler.pass.analyse.types;

/**
 * Denotes if a given provided type can fit into an expected type.
 */
enum TypeCompatibility {
  /**
   * Indicates that the provided type will always fit the expected type.
   * <p>
   * For example, `Integer` will always fit into `Any`.
   */
  ALWAYS_COMPATIBLE,

  /**
   * Indicates that the provided type will never fit the expected type.
   * <p>
   * For example, `Integer` will never fit into `Text` (if no conversions are in scope).
   */
  NEVER_COMPATIBLE,

  /**
   * Indicates that it is unknown whether the provided type will fit the expected type or not.
   * <p>
   * For example, a value of type `Any` may or may not fit into `Integer` - depending on the actual runtime value.
   */
  UNKNOWN;


  // TODO this should take into account conversion scope
  static TypeCompatibility computeTypeCompatibility(TypeRepresentation expected, TypeRepresentation provided) {
    // Exact type match is always OK.
    if (expected.equals(provided)) {
      return TypeCompatibility.ALWAYS_COMPATIBLE;
    }

    // If the expected type is Any, it will match any type.
    if (expected.equals(TypeRepresentation.ANY)) {
      return TypeCompatibility.ALWAYS_COMPATIBLE;
    }

    // If the expected type was _not_ Any, but provided type may be Any - the compatibility is unknown, as the value may be anything (good or bad).
    if (provided.equals(TypeRepresentation.ANY)) {
      return TypeCompatibility.UNKNOWN;
    }

    if (expected.equals(TypeRepresentation.NUMBER)) {
      if (provided.equals(TypeRepresentation.INTEGER) || provided.equals(TypeRepresentation.FLOAT)) {
        return TypeCompatibility.ALWAYS_COMPATIBLE;
      }
    }

    if (expected instanceof TypeRepresentation.SumType) {
      // TODO
      return TypeCompatibility.UNKNOWN;
    }

    if (expected instanceof TypeRepresentation.IntersectionType) {
      // TODO
      return TypeCompatibility.UNKNOWN;
    }

    if (provided instanceof TypeRepresentation.SumType) {
      // TODO
      return TypeCompatibility.UNKNOWN;
    }

    if (provided instanceof TypeRepresentation.IntersectionType) {
      // TODO
      return TypeCompatibility.UNKNOWN;
    }

    if (expected instanceof TypeRepresentation.TypeObject && provided instanceof TypeRepresentation.TypeObject) {
      // If both are type objects, but they were not == above, that means they are not compatible.
      return TypeCompatibility.NEVER_COMPATIBLE;
    }

    if (expected instanceof TypeRepresentation.AtomType && provided instanceof TypeRepresentation.AtomType) {
      // If both are atom types, but they were not == above, that means they are not compatible.
      // TODO we have to check if there might be a conversion in the scope, see `noTypeErrorIfConversionExists` test
      // return TypeCompatibility.NEVER_COMPATIBLE;
      return TypeCompatibility.UNKNOWN;
    }

    if (isFunctionLike(expected) != isFunctionLike(provided)) {
      // If we are matching a function-like type with a non-function-like type, they are not compatible.
      // TODO later check: this may not work well with a function that has all-default arguments
      // TODO also here we have to check if there exists a conversion (TypeOf{expected}.from (that : Function) = ...) if {provided} is a function
      // return TypeCompatibility.NEVER_COMPATIBLE;
      return TypeCompatibility.UNKNOWN;
    }

    return TypeCompatibility.UNKNOWN;
  }

  private static boolean isFunctionLike(TypeRepresentation type) {
    return type instanceof TypeRepresentation.ArrowType || type instanceof TypeRepresentation.UnresolvedSymbol;
  }
}
