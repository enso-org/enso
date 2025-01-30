package org.enso.interpreter.instrument;

import java.util.Arrays;

/**
 * The type information observed by the instrumentation.
 *
 * <p>The list in the type definition represents an intersection type. A list with a single element
 * represents a simple type.
 *
 * @param visibleType the public type of the value visible to the user
 * @param conversionTypes the available conversions
 */
public record TypeInfo(String[] visibleType, String[] conversionTypes) {

  public static TypeInfo ofType(String typeName) {
    return new TypeInfo(new String[] {typeName}, new String[] {});
  }

  public static TypeInfo ofIntersectionType(String[] intersectionType) {
    return new TypeInfo(intersectionType, new String[] {});
  }

  @Override
  public String toString() {
    return "TypeInfo("
        + Arrays.toString(visibleType)
        + ","
        + Arrays.toString(conversionTypes)
        + ")";
  }
}
