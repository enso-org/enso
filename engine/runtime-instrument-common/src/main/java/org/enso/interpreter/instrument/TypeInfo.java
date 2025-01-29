package org.enso.interpreter.instrument;

import java.util.List;

/**
 * The type information observed by the instrumentation.
 *
 * <p>The list in the type definition represents an intersection type. A list with a single element
 * represents a simple type.
 *
 * @param visibleType the public type of the value visible to the user
 * @param conversionTypes the available conversions
 */
public record TypeInfo(List<String> visibleType, List<String> conversionTypes) {}
