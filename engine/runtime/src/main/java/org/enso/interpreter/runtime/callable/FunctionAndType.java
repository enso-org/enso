package org.enso.interpreter.runtime.callable;

import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;

/**
 * Pair of a function and a type. Utility class used by function, conversion, operator, etc.
 * resolution.
 */
public record FunctionAndType(Function function, Type type) {}
