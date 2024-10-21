package org.enso.runtime.parser.processor;

import java.util.function.Function;
import org.enso.runtime.parser.dsl.IRChild;

/**
 * A field of an IR node. Represented by any parameterless method on an interface annotated with
 * {@link org.enso.runtime.parser.dsl.IRNode}.
 */
interface Field {

  /** Name (identifier) of the field. */
  String getName();

  /** Does not return null. */
  String getSimpleTypeName();

  /** May return null if the type is primitive. */
  String getQualifiedTypeName();

  /**
   * Returns true if this field is annotated with {@link org.enso.runtime.parser.dsl.IRChild}.
   *
   * @return
   */
  boolean isChild();

  /**
   * Returns true if this field is child with {@link IRChild#required()} set to false.
   *
   * @return
   */
  boolean isNullable();

  /** Returns true if the type of this field is Java primitive. */
  boolean isPrimitive();

  /**
   * Returns true if this field extends {@link org.enso.compiler.core.ir.Expression} ({@link
   * org.enso.compiler.core.ir.JExpression}).
   *
   * <p>This is useful, e.g., for the {@link org.enso.compiler.core.IR#mapExpressions(Function)}
   * method.
   *
   * @return true if this field extends {@link org.enso.compiler.core.ir.Expression}
   */
  boolean isExpression();
}
