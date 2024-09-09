package org.enso.interpreter.node.expression.builtin.meta;

import org.enso.interpreter.runtime.data.hash.EnsoHashMap;

/** Holds result of equality check with addtional information about warnings. */
public record EqualsAndInfo(boolean equals, EnsoHashMap warnings) {
  static final EqualsAndInfo TRUE = new EqualsAndInfo(true, null);
  static final EqualsAndInfo FALSE = new EqualsAndInfo(false, null);

  static EqualsAndInfo valueOf(boolean b) {
    return b ? TRUE : FALSE;
  }
}
