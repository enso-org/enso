package org.enso.interpreter;

import org.enso.compiler.core.ConstantsNames;

/** Language-level constants for use throughout the program. */
public final class Constants {
  private Constants() {}

  public static final String SCOPE_SEPARATOR = ".";

  /** Names for different language elements. */
  public static interface Names extends ConstantsNames {}

  /** Cache sizes for different AST nodes. */
  public static class CacheSizes {
    private CacheSizes() {}

    public static final String ARGUMENT_SORTER_NODE = "10";
    public static final String FUNCTION_INTEROP_LIBRARY = "10";
    public static final String THUNK_EXECUTOR_NODE = "10";
    public static final String EVAL_NODE = "10";
    public static final int BUILTIN_INTEROP_DISPATCH = 10;
  }
}
