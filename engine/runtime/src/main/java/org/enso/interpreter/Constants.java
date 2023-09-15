package org.enso.interpreter;

import org.enso.compiler.core.ConstantsNames;

/** Language-level constants for use throughout the program. */
public class Constants {
  public static final String SCOPE_SEPARATOR = ".";

  /** Names for different language elements. */
  public static class Names {
    public static final String SELF_ARGUMENT = ConstantsNames.SELF_ARGUMENT;
    public static final String SELF_TYPE_ARGUMENT = ConstantsNames.SELF_TYPE_ARGUMENT;
    public static final String THAT_ARGUMENT = ConstantsNames.THAT_ARGUMENT;
    public static final String FROM_MEMBER = ConstantsNames.FROM_MEMBER;
  }

  /** Cache sizes for different AST nodes. */
  public static class CacheSizes {
    public static final String ARGUMENT_SORTER_NODE = "10";
    public static final String FUNCTION_INTEROP_LIBRARY = "10";
    public static final String THUNK_EXECUTOR_NODE = "10";
    public static final String EVAL_NODE = "10";
    public static final int BUILTIN_INTEROP_DISPATCH = 10;
  }
}
