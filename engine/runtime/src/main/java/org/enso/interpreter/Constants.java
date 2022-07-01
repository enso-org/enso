package org.enso.interpreter;

/** Language-level constants for use throughout the program. */
public class Constants {
  public static final String SCOPE_SEPARATOR = ".";

  /** Names for different language elements. */
  public static class Names {
    public static final String SELF_ARGUMENT = "self";
    public static final String THAT_ARGUMENT = "that";
    public static final String CURRENT_MODULE = "here";
    public static final String FROM_MEMBER = "from";
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
