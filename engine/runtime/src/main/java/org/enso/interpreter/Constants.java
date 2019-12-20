package org.enso.interpreter;

/** Language-level constants for use throughout the program. */
public class Constants {
  public static final String LANGUAGE_ID = "enso";
  public static final String LANGUAGE_NAME = "Enso";
  public static final String IMPL_NAME = "EnsoRuntime";
  public static final String LANGUAGE_VERSION = "0.0.1";
  public static final String MIME_TYPE = "application/x-enso";
  public static final String FILE_EXTENSION = ".enso";

  public static final String SCOPE_SEPARATOR = ".";

  /** Names for different language elements. */
  public static class Names {
    public static final String THIS_ARGUMENT_NAME = "this";
    public static final String CURRENT_MODULE_VARIABLE_NAME = "here";
    public static final String ANY_TYPE_NAME = "Any";
  }

  /** Cache sizes for different AST nodes. */
  public static class CacheSizes {
    public static final String ARGUMENT_SORTER_NODE = "10";
    public static final String FUNCTION_INTEROP_LIBRARY = "10";
    public static final String THUNK_EXECUTOR_NODE = "10";
    public static final String EVAL_NODE = "10";
  }
}
