package org.enso.polyglot;

/** Container for polyglot method names */
public class MethodNames {
  public static class TopScope {
    public static final String CREATE_MODULE = "create_module";
    public static final String GET_MODULE = "get_module";
    public static final String LEAK_CONTEXT = "leak_context";
    public static final String REGISTER_MODULE = "register_module";
    public static final String UNREGISTER_MODULE = "unregister_module";
    public static final String COMPILE = "compile";
  }

  public static class Module {
    public static final String EVAL_EXPRESSION = "eval_expression";
    public static final String GET_ASSOCIATED_TYPE = "get_associated_type";
    public static final String GET_TYPE = "get_type";

    public static final String GET_METHOD = "get_method";
    public static final String GET_NAME = "get_name";
    public static final String REPARSE = "reparse";
    public static final String GENERATE_DOCS = "generate_docs";
    public static final String GATHER_IMPORT_STATEMENTS = "gather_import_statements";
    public static final String SET_SOURCE = "set_source";
    public static final String SET_SOURCE_FILE = "set_source_file";
  }

  public static class Function {
    public static final String EQUALS = "equals";
    public static final String GET_SOURCE_LENGTH = "get_source_length";
    public static final String GET_SOURCE_START = "get_source_start";
  }
}
