package org.enso.interpreter.runtime.type;

public class Constants {

  public static final String BUILTIN_NAMESPACE = "Standard.Builtins.Main";
  // Hard-coded names of remaining constants that are not Builtins
  // but refer to builtin-related internals
  public static final String MODULE_SCOPE = BUILTIN_NAMESPACE + ".Module_Scope";
  public static final String THUNK = BUILTIN_NAMESPACE + ".Thunk";
  public static final String UNRESOLVED_SYMBOL = BUILTIN_NAMESPACE + ".Unresolved_Symbol";
}
