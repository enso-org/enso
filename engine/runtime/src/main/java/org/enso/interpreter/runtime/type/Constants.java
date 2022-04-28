package org.enso.interpreter.runtime.type;

/** Hard-coded names of builtins. */
public class Constants {

  public static final String ANY = "Standard.Base.Data.Any.Any";
  public static final String ARRAY = "Standard.Base.Data.Array.Array";
  public static final String BOOLEAN = "Standard.Base.Data.Boolean.Boolean";
  public static final String DECIMAL = "Standard.Base.Data.Numbers.Decimal";
  public static final String ERROR = "Standard.Base.Error.Common.Error";
  public static final String FUNCTION = "Standard.Base.Function.Function";
  public static final String INTEGER = "Standard.Base.Data.Numbers.Integer";
  public static final String MANAGED_RESOURCE = "Standard.Base.Runtime.Resource.Managed_Resource";
  public static final String NOTHING = "Standard.Base.Nothing.Nothing";
  public static final String NUMBER = "Standard.Base.Data.Numbers.Number";
  public static final String PANIC = "Standard.Base.Error.Common.Panic";
  public static final String REF = "Standard.Base.Data.Ref.Ref";
  public static final String TEXT = "Standard.Base.Data.Text.Text";

  // Remaining constants that are not Builtins but refer to builtin-related internals
  public static final String MODULE_SCOPE = "Standard.Builtins.Main.Module_Scope";
  public static final String THUNK = "Standard.Builtins.Main.Thunk";
  public static final String UNRESOLVED_SYMBOL = "Standard.Builtins.Main.Unresolved_Symbol";
}
