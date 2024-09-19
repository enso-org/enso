package org.enso.base.polyglot;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A helper class that makes calling Enso methods from Java libraries easier. */
public final class EnsoMeta {
  private static Value getBindings() {
    return Context.getCurrent().getBindings("enso");
  }

  /** Returns a type object from the Enso runtime. */
  public static Value getType(String moduleName, String typeName) {
    var module = getBindings().invokeMember("get_module", moduleName);
    try {
      return module.invokeMember("get_type", typeName);
    } catch (NullPointerException e) {
      var ex =
          new NullPointerException(
              "Cannot get type for " + moduleName + " type: " + typeName + " at " + module);
      ex.initCause(ex);
      throw ex;
    }
  }

  /** Calls a static method defined directly on a module (not inside of a type). */
  public static Value callStaticModuleMethod(String moduleName, String methodName, Object... args) {
    var module = getBindings().invokeMember("get_module", moduleName);
    var moduleType = module.invokeMember("get_associated_type");
    var factory = module.invokeMember("get_method", moduleType, methodName);
    // The static method takes the module as the synthetic 'self' argument, so we need to prepend
    // it:
    Object[] argsWithSelf = new Object[args.length + 1];
    argsWithSelf[0] = moduleType;
    System.arraycopy(args, 0, argsWithSelf, 1, args.length);
    return factory.execute(argsWithSelf);
  }
}
