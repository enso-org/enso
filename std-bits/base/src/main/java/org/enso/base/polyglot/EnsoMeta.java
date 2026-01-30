package org.enso.base.polyglot;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A helper class that makes calling Enso methods from Java libraries easier. */
public final class EnsoMeta {
  private static Value getBindings() {
    var ctx = Context.getCurrent();
    var bindings = ctx.getPolyglotBindings().getMember("ensoBindings");
    if (bindings != null) {
      return bindings.execute("enso");
    } else {
      return ctx.getBindings("enso");
    }
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
      ex.initCause(e);
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

  /** Creates an instance of an Enso type by calling the specified constructor. */
  public static Value makeInstance(
      String moduleName, String typeName, String constructorName, Object... args) {
    var type = getType(moduleName, typeName);

    Value constructor;
    try {
      constructor = type.getMember(constructorName);
    } catch (NullPointerException e) {
      var ex =
          new NullPointerException(
              "Cannot find constructor "
                  + constructorName
                  + " for "
                  + moduleName
                  + " type: "
                  + typeName);
      ex.initCause(e);
      throw ex;
    }

    if (!constructor.canInstantiate()) {
      throw new IllegalStateException("Constructor " + constructorName + " is not instantiable.");
    }
    return constructor.newInstance(args);
  }

  /** Converts an Enso error atom into a Java exception. */
  public static Value asDataflowError(Value ensoAtom) {
    var ensoError =
        EnsoMeta.getType("Standard.Base.Error", "Error").invokeMember("throw", ensoAtom);
    if (!ensoError.isException()) {
      throw new IllegalStateException(
          "Expected Enso error to be an exception, but got: " + ensoError);
    }
    return ensoError;
  }
}
