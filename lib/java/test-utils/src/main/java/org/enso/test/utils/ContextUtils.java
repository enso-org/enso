package org.enso.test.utils;

import java.util.Set;
import java.util.stream.Collectors;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.common.MethodNames.TopScope;
import org.enso.interpreter.runtime.EnsoContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

/** A collection of classes and methods useful for testing {@link Context} related stuff. */
public final class ContextUtils {

  private ContextUtils() {}

  private static EnsoContext leakContext(Context ctx) {
    return ctx.getBindings(LanguageInfo.ID)
        .invokeMember(TopScope.LEAK_CONTEXT)
        .as(EnsoContext.class);
  }

  /**
   * Evaluates the given source as if it was in an unnamed module.
   *
   * @param ctx context to evaluate the module at
   * @param src The source code of the module
   * @return The value returned from the main method of the unnamed module.
   */
  public static Value evalModule(Context ctx, CharSequence src) {
    return evalModule(ctx, src, null, "main");
  }

  /**
   * Evaluates the given source as if it was in a module with given name.
   *
   * @param ctx context to evaluate the module at
   * @param src The source code of the module
   * @param name name of the module defining the source
   * @param methodName name of main method to invoke
   * @return The value returned from the main method of the unnamed module.
   */
  static Value evalModule(Context ctx, CharSequence src, String name, String methodName) {
    Source s;
    if (name == null) {
      s = Source.create("enso", src);
    } else {
      var b = Source.newBuilder("enso", src, name);
      s = b.buildLiteral();
    }
    return evalModule(ctx, s, methodName);
  }

  /**
   * Evaluates the given source as if it was in a module with given name.
   *
   * @param ctx context to evaluate the module at
   * @param src The source code of the module
   * @param methodName name of main method to invoke
   * @return The value returned from the main method of the unnamed module.
   */
  public static Value evalModule(Context ctx, Source src, String methodName) {
    var module = ctx.eval(src);
    var assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var method = module.invokeMember(Module.GET_METHOD, assocType, methodName);
    return "main".equals(methodName) ? method.execute() : method.execute(assocType);
  }

  /**
   * Parses the given module and returns a method by the given name from the module.
   *
   * @param moduleSrc Source of the whole module
   * @return Reference to the method.
   */
  static Value getMethodFromModule(Context ctx, String moduleSrc, String methodName) {
    Value module = ctx.eval(Source.create("enso", moduleSrc));
    return module.invokeMember(Module.EVAL_EXPRESSION, methodName);
  }

  /**
   * Returns set of all the builtin methods from Any. These methods are present even if the module
   * was not imported - they are present on the Any builtin type. This is in contrast to {@link
   * #allMethodsFromAny(Context)} which requires the {@code Standard.Base.Any} module to be first
   * imported.
   */
  static Set<String> builtinMethodsFromAny(Context ctx) {
    var ensoCtx = ContextUtils.leakContext(ctx);
    // This is a builtin Any type, so only the builtin methods will be included.
    var anyBuiltinType = ensoCtx.getBuiltins().any();
    var anyBuiltinMethods = anyBuiltinType.getDefinitionScope().getMethodsForType(anyBuiltinType);
    assert anyBuiltinMethods != null;
    return anyBuiltinMethods.stream()
        .map(m -> unqualifiedName(m.getName()))
        .collect(Collectors.toUnmodifiableSet());
  }

  /**
   * Returns set of all the methods on the {@code Standard.Base.Any} type. This includes both
   * builtin and non-builtin types. For this to work, {@code Standard.Base.Any} module must be
   * imported first in the context, otherwise an assertion will fail.
   */
  static Set<String> allMethodsFromAny(Context ctx) {
    // Includes, e.g., `Any.to`.
    var ensoCtx = ContextUtils.leakContext(ctx);
    var anyMod = ensoCtx.findModule("Standard.Base.Any");
    assert anyMod.isPresent() : "Standard.Base.Any module must be imported first";
    var anyModScope = anyMod.get().getScope();
    var anyType = anyModScope.getType("Any", true);
    var anyMethods = anyModScope.getMethodsForType(anyType);
    assert anyMethods != null;
    return anyMethods.stream()
        .map(m -> unqualifiedName(m.getName()))
        .collect(Collectors.toUnmodifiableSet());
  }

  private static String unqualifiedName(String name) {
    if (name.contains(".")) {
      return name.substring(name.lastIndexOf('.') + 1);
    }
    return name;
  }

}
