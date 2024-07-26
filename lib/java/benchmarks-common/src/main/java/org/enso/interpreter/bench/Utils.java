package org.enso.interpreter.bench;

import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

public class Utils {
  private Utils() {}

  /**
   * Returns the main method associated with the given module's code.
   *
   * @param code The code of the module
   * @return The main method that can be executed.
   */
  public static Value getMainMethod(Context context, String code) {
    var src = Source.create(LanguageInfo.ID, code);
    var module = context.eval(src);
    var moduleType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var main = module.invokeMember(Module.GET_METHOD, moduleType, "main");
    if (!main.canExecute()) {
      throw new AssertionError("Main method should be executable");
    }
    return main;
  }
}
