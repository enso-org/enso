package org.enso.polyglot;

import java.io.File;
import java.io.IOException;
import org.enso.common.LanguageInfo;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

public record PolyglotContext(Context context) {
  /**
   * Evaluates provided code string as a new module.
   *
   * @param code the code to evaluate.
   * @param moduleName the name for the newly parsed module.
   * @return the module representing evaluated code.
   */
  public Module evalModule(String code, String moduleName) {
    var source = Source.newBuilder(LanguageInfo.ID, code, moduleName).buildLiteral();
    return new Module(context.eval(source));
  }

  /**
   * Evaluates provided code file as a new module.
   *
   * @param codeFile the code to evaluate.
   * @return the module representing evaluated code.
   */
  public Module evalModule(File codeFile) throws IOException {
    var source = Source.newBuilder(LanguageInfo.ID, codeFile).build();
    return new Module(context.eval(source));
  }

  /**
   * Generates and evaluates default repl script for this context.
   *
   * @param mainMethodName name of the main method
   * @return module representing evaluated code
   */
  public Module evalReplModule(String mainMethodName) {
    var replModuleName = "Internal_Repl_Module___";
    var sb = new StringBuilder();
    sb.append("import Standard.Base.Runtime.Debug\n");
    for (var libName : getTopScope().getLibraries()) {
      sb.append("from ").append(libName).append(" import all\n");
    }
    sb.append("\n");
    sb.append(mainMethodName).append(" = Debug.breakpoint");
    return evalModule(sb.toString(), replModuleName);
  }

  /**
   * @return the top scope of Enso execution context
   */
  public TopScope getTopScope() {
    return new TopScope(context.getBindings(LanguageInfo.ID));
  }
}
