package org.enso.compiler.benchmarks.inline;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.compiler.benchmarks.CodeGenerator;
import org.enso.compiler.benchmarks.Utils;
import org.enso.interpreter.runtime.data.Type;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

class InlineContextUtils {
  private InlineContextUtils() {}

  static Set<String> localVarNames(int localVarsCnt) {
    Set<String> localVarNames = new HashSet<>();

    for (int i = 0; i < localVarsCnt; i++) {
      var varName = "loc_var_" + i;
      localVarNames.add(varName);
    }
    return localVarNames;
  }

  /**
   * Creates a main method, generates some local variables, and fills their identifiers in the given
   * set.
   *
   * @param localVarsCnt How many local variables should be initialized in the main method
   * @return Body of the main method
   */
  static InlineSource createMainMethodWithLocalVars(Context ctx, Set<String> localVarNames)
      throws IOException {
    var sb = new StringBuilder();
    sb.append("main = ").append(System.lineSeparator());
    var codeGen = new CodeGenerator();
    for (String localVarName : localVarNames) {
      var literal = codeGen.nextLiteral();
      sb.append("    ")
          .append(localVarName)
          .append(" = ")
          .append(literal)
          .append(System.lineSeparator());
    }
    // In the last expression of main method, use all the variables, so that there is no unused
    // variable warning
    var lastExpr =
        localVarNames.stream().reduce((acc, varName) -> acc + " + " + varName).orElseThrow();
    sb.append("    ").append(lastExpr).append(System.lineSeparator());
    var ensoCtx = Utils.leakEnsoContext(ctx);
    var srcFile = Utils.createSrcFile(sb.toString(), "inlineBenchmark.enso");
    var src = Source.newBuilder(LanguageInfo.ID, srcFile).build();
    var module = ctx.eval(src);
    var moduleAssocType = module.invokeMember(MethodNames.Module.GET_ASSOCIATED_TYPE);
    var assocTypeReceiver = (Type) Utils.unwrapReceiver(ctx, moduleAssocType);
    var moduleScope = assocTypeReceiver.getDefinitionScope();
    var compiler = ensoCtx.getCompiler();
    var inlineCtxMeta =
        new InlineContextResourceFactory(
            moduleScope, assocTypeReceiver, ensoCtx, compiler.packageRepository());
    return new InlineSource(sb.toString(), inlineCtxMeta, localVarNames);
  }

  static String createLongExpression(Set<String> localVars, int exprSize) {
    var codeGen = new CodeGenerator(localVars);
    return codeGen.createExpressionFromDefinedIdentifiers(exprSize);
  }
}
