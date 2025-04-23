package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.interpreter.runtime.util.DiagnosticFormatter;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

public class DiagnosticFormatterTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(
              b -> b.option(RuntimeOptions.STRICT_ERRORS, "true").environment("NO_COLOR", "true"))
          .build();

  @Before
  public void resetOut() {
    ctxRule.resetOut();
  }

  @Test
  public void testOneLineDiagnostics() throws IOException {
    var code = "main = foo";
    var polyglotSrc =
        org.graalvm.polyglot.Source.newBuilder(LanguageInfo.ID, code, "tmp_test.enso").build();
    var expectedDiagnostics =
        """
tmp_test:1:8: error: The name `foo` could not be found.
    1 | main = foo
      |        ^~~""";
    try {
      var module = ctxRule.eval(polyglotSrc);
      module.invokeMember(Module.EVAL_EXPRESSION, "main");
    } catch (PolyglotException e) {
      assertThat(ctxRule.getOut(), containsString(expectedDiagnostics));
    }
    var moduleOpt = ctxRule.ensoContext().getTopScope().getModule("tmp_test");
    assertThat(moduleOpt.isPresent(), is(true));
    var moduleIr = moduleOpt.get().getIr();
    var diags = gatherDiagnostics(moduleIr);
    assertThat("There should be just one Diagnostic in main method", diags.size(), is(1));

    var src = Source.newBuilder(LanguageInfo.ID, code, "tmp_test").build();
    var diag = diags.get(0);
    var diagFormatter = new DiagnosticFormatter(diag, src, true, false);
    var formattedDiag = diagFormatter.format();
    assertThat(formattedDiag, containsString(expectedDiagnostics));
  }

  private static List<Diagnostic> gatherDiagnostics(org.enso.compiler.core.ir.Module moduleIr) {
    List<Diagnostic> diags = new ArrayList<>();
    moduleIr
        .preorder()
        .foreach(
            ir -> {
              if (ir instanceof Diagnostic diag) {
                diags.add(diag);
              }
              return null;
            });
    return diags;
  }
}
