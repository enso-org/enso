package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.interpreter.runtime.util.DiagnosticFormatter;
import org.enso.interpreter.runtime.util.GitHubDiagnosticFormatter;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.junit.Assert;
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

  private final String exampleExpectedDiagnostics =
      """
      tmp_test:1:8: error: The name `foo` could not be found.
          1 | main = foo
            |        ^~~\
      """;

  private Map.Entry<Diagnostic, Source> compileExample() throws IOException {
    var code = "main = foo";
    var polyglotSrc =
        org.graalvm.polyglot.Source.newBuilder(LanguageInfo.ID, code, "tmp_test.enso").build();
    try {
      var module = ctxRule.eval(polyglotSrc);
      module.invokeMember(Module.EVAL_EXPRESSION, "main");
      Assert.fail("Expected error.");
    } catch (PolyglotException e) {
      assertThat(ctxRule.getOut(), containsString(exampleExpectedDiagnostics));
    }

    var moduleOpt = ctxRule.ensoContext().getTopScope().getModule("tmp_test");
    assertThat(moduleOpt.isPresent(), is(true));
    var moduleIr = moduleOpt.get().getIr();
    var diags = gatherDiagnostics(moduleIr);
    assertThat("There should be just one Diagnostic in main method", diags.size(), is(1));

    var src = Source.newBuilder(LanguageInfo.ID, code, "tmp_test").build();
    return new AbstractMap.SimpleEntry<>(diags.get(0), src);
  }

  @Test
  public void testOneLineDiagnostics() throws IOException {
    var pair = compileExample();
    var diag = pair.getKey();
    var src = pair.getValue();
    var diagFormatter = new DiagnosticFormatter(diag, src, true, false);
    var formattedDiag = diagFormatter.format();
    assertThat(formattedDiag, containsString(exampleExpectedDiagnostics));
  }

  @Test
  public void testGithubDiagnostics() throws IOException {
    var expectedGithubCommand =
        "::error line=1,col=8,file=tmp_test,title=Enso Compiler Error @ tmp_test,endCol=10::The"
            + " name `foo` could not be found.";
    var pair = compileExample();
    var diag = pair.getKey();
    var src = pair.getValue();
    var diagFormatter = new GitHubDiagnosticFormatter(diag, src, true, false);
    var formattedDiag = diagFormatter.format();
    assertThat(formattedDiag, containsString(exampleExpectedDiagnostics));
    assertThat(formattedDiag, containsString(expectedGithubCommand));
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
