package org.enso.interpreter.test.privateaccess;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.RuntimeOptions;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class PrivateCheckDisabledTest extends TestBase {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void privateCtorCanBeAccessedWhenPrivateCheckIsDisabled() throws IOException {
    var libSrc = """
        type T
            private Cons data
        """;
    createProject("Lib", libSrc, tempFolder);
    var mainSrc =
        """
        from local.Lib import T
        main =
            obj = T.Cons 42
            obj.data
        """;
    var mainDir = createProject("Main", mainSrc, tempFolder);
    var ctxBuilder = defaultContextBuilder().option(RuntimeOptions.DISABLE_PRIVATE_CHECK, "true");
    testProjectRun(
        ctxBuilder,
        mainDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(42));
        });
  }
}
