package org.enso.interpreter.test.privateaccess;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.TestUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class PrivateCheckDisabledTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void privateCtorCanBeAccessedWhenPrivateCheckIsDisabled() throws IOException {
    var libSrc = """
        type T
            private Cons data
        """;
    var libDir = tempFolder.newFolder().toPath();
    TestUtils.createProject("Lib", libSrc, libDir);
    var mainSrc =
        """
        from local.Lib import T
        main =
            obj = T.Cons 42
            obj.data
        """;
    var mainDir = tempFolder.newFolder().toPath();
    TestUtils.createProject("Main", mainSrc, mainDir);
    var ctxBuilder =
        TestUtils.defaultContextBuilder().option(RuntimeOptions.DISABLE_PRIVATE_CHECK, "true");
    TestUtils.testProjectRun(
        ctxBuilder,
        mainDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(42));
        });
  }
}
