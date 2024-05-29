package org.enso.interpreter.test.privateaccess;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
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
    ProjectUtils.createProject("Lib", libSrc, libDir);
    var mainSrc =
        """
        from local.Lib import T
        main =
            obj = T.Cons 42
            obj.data
        """;
    var mainDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Main", mainSrc, mainDir);
    var ctxBuilder =
        ContextUtils.defaultContextBuilder().option(RuntimeOptions.DISABLE_PRIVATE_CHECK, "true");
    ProjectUtils.testProjectRun(
        ctxBuilder,
        mainDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(42));
        });
  }
}
