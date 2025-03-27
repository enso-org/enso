package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ContextInitTest {
  @ClassRule public static final TemporaryFolder tmpFolder = new TemporaryFolder();

  @Test
  public void shouldChangeWorkingDir_BeforeExecutingProject() throws IOException {
    var projDir = tmpFolder.newFolder().toPath();
    var mainSrc =
        """
        from Standard.Base import all
        main =
            file = File.new "MY_FILE.txt"
            file.absolute.path
        """;
    ProjectUtils.createProject("Project", mainSrc, projDir);
    var out = new ByteArrayOutputStream();
    var ctxBuilder = ContextUtils.defaultContextBuilder().out(out).err(out);
    var expectedWorkingDir = projDir.getParent();
    var expectedPath = expectedWorkingDir.resolve("MY_FILE.txt").toAbsolutePath().toString();
    ProjectUtils.testProjectRun(
        ctxBuilder,
        projDir,
        res -> {
          assertThat(res.isString(), is(true));
          assertThat(res.asString(), is(expectedPath));
        });
    assertThat("Expected empty out, but got: " + out, out.toString().isEmpty(), is(true));
  }
}
