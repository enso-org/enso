package org.enso.interpreter.test;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.util.Set;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class TypeErrorTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void typeErrorMessageContainsQualifiedNames() throws IOException {
    var modSrc = """
        type T
            Ctor

        make_t = T.Ctor
        """;
    var mainSrc =
        """
        from Standard.Base import all
        from project.Data.Mod import T
        from project.Data.Mod import make_t

        type T

        method (t:T) = t.baz

        main =
            t = make_t
            p = Panic.catch Any handler=(_.payload) <| method t
            p.to_text
        """;
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(QualifiedName.fromString("Main"), mainSrc),
            new SourceModule(QualifiedName.fromString("Data.Mod"), modSrc)),
        projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isString(), is(true));
          assertThat(
              res.asString(),
              allOf(
                  containsString("Type error"),
                  containsString("expected `t` to be"),
                  containsString("Data.Mod.T"),
                  containsString("Main.T")));
        });
  }
}
