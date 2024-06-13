package org.enso.interpreter.test.exports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ExportStaticMethodTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void staticMethodCanBeExportedByName() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"), """
        static_method x = x
        """);
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        from project.T_Module export static_method
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.A_Module import all
        main =
            static_method 42
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(tMod, aMod, mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(42));
        });
  }
}
