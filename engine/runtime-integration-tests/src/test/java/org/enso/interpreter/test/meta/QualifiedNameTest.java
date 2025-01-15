package org.enso.interpreter.test.meta;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import org.enso.test.utils.ProjectUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class QualifiedNameTest {
  @Rule public TemporaryFolder temporaryFolder = new TemporaryFolder();

  private static final String mainModSrc =
      """
      from Standard.Base import all

      type My_Type
          Value x

      main =
          obj = My_Type.Value 42
          Meta.get_qualified_type_name obj
      """;

  @Test
  public void qualifiedTypeNameWorks_WhenRunningSingleFile() throws IOException {
    var projDir = temporaryFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", mainModSrc, projDir);
    ProjectUtils.testProjectRun(
        projDir,
        (res) -> {
          assertThat(res.asString(), is("local.Proj.Main.My_Type"));
        });
  }
}
