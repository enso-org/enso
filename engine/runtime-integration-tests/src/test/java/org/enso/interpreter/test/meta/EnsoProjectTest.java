package org.enso.interpreter.test.meta;

import java.io.IOException;
import java.util.Set;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.containsString;


public class EnsoProjectTest {
  @Rule
  public TemporaryFolder temporaryFolder = new TemporaryFolder();

  @Test
  public void noProjectWhenEvaluatingSingleFile() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      var res = ContextUtils.evalModule(ctx, """
          from Standard.Base import all
          from Standard.Base.Errors.Common import Module_Not_In_Package_Error
          
          main =
              enso_project.is_a Module_Not_In_Package_Error
          """);
      assertThat(res, notNullValue());
      assertThat(res.asBoolean(), is(true));
    }
  }

  @Test
  public void ensoProjectWorksInOneProject() throws IOException {
    var mainMod = new SourceModule(QualifiedName.fromString("Main"), """
        from Standard.Base import all
        main =
            enso_project.name
        """);
    var projDir = temporaryFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mainMod), projDir);
    ProjectUtils.testProjectRun(projDir, (res) -> {
      assertThat(res.asString(), is("Proj"));
    });
  }

  @Test
  public void ensoProjectWorksInTwoProjects() throws IOException {
    var mainMod1 = new SourceModule(QualifiedName.fromString("Main"), """
        from Standard.Base import all
        
        get_enso_project_name =
            enso_project.name
        """);
    var mainMod2 = new SourceModule(QualifiedName.fromString("Main"), """
        from local.Proj1 import get_enso_project_name
        main =
            get_enso_project_name
        """);
    var projDir1 = temporaryFolder.newFolder().toPath();
    var projDir2 = temporaryFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj1", Set.of(mainMod1), projDir1);
    ProjectUtils.createProject("Proj2", Set.of(mainMod2), projDir2);
    ProjectUtils.testProjectRun(projDir1, (res) -> {
      assertThat(res.asString(), is("Proj2"));
    });
  }
}
