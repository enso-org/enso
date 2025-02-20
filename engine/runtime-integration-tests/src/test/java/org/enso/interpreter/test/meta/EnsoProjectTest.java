package org.enso.interpreter.test.meta;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import org.enso.common.LanguageInfo;
import org.enso.common.RuntimeOptions;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.scala.wrapper.ScalaConversions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.Source;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class EnsoProjectTest {
  @Rule public TemporaryFolder temporaryFolder = new TemporaryFolder();

  @Test
  public void noProjectWhenEvaluatingSingleFile() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      var res =
          ContextUtils.evalModule(
              ctx,
              """
          from Standard.Base import all
          from Standard.Base.Errors.Common import Module_Not_In_Package_Error

          main =
              enso_project.is_error
          """);
      assertThat(res, notNullValue());
      assertThat(res.asBoolean(), is(true));
    }
  }

  @Test
  public void ensoProjectWorksInOneProject() throws IOException {
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from Standard.Base import all
        main =
            enso_project.name
        """);
    var projDir = temporaryFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        (res) -> {
          assertThat(res.asString(), is("Proj"));
        });
  }

  @Test
  public void ensoProjectWorksInTwoProjects() throws IOException {
    var mainMod1 =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from Standard.Base import all

        get_enso_project_name =
            enso_project.name
        """);
    var mainMod2 =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from local.Proj1 import get_enso_project_name
        main =
            get_enso_project_name
        """);
    var projDir1 = temporaryFolder.newFolder().toPath();
    var projDir2 = temporaryFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj1", Set.of(mainMod1), projDir1);
    ProjectUtils.createProject("Proj2", Set.of(mainMod2), projDir2);
    ProjectUtils.testProjectRun(
        projDir2,
        (res) -> {
          assertThat(res.asString(), is("Proj2"));
        });
  }

  @Test
  public void ensoProjectCanBeCalledFromJava() throws IOException {
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from Standard.Base import all
        main =
            42
        """);
    var projDir = temporaryFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mainMod), projDir);
    var mainModFile = projDir.resolve("src").resolve("Main.enso");
    assertThat(mainModFile.toFile().exists(), is(true));
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var mainSrc = Source.newBuilder(LanguageInfo.ID, mainModFile.toFile()).build();
      // First eval the source so that everything is compiled.
      ctx.eval(mainSrc);
      var polyCtx = new PolyglotContext(ctx);
      var mod = polyCtx.getTopScope().getModule("Standard.Base.Meta.Enso_Project");
      var assocType = mod.getAssociatedType();
      var ensoProjMethod = mod.getMethod(assocType, "enso_project").get();
      var projDescr = ensoProjMethod.execute(ScalaConversions.seq(List.of(assocType)));
      assertThat(projDescr.hasMembers(), is(true));
      assertThat(projDescr.getMetaObject().getMetaSimpleName(), is("Project_Description"));
      assertThat(projDescr.hasMember("name"), is(true));
      assertThat(projDescr.invokeMember("name").asString(), is("Proj"));
    }
  }
}
