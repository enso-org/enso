package org.enso.interpreter.test.privateaccess;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.AllOf.allOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import org.enso.common.RuntimeOptions;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.PolyglotException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class PrivateConstructorAccessTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void privateConstructorCanBeCalledInUnknownProject() {
    var src =
        """
        type My_Type
            private Cons data
        main =
            obj = My_Type.Cons 42
            obj.data
        """;
    try (var ctx = ContextUtils.createDefault()) {
      var res = ctx.evalModule(src);
      assertThat(res.isNumber(), is(true));
      assertThat(res.asInt(), is(42));
    }
  }

  @Test
  public void accessMethodOnATypeWithAllPrivateConstructors() throws IOException {
    var codeA =
        """
        type A
            private Cons data
            find d = A.Cons d
        """;
    var codeUse =
        """
        import local.Proj_A
        create x = local.Proj_A.Main.A.find x
        main = create
        """;
    var proj1Dir = tempFolder.newFolder("Proj_A").toPath();
    ProjectUtils.createProject("Proj_A", codeA, proj1Dir);
    var proj2Dir = tempFolder.newFolder("Proj_Use").toPath();
    ProjectUtils.createProject("Proj_Use", codeUse, proj2Dir);
    ProjectUtils.testProjectRun(
        proj2Dir,
        create -> {
          var res = create.execute("Hello");
          assertEquals("It is object: " + res, "(Cons 'Hello')", res.toString());
        });
  }

  @Test
  public void privateConstructorIsNotExposedToPolyglot() throws Exception {
    var mainSrc =
        """
        type My_Type
            private Cons data
        """;
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("My_Project", mainSrc, projDir);
    var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString()))
            .build()) {
      handlePrivateConstructorIsNotExposedCheck(ctx, mainSrcPath);
    }
  }

  private void handlePrivateConstructorIsNotExposedCheck(final ContextUtils ctx, Path mainSrcPath)
      throws UnsupportedMessageException {
    var mainMod = new PolyglotContext(ctx.context()).evalModule(mainSrcPath.toFile());
    var myType = mainMod.getType("My_Type");
    var myTypeUnwrapped = ctx.unwrapValue(myType);
    var interop = InteropLibrary.getUncached();
    var members = interop.getMembers(myTypeUnwrapped, false);
    assertThat(
        "My_Type should not have any 'public' members", interop.getArraySize(members), is(0L));
  }

  @Test
  public void canPatternMatchOnPrivateConstructorFromSameProject() throws IOException {
    var mainSrc =
        """
        type My_Type
            private Cons data
        main =
            obj = My_Type.Cons 42
            case obj of
                My_Type.Cons x -> x
                _ -> 0
        """;
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("My_Project", mainSrc, projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(42));
        });
  }

  /** Tests that pattern matching on private constructors fails in compilation. */
  @Test
  public void cannotPatternMatchOnPrivateConstructorFromDifferentProject() throws IOException {
    var libSrc =
        """
        type My_Type
            private Cons data
            create x = My_Type.Cons x
        """;
    ProjectUtils.createProject("Lib", libSrc, tempFolder.newFolder("Lib").toPath());
    var projSrc =
        """
        from local.Lib import My_Type
        main =
            obj = My_Type.create 42
            case obj of
                My_Type.Cons x -> x
        """;
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", projSrc, projDir);

    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
                        .option(RuntimeOptions.STRICT_ERRORS, "true")
                        .option(RuntimeOptions.DISABLE_IR_CACHES, "true"))
            .build()) {
      try {
        ctx.topScope().compile(true);
        fail("Expected compiler error");
      } catch (PolyglotException e) {
        assertThat(
            ctx.getOut(),
            allOf(
                containsString("error:"),
                containsString("Project-private constructor"),
                containsString("cannot be used from")));
      }
    }
  }

  @Test
  public void canCallPrivateConstructor_ViaCallback() throws Exception {
    var libDir = tempFolder.newFolder("Lib").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("My_Type"),
                """
                private

                type My_Type
                    Cons name
                """),
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.My_Type.My_Type

                call_method ~callback =
                    callback My_Type.Cons
                """)),
        libDir);

    var projDir = tempFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import call_method

        callback cons =
            cons "Name"

        main =
            call_method callback . to_text
        """,
        projDir);

    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.asString(), containsString("Cons 'Name'"));
        });
  }

  @Test
  public void canCallPrivateConstructor_ViaLambda() throws Exception {
    var libDir = tempFolder.newFolder("Lib").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("My_Type"),
                """
                private

                type My_Type
                    Cons name
                """),
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.My_Type.My_Type

                call_method ~callback =
                    callback \\it -> My_Type.Cons it
                """)),
        libDir);

    var projDir = tempFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import call_method

        callback cons =
            cons "Name"

        main =
            call_method callback . to_text
        """,
        projDir);

    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.asString(), containsString("Cons 'Name'"));
        });
  }
}
