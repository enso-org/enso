package org.enso.interpreter.test.privateaccess;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.AllOf.allOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import org.enso.common.RuntimeOptions;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
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
    try (var ctx = ContextUtils.createDefaultContext()) {
      var res = ContextUtils.evalModule(ctx, src);
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
  public void privateConstructorIsNotExposedToPolyglot() throws IOException {
    var mainSrc = """
        type My_Type
            private Cons data
        """;
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("My_Project", mainSrc, projDir);
    var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var myType = mainMod.getType("My_Type");
      ContextUtils.executeInContext(
          ctx,
          () -> {
            var myTypeUnwrapped = ContextUtils.unwrapValue(ctx, myType);
            var interop = InteropLibrary.getUncached();
            var members = interop.getMembers(myTypeUnwrapped, false);
            assertThat(
                "My_Type should not have any 'public' members",
                interop.getArraySize(members),
                is(0L));
            return null;
          });
    }
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
    var out = new ByteArrayOutputStream();
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .out(out)
            .err(out)
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      try {
        polyCtx.getTopScope().compile(true);
        fail("Expected compiler error");
      } catch (PolyglotException e) {
        assertThat(
            out.toString(),
            allOf(
                containsString("error:"),
                containsString("Project-private constructor"),
                containsString("cannot be used from")));
      }
    }
  }
}
