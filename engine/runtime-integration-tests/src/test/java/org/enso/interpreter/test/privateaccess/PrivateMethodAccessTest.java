package org.enso.interpreter.test.privateaccess;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class PrivateMethodAccessTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();
  @Rule public ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void moduleDoesNotExposePrivateMethodsToPolyglot() {
    var module =
        ctx.eval(
            LanguageInfo.ID,
            """
            private priv_method x = x
            pub_method x = x
            """);
    var assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var privMethod = module.invokeMember(Module.GET_METHOD, assocType, "priv_method");
    assertThat("private method must not be exposed to polyglot", privMethod.isNull(), is(true));
    var pubMethod = module.invokeMember(Module.GET_METHOD, assocType, "pub_method");
    assertThat("public method is exposed to polyglot", pubMethod.canExecute(), is(true));
  }

  @Test
  public void typeDoesNotExposePrivateMethodsToPolyglot() {
    var module =
        ctx.eval(
            LanguageInfo.ID,
            """
            type My_Type
                private priv_method x = x
                pub_method x = x
            """);
    var myType = module.invokeMember(Module.GET_TYPE, "My_Type");
    var privMethod = module.invokeMember(Module.GET_METHOD, myType, "priv_method");
    assertThat("private method must not be exposed to polyglot", privMethod.isNull(), is(true));
    var pubMethod = module.invokeMember(Module.GET_METHOD, myType, "pub_method");
    assertThat("public method is exposed to polyglot", pubMethod.canExecute(), is(true));
  }

  @Test
  public void canCallPrivateMethod_AsCallback() throws IOException {
    var libDir = tempFolder.newFolder("Lib").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Mod"),
                """
                private priv_method x = x
                """),
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                from project.Mod import priv_method

                call_method ~callback =
                    callback priv_method
                """)),
        libDir);

    var projDir = tempFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import call_method

        callback priv_method =
            priv_method 42

        main =
            call_method callback
        """,
        projDir);

    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.asInt(), is(42));
        });
  }

  @Test
  public void canCallPrivateMethod_ViaLambda() throws IOException {
    var libDir = tempFolder.newFolder("Lib").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Mod"),
                """
                private priv_method =
                    42
                """),
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                from project.Mod import priv_method

                call_method ~callback =
                    callback \\_ -> priv_method
                """)),
        libDir);

    var projDir = tempFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import call_method

        callback priv_method =
            # Here, we actually call the lambda from `local.Lib.Main.call_method`
            priv_method 42

        main =
            call_method callback
        """,
        projDir);

    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.asInt(), is(42));
        });
  }

  @Test
  public void canCallPrivateMethod_UnresolvedSymbol() throws IOException {
    var libDir = tempFolder.newFolder("Lib").toPath();
    ProjectUtils.createProject(
        "Lib",
        """
        apply obj func =
            func obj
        """,
        libDir);

    var projDir = tempFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import apply

        type My_Type
            private Cons value

        main =
            mt = My_Type.Cons 42
            apply mt .value
        """,
        projDir);

    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.asInt(), is(42));
        });
  }
}
