package org.enso.interpreter.test.exports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ModuleUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ExportConstructorTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void exportedConstructorsAreInBindingMap() throws IOException {
    var booleanMod =
        new SourceModule(
            QualifiedName.fromString("Boolean"),
            """
        type Boolean
            True
            False
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.Boolean.Boolean.True
        export project.Boolean.Boolean.False
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(booleanMod, mainMod), projDir);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var mainModExportedSymbols = ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainModExportedSymbols.size(), is(2));
      assertThat(mainModExportedSymbols, allOf(hasKey("True"), hasKey("False")));
    }
  }

  @Test
  public void constructorsCanBeExportedFromTheSameModule() throws IOException {
    var booleanMod =
        new SourceModule(
            QualifiedName.fromString("Boolean"),
            """
        export project.Boolean.Boolean.True
        export project.Boolean.Boolean.False
        type Boolean
            True
            False
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        # Import just the module on purpose
        import project.Boolean
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(booleanMod, mainMod), projDir);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var boolModExportedSymbols =
          ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Boolean");
      assertThat(boolModExportedSymbols.size(), is(3));
      assertThat(boolModExportedSymbols.keySet(), containsInAnyOrder("True", "False", "Boolean"));
    }
  }
}
