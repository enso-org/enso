package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.util.List;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.Warning.UnusedImport;
import org.enso.compiler.core.ir.Warning.UnusedSymbolsFromImport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.test.mock.WithCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;
import scala.jdk.javaapi.CollectionConverters;

public class UnusedImportsTest {
  @Rule public final WithCompilerContext compilerCtx = WithCompilerContext.createDefault();

  @Test
  public void canResolveSimpleImport() {
    compilerCtx.createModule(QualifiedName.fromString("local.Proj.Module"), "type My_Type");
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
        import project.Module.My_Type
        main = My_Type
        """);
    compilerCtx.getCompiler().run(mainMod);
    var modIr = mainMod.getIr();
    var bm = getBindingsMap(modIr);
    assertThat(bm.resolvedImports().size(), is(1));
  }

  @Test
  public void usageOfImport_CanBeRecognized_InNestedExpression() {
    compilerCtx.createModule(QualifiedName.fromString("local.Proj.Module"), "type My_Type");
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.My_Type

            main =
                func_1 _ =
                    func_2 _ =
                        func_3 _ =
                            My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    var bm = getBindingsMap(mainMod.getIr());
    assertThat(bm.resolvedImports().size(), is(1));
  }

  @Test
  public void unusedImport() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type_1
            type My_Type_2
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.My_Type_1
            import project.Module.My_Type_2

            main = My_Type_1
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().apply(1);
    expectWarning(imp);
  }

  @Test
  public void unusedSymbols_1() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type_1
            type My_Type_2
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import My_Type_1, My_Type_2
            main = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().apply(0);
    expectWarning(imp, List.of("local.Proj.Module.My_Type_1", "local.Proj.Module.My_Type_2"));
  }

  @Test
  public void unusedSymbols_2() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type_1
            type My_Type_2
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import My_Type_1, My_Type_2
            main = My_Type_2
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().apply(0);
    expectWarning(imp, List.of("local.Proj.Module.My_Type_1"));
  }

  @Test
  public void unusedSymbols_InTypeAscription() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type_1
            type My_Type_2
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import My_Type_1, My_Type_2
            foo (x:My_Type_1) = x
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().apply(0);
    expectWarning(imp, List.of("local.Proj.Module.My_Type_2"));
  }

  /** If there is no used symbol from {@code from ... import all} import, a warning is generated. */
  @Test
  public void noSymbolIsUsedForImportAll() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type_1
            type My_Type_2
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import all
            main = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().apply(0);
    expectWarning(imp);
  }

  /**
   * If there is at least one symbol used in {@code from ... import all} import, no warning is
   * generated.
   */
  @Test
  public void oneSymbolIsUsedForImportAll() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type_1
            type My_Type_2
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import all
            main = My_Type_1
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void noWarning_WhenImportingSymbolFromReexport() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Other_Module"),
        """
            type My_Type
            """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            export project.Other_Module.My_Type
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import My_Type
            main = My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void noWarningWhenSymbolIsUsedInExport_SimpleExport() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.My_Type
            export project.Module.My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void noWarningWhenSymbolIsUsedInExport_RenameExport() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.My_Type
            export project.Module.My_Type as Your_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void noWarningWhenSymbolIsUsedInExport_OnlyNamesExport() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.My_Type
            from project.Module export My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void ignoresDuplicatedImports() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            type My_Type
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.My_Type
            import project.Module.My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    var firstImp = mainMod.getIr().imports().apply(0);
    expectWarning(firstImp);
    var secondImp = mainMod.getIr().imports().apply(1);
    expectNoWarnings(secondImp);
  }

  @Test
  public void importConstructors() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Boolean"),
        """
            export project.Boolean.Boolean.False
            export project.Boolean.Boolean.True

            type Boolean
                False
                True
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Boolean import Boolean, False, True
            main = [Boolean, False, True]
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void canDetectUnusedMethods() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
            method x = x + 1
            """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import method
            main = method 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  private static void expectWarning(Import importIr, List<String> expectedUnusedSymbols) {
    var warn = getSingleWarning(importIr, UnusedSymbolsFromImport.class);
    var actualUnusedSymbols = CollectionConverters.asJava(warn.unusedSymbols());
    assertThat("Unused symbols do not match", actualUnusedSymbols, is(expectedUnusedSymbols));
  }

  private static void expectWarning(Import importIr) {
    var warn = getSingleWarning(importIr, UnusedImport.class);
    assertThat("UnusedImport warning is present", warn, is(notNullValue()));
  }

  private static void expectNoWarnings(Module modIr) {
    modIr
        .imports()
        .foreach(
            imp -> {
              expectNoWarnings(imp);
              return null;
            });
  }

  private static void expectNoWarnings(Import importIr) {
    var warn =
        importIr
            .getDiagnostics()
            .toList()
            .find(diag -> diag instanceof UnusedImport || diag instanceof UnusedSymbolsFromImport);
    assertThat("No warnings expected, but got: " + warn, warn.isEmpty(), is(true));
  }

  private static <W extends Warning> W getSingleWarning(Import importIr, Class<W> warningClass) {
    assertThat(
        "Must have at least one warning in diagnostics: " + importIr,
        importIr.diagnostics(),
        is(notNullValue()));
    var found =
        importIr
            .diagnostics()
            .toList()
            .find(diag -> warningClass.isAssignableFrom(diag.getClass()))
            .map(warningClass::cast);
    assertThat("Single UnusedImport warning expected on " + importIr, found.isDefined(), is(true));
    return found.get();
  }

  private static BindingsMap getBindingsMap(Module modIr) {
    return MetadataInteropHelpers.getMetadata(modIr, BindingAnalysis$.MODULE$, BindingsMap.class);
  }
}
