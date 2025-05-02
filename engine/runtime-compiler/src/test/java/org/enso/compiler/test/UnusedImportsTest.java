package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.util.List;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Warning.UnusedImport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.test.mock.WithCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters;

public class UnusedImportsTest {
  @Rule
  public final WithCompilerContext compilerCtx =
      WithCompilerContext.newBuilder()
          .withModifiedCompilerConfig(bldr -> bldr.dumpModuleIR(Option.apply("Main")))
          .build();

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
  public void unusedSymbol() {
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
    expectWarning(imp, List.of("local.Proj.Module.My_Type_2"));
  }

  private static void expectWarning(Import importIr, List<String> expectedUnusedSymbols) {
    var warn = getSingleWarning(importIr);
    var actualUnusedSymbols = CollectionConverters.asJava(warn.unusedSymbols());
    assertThat("Unused symbols do not match", actualUnusedSymbols, is(expectedUnusedSymbols));
  }

  private static UnusedImport getSingleWarning(Import importIr) {
    assertThat(
        "Must have at least one warning in diagnostics: " + importIr,
        importIr.diagnostics(),
        is(notNullValue()));
    var found =
        importIr
            .diagnostics()
            .toList()
            .find(diag -> diag instanceof UnusedImport)
            .map(diag -> (UnusedImport) diag);
    assertThat("Single UnusedImport warning expected on " + importIr, found.isDefined(), is(true));
    return found.get();
  }

  private static BindingsMap getBindingsMap(Module modIr) {
    return MetadataInteropHelpers.getMetadata(modIr, BindingAnalysis$.MODULE$, BindingsMap.class);
  }
}
