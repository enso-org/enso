package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.test.mock.WithMockCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

public class UnusedImportsTest {
  @Rule public final WithMockCompilerContext compilerCtx = WithMockCompilerContext.createDefault();

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

  private static BindingsMap getBindingsMap(Module modIr) {
    return MetadataInteropHelpers.getMetadata(modIr, BindingAnalysis$.MODULE$, BindingsMap.class);
  }
}
