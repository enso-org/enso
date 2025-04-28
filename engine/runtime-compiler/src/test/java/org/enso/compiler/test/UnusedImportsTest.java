package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Set;
import org.apache.commons.vfs2.FileObject;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.test.mock.SourceModule;
import org.enso.compiler.test.mock.WithMockCompilerContext;
import org.enso.editions.LibraryName;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

public class UnusedImportsTest {
  @Rule public final WithMockCompilerContext compilerCtx = WithMockCompilerContext.createDefault();

  @Test
  public void canResolveSimpleImport() {
    var pkgName = LibraryName.apply("local", "Proj");
    var srcModules =
        Set.of(
            new SourceModule(QualifiedName.simpleName("Module"), "type My_Type"),
            new SourceModule(
                QualifiedName.simpleName("Main"),
                """
            import project.Module.My_Type
            main = My_Type
            """));
    Package<FileObject> pkg = compilerCtx.createPackage(pkgName, srcModules);
    compilerCtx.registerMainProjectPackage(pkgName, pkg);
    var mainMod = compilerCtx.findModule(QualifiedName.fromString("local.Proj.Main"));
    compilerCtx.getCompiler().run(mainMod);
    var modIr = mainMod.getIr();
    var bm = getBindingsMap(modIr);
    assertThat(bm.resolvedImports().size(), is(1));
  }

  @Test
  public void usageOfImport_CanBeRecognized_InNestedExpression() {
    var pkgName = LibraryName.apply("local", "Proj");
    var srcModules =
        Set.of(
            new SourceModule(QualifiedName.simpleName("Module"), "type My_Type"),
            new SourceModule(
                QualifiedName.simpleName("Main"),
                """
            import project.Module.My_Type

            main =
                func_1 _ =
                    func_2 _ =
                        func_3 _ =
                            My_Type
            """));
    Package<FileObject> pkg = compilerCtx.createPackage(pkgName, srcModules);
    compilerCtx.registerMainProjectPackage(pkgName, pkg);
    var mainMod = compilerCtx.findModule(QualifiedName.fromString("local.Proj.Main"));
    compilerCtx.getCompiler().run(mainMod);
    var bm = getBindingsMap(mainMod.getIr());
    assertThat(bm.resolvedImports().size(), is(1));
  }

  private static BindingsMap getBindingsMap(Module modIr) {
    return MetadataInteropHelpers.getMetadata(modIr, BindingAnalysis$.MODULE$, BindingsMap.class);
  }
}
