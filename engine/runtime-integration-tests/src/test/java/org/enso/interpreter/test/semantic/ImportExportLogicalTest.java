package org.enso.interpreter.test.semantic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Set;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ImportExportLogicalTest {

  @ClassRule public static final TemporaryFolder tmp = new TemporaryFolder();
  @ClassRule public static final ContextUtils ctx = prepareLogicalExportProject();

  private static ContextUtils prepareLogicalExportProject() {
    try {
      tmp.create();
      var projDir = tmp.newFolder("import-export-logical-test").toPath();

      var main =
          new SourceModule(
              QualifiedName.simpleName("Main"),
              """
              export project.Api.Element
              """);
      var apiElement =
          new SourceModule(
              QualifiedName.fromString("Api.Element"),
              """
              import project.Impl.Internal.Internal

              type Element
                  Value impl:Internal

                  create = Element.Value (Internal.Impl)

                  describe self = "Element with " + self.impl.describe
              """);
      var implInternal =
          new SourceModule(
              QualifiedName.fromString("Impl.Internal"),
              """
              type Internal
                  Impl

                  describe self = "Internal"
              """);
      var sources = Set.of(main, apiElement, implInternal);

      ProjectUtils.createProject("Logical_Export", sources, projDir);

      return ContextUtils.newBuilder()
          .assertGC(false)
          .withProjectRoot(projDir)
          .withModifiedContext(
              (b) ->
                  b.option(RuntimeOptions.CHECK_CWD, "false")
                      .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
                      .option(RuntimeOptions.STRICT_ERRORS, "false"))
          .build();
    } catch (IOException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @Test
  public void resolveApiFromMain() {

    var mainCode =
        """
        from local.Logical_Export import Element

        main =
            element = Element.Element.create
            element.describe
        """;

    var name = QualifiedName.fromString("local.My_package").createChild("OK");
    var mainIr = compileIr(name, mainCode).getIr();

    assertEquals("One import", 1, mainIr.imports().size());
    var in = (Import.Module) mainIr.imports().head();

    assertTrue(
        "Right name: " + in.name().name(), in.name().name().contains("local.Logical_Export.Main"));
    var names = in.onlyNames().get().map(x -> x.name());
    assertEquals(1, names.size());
    assertEquals("Element", names.apply(0));

    var errors = mainIr.preorder().filter(x -> x instanceof Type.Error);
    assertEquals(0, errors.size());
  }

  @Test
  public void notExposeImplFromMain() {
    var mainCode =
        """
        from local.Logical_Export import Impl

        main = Impl
        """;
    var name = QualifiedName.fromString("local.My_package").createChild("Bad");
    var mainIr = compileIr(name, mainCode).getIr();

    assertEquals(1, mainIr.imports().size());
    var head = mainIr.imports().head();
    if (head instanceof ImportExport impExp) {
      if (impExp.reason() instanceof ImportExport.SymbolDoesNotExist reason) {
        assertEquals("Impl", reason.symbolName());
      } else {
        fail("Expecting doesn't exist: " + impExp.reason());
      }
    } else {
      fail("Expecting ImportExport: " + head);
    }
  }

  private static org.enso.interpreter.runtime.Module compileIr(
      QualifiedName moduleName, String code) {
    var langCtx = ctx.ensoContext();
    var pkg = langCtx.getPackageRepository().getMainProjectPackage().get();
    var module = new org.enso.interpreter.runtime.Module(moduleName, pkg, code);
    langCtx.getPackageRepository().registerModuleCreatedInRuntime(module.asCompilerModule());
    langCtx.getCompiler().run(module.asCompilerModule());
    return module;
  }
}
