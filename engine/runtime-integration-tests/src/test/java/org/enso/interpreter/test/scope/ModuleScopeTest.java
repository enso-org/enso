package org.enso.interpreter.test.scope;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.util.Set;
import org.enso.common.LanguageInfo;
import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.Module;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.Source;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ModuleScopeTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void extensionMethodIsRegisteredInModuleScope() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
        type My_Type
            Value x
        My_Type.extension_method self = self.x
        """,
                "test.enso")
            .build();
    try (var ctx = ContextUtils.createDefaultContext()) {
      var mainMod = ctx.eval(mainSrc);
      var mainRuntimeMod = (Module) ContextUtils.unwrapValue(ctx, mainMod);
      var myType = mainRuntimeMod.getScope().getType("My_Type", true);
      assertThat(myType, is(notNullValue()));
      var extensionMethod = mainRuntimeMod.getScope().getMethodForType(myType, "extension_method");
      assertThat(extensionMethod, is(notNullValue()));
    }
  }

  @Test
  public void staticMethodIsInResolvedExports() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_module"),
            """
        type My_Type
        My_Type.extension_method self = 42
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.A_Module.My_Type
        export project.A_Module.extension_method
        main = 42
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);
  }

  @Test
  public void staticMethodIsRegisteredInModuleScope() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
        type My_Type
            static_method _ = 42
        """,
                "test.enso")
            .build();
    try (var ctx = ContextUtils.createDefaultContext()) {
      var mainMod = ctx.eval(mainSrc);
      var mainRuntimeMod = (Module) ContextUtils.unwrapValue(ctx, mainMod);
      var myType = mainRuntimeMod.getScope().getType("My_Type", true);
      assertThat(myType, is(notNullValue()));
      var staticMethod = mainRuntimeMod.getScope().getMethodForType(myType, "static_method");
      assertThat(staticMethod, is(notNullValue()));
    }
  }

  @Test
  public void moduleMethodIsRegisteredInModuleScope() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID, """
        module_method _ = 42
        """, "test.enso")
            .build();
    try (var ctx = ContextUtils.createDefaultContext()) {
      // ModuleScope is populated in IrToTruffle - at runtime. So we have to evaluate
      // the main module before we inspect the ModuleScope.
      var mainMod = ctx.eval(mainSrc);
      var mainRuntimeMod = (Module) ContextUtils.unwrapValue(ctx, mainMod);
      var assocType = mainRuntimeMod.getScope().getAssociatedType();
      assertThat(assocType, is(notNullValue()));
      var moduleMethod = mainRuntimeMod.getScope().getMethodForType(assocType, "module_method");
      assertThat(moduleMethod, is(notNullValue()));
    }
  }

  @Test
  public void importedStaticMethodIsRegisteredInModuleScope() throws IOException {
    var mod =
        new SourceModule(
            QualifiedName.fromString("Mod"),
            """
        type My_Type
            static_method _ = 1
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.Mod import My_Type
        main = 2
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mod, mainMod), projDir);
    var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      var mainRuntimeMod = polyCtx.evalModule(mainSrcPath.toFile());
      var mainMethod = mainRuntimeMod.getMethod(mainRuntimeMod.getAssociatedType(), "main").get();
      var mainRes = mainMethod.execute();
      assertThat(mainRes.asInt(), is(2));
      var ensoCtx = ContextUtils.leakContext(ctx);
      var runtimeAbstractMod =
          ensoCtx.getPackageRepository().getLoadedModule("local.Proj.Mod").get();
      var runtimeConcreteMod = Module.fromCompilerModule(runtimeAbstractMod);
      var myType = runtimeConcreteMod.getScope().getType("My_Type", true);
      var staticMethod = runtimeConcreteMod.getScope().getMethodForType(myType, "static_method");
      assertThat(staticMethod, is(notNullValue()));
    }
  }
}
