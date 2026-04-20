package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.compiler.data.BindingsMap;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.PolyglotException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import scala.jdk.javaapi.CollectionConverters;

public class ExportedSymbolsTest {
  private Path projDir;

  @Before
  public void setup() throws IOException {
    this.projDir = Files.createTempDirectory("exported-symbols-test");
  }

  @After
  public void tearDown() throws IOException {
    ProjectUtils.deleteRecursively(projDir);
  }

  @Test
  public void exportedSymbolsFromSingleModule() throws IOException {
    var mainSrcMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            type A_Type
            """);
    ProjectUtils.createProject("Proj", Set.of(mainSrcMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var mainExportedSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSymbols.size(), is(1));
      assertThat(mainExportedSymbols.containsKey("A_Type"), is(true));
      assertThat(
          mainExportedSymbols.get("A_Type").get(0), instanceOf(BindingsMap.ResolvedType.class));
    }
  }

  @Test
  public void transitivelyExportedSymbols() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
            type A_Type
            """);
    var mainSrcMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            export project.A_Module.A_Type
            type B_Type
            """);
    ProjectUtils.createProject("Proj", Set.of(aMod, mainSrcMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var mainExportedSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSymbols.size(), is(2));
      assertThat(mainExportedSymbols.keySet(), containsInAnyOrder("A_Type", "B_Type"));
    }
  }

  @Test
  public void exportSymbolFromDifferentModule() throws IOException {
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            export project.B_Module.B_Type
            type A_Type
            """);
    var bMod =
        new SourceModule(
            QualifiedName.fromString("B_Module"),
            """
            type B_Type
            """);
    ProjectUtils.createProject("Proj", Set.of(mainMod, bMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var mainExportedSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSymbols.size(), is(2));
      assertThat(mainExportedSymbols.keySet(), containsInAnyOrder("A_Type", "B_Type"));
    }
  }

  @Test
  public void exportRenamedSymbol() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
            type A_Type
            """);
    var mainSrcMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            export project.A_Module.A_Type as Foo
            """);
    ProjectUtils.createProject("Proj", Set.of(aMod, mainSrcMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var mainExportedSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSymbols.size(), is(1));
      assertThat(mainExportedSymbols.keySet(), containsInAnyOrder("Foo"));
    }
  }

  @Test
  public void exportedSymbolsFromSubModule() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("Synthetic_Module.A_Module"),
            """
            type A_Module
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            import project.Synthetic_Module
            """);
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var syntheticModExpSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Synthetic_Module");
      assertThat(
          "Just a A_Module submodule should be exported", syntheticModExpSymbols.size(), is(1));
      assertThat(
          "Just a A_Module submodule should be exported",
          syntheticModExpSymbols,
          hasKey("A_Module"));
    }
  }

  @Test
  public void exportTypeFromModuleWithSameName() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
            type A_Module
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            export project.A_Module.A_Module
            """);
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var mainExportedSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSymbols.size(), is(1));
      assertThat(mainExportedSymbols.keySet(), containsInAnyOrder("A_Module"));
      assertThat(mainExportedSymbols.get("A_Module").size(), is(1));
      assertThat(
          mainExportedSymbols.get("A_Module").get(0),
          is(instanceOf(BindingsMap.ResolvedType.class)));
    }
  }

  @Test
  public void exportModuleWithTypeWithSameName() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
            type A_Module
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            export project.A_Module
            """);
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var mainExportedSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSymbols.size(), is(1));
      assertThat(mainExportedSymbols.keySet(), containsInAnyOrder("A_Module"));
      assertThat(mainExportedSymbols.get("A_Module").size(), is(1));
      assertThat(
          mainExportedSymbols.get("A_Module").get(0),
          is(instanceOf(BindingsMap.ResolvedModule.class)));
    }
  }

  @Test
  public void exportSyntheticModule() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("Synthetic_Module.A_Module"),
            """
            type A_Type
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            export project.Synthetic_Module
            """);
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var mainExportedSymbols = getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSymbols.size(), is(1));
      assertThat(mainExportedSymbols.keySet(), containsInAnyOrder("Synthetic_Module"));
      assertThat(mainExportedSymbols.get("Synthetic_Module").size(), is(1));
      assertThat(
          mainExportedSymbols.get("Synthetic_Module").get(0),
          is(instanceOf(BindingsMap.ResolvedModule.class)));
    }
  }

  @Test
  public void exportExtensionMethodViaAll() throws IOException {
    var rawMod =
        new SourceModule(
            QualifiedName.fromString("Raw_Module"),
            """
            type Raw_Type
            """);
    var extMod =
        new SourceModule(
            QualifiedName.fromString("Ext_Module"),
            """
            import project.Raw_Module.Raw_Type

            Raw_Type.enhanced_method = 42
            """);
    ProjectUtils.createProject("Enhancing", Set.of(rawMod, extMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);

      var mainValue =
          ctx.evalModule(
              """
              import local.Enhancing.Raw_Module.Raw_Type
              from local.Enhancing.Ext_Module import all

              main = Raw_Type.enhanced_method
              """);
      assertEquals(42, mainValue.asInt());
    }
  }

  @Test
  public void exportExtensionMethodByName() throws IOException {
    var rawMod =
        new SourceModule(
            QualifiedName.fromString("Raw_Module"),
            """
            type Raw_Type
            """);
    var extMod =
        new SourceModule(
            QualifiedName.fromString("Ext_Module"),
            """
            import project.Raw_Module.Raw_Type

            Raw_Type.enhanced_method = 42
            """);
    ProjectUtils.createProject("Enhancing", Set.of(rawMod, extMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);

      var mainValue =
          ctx.evalModule(
              """
              import local.Enhancing.Raw_Module.Raw_Type
              from local.Enhancing.Ext_Module import enhanced_method

              main = Raw_Type.enhanced_method
              """);
      assertEquals(42, mainValue.asInt());
    }
  }

  @Test
  public void exportExtensionMethodByAllWithHiding() throws IOException {
    var rawMod =
        new SourceModule(
            QualifiedName.fromString("Raw_Module"),
            """
            type Raw_Type
            """);
    var extMod =
        new SourceModule(
            QualifiedName.fromString("Ext_Module"),
            """
            import project.Raw_Module.Raw_Type

            Raw_Type.enhanced_method = 42
            Raw_Type.wrong_method = 33
            """);
    ProjectUtils.createProject("Enhancing", Set.of(rawMod, extMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);

      var mainValue =
          ctx.evalModule(
              """
              import local.Enhancing.Raw_Module.Raw_Type
              from local.Enhancing.Ext_Module import all hiding wrong_method

              main = Raw_Type.enhanced_method
              """);
      assertEquals(42, mainValue.asInt());
    }
  }

  @Test
  public void exportExtensionWrongMethod() throws IOException {
    var rawMod =
        new SourceModule(
            QualifiedName.fromString("Raw_Module"),
            """
            type Raw_Type
            """);
    var extMod =
        new SourceModule(
            QualifiedName.fromString("Ext_Module"),
            """
            import project.Raw_Module.Raw_Type

            Raw_Type.enhanced_method = 42
            Raw_Type.wrong_method = 33
            """);
    ProjectUtils.createProject("Enhancing", Set.of(rawMod, extMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);

      try {

        var mainValue =
            ctx.evalModule(
                """
                import local.Enhancing.Raw_Module.Raw_Type
                from local.Enhancing.Ext_Module import wrong_method

                main = Raw_Type.enhanced_method
                """);
        fail("Expecting exception, not a value: " + mainValue);
      } catch (PolyglotException ex) {
        assertEquals(
            "Method `enhanced_method` of type Raw_Type could not be found.", ex.getMessage());
      }
    }
  }

  @Test
  public void exportExtensionHidingTheRightMethod() throws IOException {
    var rawMod =
        new SourceModule(
            QualifiedName.fromString("Raw_Module"),
            """
            type Raw_Type
            """);
    var extMod =
        new SourceModule(
            QualifiedName.fromString("Ext_Module"),
            """
            import project.Raw_Module.Raw_Type

            Raw_Type.enhanced_method = 42
            Raw_Type.wrong_method = 33
            """);
    ProjectUtils.createProject("Enhancing", Set.of(rawMod, extMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);

      try {

        var mainValue =
            ctx.evalModule(
                """
                import local.Enhancing.Raw_Module.Raw_Type
                from local.Enhancing.Ext_Module import all hiding enhanced_method

                main = Raw_Type.enhanced_method
                """);
        fail("Expecting exception, not a value: " + mainValue);
      } catch (PolyglotException ex) {
        assertEquals(
            "Method `enhanced_method` of type Raw_Type could not be found.", ex.getMessage());
      }
    }
  }

  @Test
  public void exportConversionWhileHidingSomething() throws IOException {
    var rawMod =
        new SourceModule(
            QualifiedName.fromString("Raw_Module"),
            """
            type Raw_Type
            """);
    var extMod =
        new SourceModule(
            QualifiedName.fromString("Ext_Module"),
            """
            from Standard.Base import Integer
            import project.Raw_Module.Raw_Type

            something = 33
            Integer.from (_:Raw_Type) = 42
            """);
    ProjectUtils.createProject("Enhancing", Set.of(rawMod, extMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);

      var mainValue =
          ctx.evalModule(
              """
              from Standard.Base import Integer
              import local.Enhancing.Raw_Module.Raw_Type
              from local.Enhancing.Ext_Module import all hiding something

              main =
                  fourtyTwo = Raw_Type:Integer
                  fourtyTwo
              """);
      assertEquals("Conversion from Raw_Type to Integer found", 42, mainValue.asInt());
    }
  }

  private static ContextUtils createCtx(Path projDir) {
    return ContextUtils.newBuilder().withProjectRoot(projDir).build();
  }

  private static void compile(ContextUtils ctx) {
    new PolyglotContext(ctx.context()).getTopScope().compile(true);
  }

  private static Map<String, List<BindingsMap.ResolvedName>> getExportedSymbolsFromModule(
      ContextUtils ctx, String modName) {
    var ensoCtx = ctx.ensoContext();
    var mod = ensoCtx.getPackageRepository().getLoadedModule(modName).get();
    return getExportedSymbols(mod);
  }

  private static Map<String, List<BindingsMap.ResolvedName>> getExportedSymbols(Module module) {
    var bindings = new HashMap<String, List<BindingsMap.ResolvedName>>();
    var bindingsScala = module.getBindingsMap().exportedSymbols();
    bindingsScala.foreach(
        entry -> {
          var symbol = entry._1;
          var resolvedNames = CollectionConverters.asJava(entry._2.toSeq());
          bindings.put(symbol, resolvedNames);
          return null;
        });
    return bindings;
  }
}
