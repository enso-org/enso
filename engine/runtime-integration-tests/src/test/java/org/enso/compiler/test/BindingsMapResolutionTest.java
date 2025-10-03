package org.enso.compiler.test;

import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolutionError;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import scala.jdk.CollectionConverters;
import scala.util.Either;

public class BindingsMapResolutionTest {

  @ClassRule public static final TemporaryFolder TMP_DIR = new TemporaryFolder();

  @Test
  public void resolveSingleName_FromSingleImport() throws IOException {
    var projDir = createProject("import local.Proj.My_Vector.My_Vector");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Vector");
          assertSingleResolvedType(bindingsMap, "local.Proj.My_Vector.My_Vector");
        });
  }

  @Test
  public void resolveSingleName_FromSingleImportWithFrom() throws IOException {
    var projDir = createProject("from local.Proj.My_Vector import My_Vector");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Vector");
          assertSingleResolvedType(bindingsMap, "local.Proj.My_Vector.My_Vector");
        });
  }

  @Test
  public void resolveQualifiedName_FromSingleImport_TwoProjects() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var libDir = tmpDir.toPath().resolve("Lib");
    var projDir = tmpDir.toPath().resolve("Proj");
    libDir.toFile().mkdir();
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Lib",
        Set.of(new SourceModule(QualifiedName.fromString("My_Vector"), "type My_Vector")),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"), "import local.Lib.My_Vector.My_Vector")),
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Vector");
          assertSingleResolvedType(bindingsMap, "local.Lib.My_Vector.My_Vector");
        });
  }

  @Test
  public void resolveQualifiedName_DefinedEntity() throws IOException {
    var projDir =
        createProject(
            """
            type My_Type
            """);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Type");
          assertSingleResolvedType(bindingsMap, "local.Proj.My_Type");
        });
  }

  @Test
  @Ignore
  public void resolveConstructor_DefinedEntity() throws IOException {
    var projDir =
        createProject(
            """
            type My_Type
                Cons
            """);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          for (var nameToResolve : List.of("My_Type.Cons", "local.Proj.My_Type.Cons")) {
            assertResolvedNames(
                bindingsMap,
                nameToResolve,
                resolvedNames -> {
                  assertThat(resolvedNames.size(), is(1));
                  assertThat(resolvedNames.head() instanceof ResolvedConstructor, is(true));
                });
          }
        });
  }

  @Test
  public void resolveModule_InTheSameModule() throws IOException {
    var projDir = createProject("type My_Type");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertResolvedNames(
              bindingsMap,
              "local.Proj.Main",
              resolvedNames -> {
                assertThat("single module resolved", resolvedNames.size(), is(1));
                assertThat(
                    "is ResolvedModule", resolvedNames.head() instanceof ResolvedModule, is(true));
              });
        });
  }

  @Test
  public void resolveConstructor_ImportWithFrom() throws IOException {
    var projDir = createProject("from local.Proj.My_Vector.My_Vector import Cons");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertResolvedNames(
              bindingsMap,
              "Cons",
              resolvedNames -> {
                assertThat("single Cons resolved", resolvedNames.size(), is(1));
                assertThat(
                    "is ResolvedConstructor",
                    resolvedNames.head() instanceof ResolvedConstructor,
                    is(true));
              });
        });
  }

  @Test
  public void resolveConstructor_ImportFQN() throws IOException {
    var projDir = createProject("import local.Proj.My_Vector.My_Vector.Cons");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertResolvedNames(
              bindingsMap,
              "Cons",
              resolvedNames -> {
                assertThat("single Cons resolved", resolvedNames.size(), is(1));
                assertThat(
                    "is ResolvedConstructor",
                    resolvedNames.head() instanceof ResolvedConstructor,
                    is(true));
              });
        });
  }

  @Test
  public void resolveConstructor_ViaFQN_ImportFQN() throws IOException {
    var projDir = createProject("import local.Proj.My_Vector.My_Vector.Cons");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertResolvedNames(
              bindingsMap,
              "local.Proj.My_Vector.My_Vector.Cons",
              resolvedNames -> {
                assertThat("single Cons resolved", resolvedNames.size(), is(1));
                assertThat(
                    "is ResolvedConstructor",
                    resolvedNames.head() instanceof ResolvedConstructor,
                    is(true));
              });
        });
  }

  @Test
  public void resolveExportedType() throws IOException {
    var projDir = TMP_DIR.newFolder().toPath();
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("My_Module"),
                """
                type My_Type
                    Cons
                """),
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.My_Module.My_Type
                export project.My_Module.My_Type
                """)),
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Type");
          assertSingleResolvedType(bindingsMap, "local.Proj.My_Module.My_Type");
        });
  }

  @Test
  public void resolveReexportedType() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var libDir = tmpDir.toPath().resolve("Lib");
    var projDir = tmpDir.toPath().resolve("Proj");
    libDir.toFile().mkdir();
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.My_Module.My_Type
                export project.My_Module.My_Type
                """),
            new SourceModule(
                QualifiedName.fromString("My_Module"),
                """
                type My_Type
                    Cons
                """)),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import all
        """,
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Type");
          assertSingleResolvedType(bindingsMap, "local.Lib.My_Module.My_Type");
        });
  }

  @Test
  public void notResolveModule_IfItItWasNotDirectlyImported() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var projDir = tmpDir.toPath().resolve("Proj");
    projDir.toFile().mkdirs();
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.My_Module.My_Type
                """),
            new SourceModule(
                QualifiedName.fromString("My_Module"),
                """
                type My_Type
                """)),
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          var res = bindingsMap.resolveName("My_Module");
          assertThat("My_Module is not resolved", res.isLeft(), is(true));
        });
  }

  @Test
  public void resolveReexportedType_MoreDefinedEntitiesInLib() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var libDir = tmpDir.toPath().resolve("Lib");
    var projDir = tmpDir.toPath().resolve("Proj");
    libDir.toFile().mkdir();
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.My_Other_Module.My_Other_Type
                import project.My_Module.My_Type
                export project.My_Other_Module.My_Other_Type
                export project.My_Module.My_Type
                """),
            new SourceModule(
                QualifiedName.fromString("My_Module"),
                """
                type My_Type
                """),
            new SourceModule(
                QualifiedName.fromString("My_Other_Module"),
                """
                type My_Other_Type
                """)),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import all
        """,
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Type");
          assertSingleResolvedType(bindingsMap, "local.Lib.My_Module.My_Type");
          assertSingleResolvedType(bindingsMap, "My_Other_Type");
          assertSingleResolvedType(bindingsMap, "local.Lib.My_Other_Module.My_Other_Type");
        });
  }

  @Test
  @Ignore
  public void resolveReexportedType_FromDifferentProject() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var libDir = tmpDir.toPath().resolve("Lib");
    var otherLibDir = tmpDir.toPath().resolve("Other_Lib");
    var projDir = tmpDir.toPath().resolve("Proj");
    libDir.toFile().mkdir();
    projDir.toFile().mkdir();
    otherLibDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Other_Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Other_Module"),
                """
                type Other_Type
                """),
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.Other_Module.Other_Type
                export project.Other_Module.Other_Type
                """)),
        otherLibDir);
    ProjectUtils.createProject(
        "Lib",
        """
        import local.Other_Lib.Other_Module.Other_Type
        export local.Other_Lib.Other_Module.Other_Type
        type My_Type
        """,
        libDir);
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import all
        """,
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Type");
          assertSingleResolvedType(bindingsMap, "local.Lib.My_Type");
          assertSingleResolvedType(bindingsMap, "Other_Type");
          assertSingleResolvedType(bindingsMap, "local.Other_Lib.Other_Module.Other_Type");
        });
  }

  @Test
  public void resolveReexportedType_ThreeProjects() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var libDir = tmpDir.toPath().resolve("Lib");
    var otherLibDir = tmpDir.toPath().resolve("Other_Lib");
    var projDir = tmpDir.toPath().resolve("Proj");
    libDir.toFile().mkdir();
    projDir.toFile().mkdir();
    otherLibDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Other_Lib",
        """
        type Other_Type
        """,
        otherLibDir);
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                import project.My_Module.My_Type
                export project.My_Module.My_Type
                """),
            new SourceModule(
                QualifiedName.fromString("My_Module"),
                """
                type My_Type
                    Cons
                """)),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Other_Lib import all
        from local.Lib import all
        """,
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertResolvedNames(
              bindingsMap,
              "My_Type",
              resolvedNames -> {
                assertThat(resolvedNames.size(), is(1));
                assertThat(resolvedNames.head() instanceof ResolvedType, is(true));
              });
          assertResolvedNames(
              bindingsMap,
              "local.Lib.My_Module.My_Type",
              resolvedNames -> {
                assertThat(resolvedNames.size(), is(1));
                assertThat(resolvedNames.head() instanceof ResolvedType, is(true));
              });
        });
  }

  @Test
  public void resolveModule_InImportCycle_1() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var projDir = tmpDir.toPath().resolve("Proj");
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Data.A"),
                """
                import project.Data.B
                """),
            new SourceModule(
                QualifiedName.fromString("Data.B"),
                """
                import project.Data.A
                """),
            new SourceModule(QualifiedName.fromString("Main"), "")),
        projDir);
    testBindingsMap(
        projDir,
        "local.Proj.Data.A",
        bindingsMap -> {
          var nameToResolve = asScala(List.of("local", "Proj", "Data", "A"));
          var res = bindingsMap.resolveQualifiedName(nameToResolve);
          assertThat("Resolution method finishes", res, is(notNullValue()));
        });
  }

  @Test
  public void resolveModule_InImportCycle_2() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var projDir = tmpDir.toPath().resolve("Proj");
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("A"),
                """
                import project.B
                """),
            new SourceModule(
                QualifiedName.fromString("B"),
                """
                import project.A
                """),
            new SourceModule(QualifiedName.fromString("Main"), "")),
        projDir);
    testBindingsMap(
        projDir,
        "local.Proj.A",
        bindingsMap -> {
          var nameToResolve = asScala(List.of("local", "Proj", "A"));
          var res = bindingsMap.resolveQualifiedName(nameToResolve);
          assertThat("Resolution method finishes", res, is(notNullValue()));
        });
  }

  @Test
  public void resolveModule_InImportCycle_3() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var projDir = tmpDir.toPath().resolve("Proj");
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("A"),
                """
                import project.B
                import project.C
                """),
            new SourceModule(
                QualifiedName.fromString("B"),
                """
                import project.A
                """),
            new SourceModule(
                QualifiedName.fromString("C"),
                """
                type C_Type
                """),
            new SourceModule(QualifiedName.fromString("Main"), "")),
        projDir);
    testBindingsMap(
        projDir,
        "local.Proj.A",
        bindingsMap -> {
          var nameToResolve = asScala(List.of("local", "Proj", "C", "C_Type"));
          var res = bindingsMap.resolveQualifiedName(nameToResolve);
          assertThat("Resolution succeeds", res.toOption().isDefined(), is(true));
          var resolvedNames = res.toOption().get();
          assertThat("Single resolution found", resolvedNames.size(), is(1));
          assertThat("Is ResolvedType", resolvedNames.head() instanceof ResolvedType, is(true));
        });
  }

  @Test
  public void noStackOverflow_WhenResolving_StandardBoolean() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var projDir = tmpDir.toPath().resolve("Proj");
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Proj",
        """
        import Standard.Base.Any
        """,
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          var nameToResolve = asScala(List.of("Standard", "Base", "Data", "Boolean", "boolean"));
          var res = bindingsMap.resolveQualifiedName(nameToResolve);
          assertThat("Resolution method finishes", res, is(notNullValue()));
        });
  }

  @Test
  public void resolveMainModule_FromDifferentProject() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var projDir = tmpDir.toPath().resolve("Proj");
    var libDir = tmpDir.toPath().resolve("Lib");
    projDir.toFile().mkdirs();
    libDir.toFile().mkdirs();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                # Empty on purpose
                """)),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                from local.Lib import all
                """)),
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          var nameToResolve = toScalaList(List.of("local", "Lib", "Main"));
          var res = bindingsMap.resolveQualifiedName(nameToResolve);
          assertThat("Is resolved", res.isRight(), is(true));
        });
  }

  private Path createProject(String mainModuleSrc) throws IOException {
    var projDir = TMP_DIR.newFolder().toPath();
    var modules =
        Set.of(
            new SourceModule(QualifiedName.fromString("Main"), mainModuleSrc),
            new SourceModule(
                QualifiedName.fromString("My_Vector"),
                """
                type My_Vector
                    Cons data
                """));
    ProjectUtils.createProject("Proj", modules, projDir);
    return projDir;
  }

  private static void assertSingleResolvedType(BindingsMap bindingsMap, String typeName) {
    Either<ResolutionError, scala.collection.immutable.List<ResolvedName>> resolution;
    if (typeName.contains(".")) {
      var fqn = Arrays.stream(typeName.split("\\.")).toList();
      resolution = bindingsMap.resolveQualifiedName(toScalaList(fqn));
    } else {
      resolution = bindingsMap.resolveName(typeName);
    }
    assertThat("Type '" + typeName + "' is resolved", resolution.isRight(), is(true));
    var resolvedNames = resolution.toOption().get();
    assertThat("single resolution found", resolvedNames.size(), is(1));
    assertThat("is ResolvedType", resolvedNames.head() instanceof ResolvedType, is(true));
  }

  private static void assertResolvedNames(
      BindingsMap bindingsMap,
      String name,
      Consumer<scala.collection.immutable.List<ResolvedName>> callback) {
    Either<ResolutionError, scala.collection.immutable.List<ResolvedName>> resolution;
    if (name.contains(".")) {
      var fqn = Arrays.stream(name.split("\\.")).toList();
      resolution = bindingsMap.resolveQualifiedName(toScalaList(fqn));
    } else {
      resolution = bindingsMap.resolveName(name);
    }
    assertThat("Name '" + name + "' is resolved", resolution.isRight(), is(true));
    var resolvedNames = resolution.toOption().get();
    callback.accept(resolvedNames);
  }

  private static void testBindingsMap(Path projDir, Consumer<BindingsMap> callback) {
    testBindingsMap(projDir, "local.Proj.Main", callback);
  }

  private static void testBindingsMap(
      Path projDir, String moduleName, Consumer<BindingsMap> callback) {
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var bm = getBindingsMap(ctx, moduleName);
      assertThat(bm, is(notNullValue()));
      callback.accept(bm);
    }
  }

  private static ContextUtils createCtx(Path projDir) {
    return ContextUtils.newBuilder()
        .withModifiedContext(
            bldr -> bldr.option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString()))
        .build();
  }

  private static void compile(ContextUtils ctx) {
    new PolyglotContext(ctx.context()).getTopScope().compile(true);
  }

  private static BindingsMap getBindingsMap(ContextUtils ctx, String moduleName) {
    var ensoCtx = ctx.ensoContext();
    var mod = ensoCtx.getPackageRepository().getLoadedModule(moduleName).get();
    assert mod != null;
    return mod.getBindingsMap();
  }

  private static <T> scala.collection.immutable.List<T> toScalaList(java.util.List<T> list) {
    return CollectionConverters.ListHasAsScala(list).asScala().toList();
  }
}
