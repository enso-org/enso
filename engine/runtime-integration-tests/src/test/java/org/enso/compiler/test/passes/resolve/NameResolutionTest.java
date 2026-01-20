package org.enso.compiler.test.passes.resolve;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Predicate;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.resolve.FullyQualifiedNames;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.Patterns;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.scala.wrapper.ScalaConversions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Tests name resolution inside the compiler done by various {@link IRProcessingPass compiler
 * passes}.
 */
public final class NameResolutionTest {

  @Rule public final TemporaryFolder TMP_DIR = new TemporaryFolder();

  @Test
  public void libraryNameIsResolved_InExpressionBlock() throws IOException {
    var myMod =
        srcModule(
            "My_Module",
            """
            type My_Type
            """);
    var mainMod =
        srcModule(
            "Main",
            """
            import project.My_Module.My_Type

            main =
                local.Proj.My_Module.My_Type
            """);
    withProject(
        "Proj",
        Set.of(myMod, mainMod),
        ctx -> {
          var modIr = getModuleIr(ctx, "local.Proj.Main");
          var location = getLastLocationOf(mainMod.code(), "Proj");
          var ir = findIrByLocation(modIr, location);
          assertHasFQNMetadata(
              ir,
              FullyQualifiedNames.ResolvedModule.class,
              meta -> {
                assertThat(meta.moduleRef().getName().toString(), is("local.Proj.Main"));
              });
          assertHasMetadata(
              ir,
              GlobalNames$.MODULE$,
              BindingsMap.Resolution.class,
              resolution -> {
                assertThat(resolution.target(), instanceOf(BindingsMap.ResolvedModule.class));
                assertThat(resolution.target().qualifiedName().item(), is("Main"));
              });
        });
  }

  @Test
  public void importedNameIsResolved_AsType() throws Exception {
    var myMod =
        srcModule(
            "My_Module",
            """
            type My_Type
            """);
    var mainMod =
        srcModule(
            "Main",
            """
            import project.My_Module.My_Type

            main =
                My_Type
            """);
    withProject(
        "Proj",
        Set.of(myMod, mainMod),
        ctx -> {
          var modIr = getModuleIr(ctx, "local.Proj.Main");
          var location = getLastLocationOf(mainMod.code(), "My_Type");
          var ir = findIrByLocation(modIr, location);
          assertHasMetadata(
              ir,
              GlobalNames$.MODULE$,
              BindingsMap.Resolution.class,
              resolution -> {
                assertThat(resolution.target(), instanceOf(BindingsMap.ResolvedType.class));
                assertThat(
                    resolution.target().qualifiedName().toString(),
                    is("local.Proj.My_Module.My_Type"));
              });
        });
  }

  /**
   * {@link TypeSignatures} and {@link org.enso.compiler.pass.resolve.TypeNames} passes resolves the
   * ascribed type.
   */
  @Test
  public void nameIsResolved_InAscribedType() throws Exception {
    var myMod =
        srcModule(
            "My_Module",
            """
            type My_Type
                Cons
            """);
    var mainMod =
        srcModule(
            "Main",
            """
            import project.My_Module.My_Type

            foo : local.Proj.My_Module.My_Type
            foo = My_Type.Cons
            """);
    withProject(
        "Proj",
        Set.of(myMod, mainMod),
        ctx -> {
          var modIr = getModuleIr(ctx, "local.Proj.Main");
          var fooMethod = modIr.bindings().head();
          var signatureMeta =
              MetadataInteropHelpers.getMetadataOrNull(
                  fooMethod, TypeSignatures$.MODULE$, TypeSignatures.Signature.class);
          assertThat(signatureMeta, is(notNullValue()));
          assertThat(signatureMeta.signature(), instanceOf(Name.Qualified.class));
          var res =
              MetadataInteropHelpers.getMetadataOrNull(
                  signatureMeta.signature(), TypeNames$.MODULE$, BindingsMap.Resolution.class);
          assertThat(res, is(notNullValue()));
          assertThat(res.target(), instanceOf(BindingsMap.ResolvedType.class));
          assertThat(res.target().qualifiedName().toString(), is("local.Proj.My_Module.My_Type"));
        });
  }

  /**
   * Inline type ascription resolution is handled by {@link
   * org.enso.compiler.pass.resolve.TypeNames}.
   *
   * @throws Exception
   */
  @Test
  public void nameIsResolved_InInlineAscription() throws Exception {
    var myMod =
        srcModule(
            "My_Module",
            """
            type My_Type
                Cons
                method self = 42
            """);
    var mainMod =
        srcModule(
            "Main",
            """
            import project.My_Module.My_Type

            foo (obj : local.Proj.My_Module.My_Type) =
                obj.method
            """);
    withProject(
        "Proj",
        Set.of(myMod, mainMod),
        ctx -> {
          var modIr = getModuleIr(ctx, "local.Proj.Main");
          var fooMethod = modIr.bindings().head();
          assertThat(fooMethod, is(notNullValue()));
          var defArg =
              findIR(
                  modIr,
                  DefinitionArgument.Specified.class,
                  arg -> {
                    if (arg.name() instanceof Name.Literal lit) {
                      return lit.name().equals("obj") && arg.ascribedType().isDefined();
                    }
                    return false;
                  });
          var ascribedTypeName = ((Name.Qualified) defArg.ascribedType().get());
          var res =
              MetadataInteropHelpers.getMetadataOrNull(
                  ascribedTypeName, TypeNames$.MODULE$, BindingsMap.Resolution.class);
          assertThat(res.target(), instanceOf(BindingsMap.ResolvedType.class));
          assertThat(res.target().qualifiedName().toString(), is("local.Proj.My_Module.My_Type"));
        });
  }

  @Test
  public void nameIsResolved_InInlineAscription_TwoProjects() throws Exception {
    var libDir = TMP_DIR.newFolder("Lib").toPath();
    var projDir = TMP_DIR.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            srcModule(
                "My_Module",
                """
                type My_Type
                    method self = 42
                """),
            srcModule(
                "Main",
                """
                export project.My_Module.My_Type
                """)),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import all

        foo (obj : local.Lib.My_Module.My_Type) = obj.method
        """,
        projDir);
    try (var ctx = createCtx(projDir)) {
      compileAllModules(ctx);
      var modIr = getModuleIr(ctx, "local.Proj.Main");
      var fooMethod = modIr.bindings().head();
      assertThat(fooMethod, is(notNullValue()));
      var defArg =
          findIR(
              modIr,
              DefinitionArgument.Specified.class,
              arg -> {
                if (arg.name() instanceof Name.Literal lit) {
                  return lit.name().equals("obj") && arg.ascribedType().isDefined();
                }
                return false;
              });
      var ascribedTypeName = ((Name.Qualified) defArg.ascribedType().get());
      var res =
          MetadataInteropHelpers.getMetadataOrNull(
              ascribedTypeName, TypeNames$.MODULE$, BindingsMap.Resolution.class);
      assertThat(res.target(), instanceOf(BindingsMap.ResolvedType.class));
      assertThat(res.target().qualifiedName().toString(), is("local.Lib.My_Module.My_Type"));
    }
  }

  @Test
  public void nameIsResolved_InExpression_TwoProjects_TypeInSubmodule() throws Exception {
    var libDir = TMP_DIR.newFolder("Lib").toPath();
    var projDir = TMP_DIR.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            srcModule(
                "Data.Numbers",
                """
                type Integer
                """),
            srcModule(
                "Main",
                """
                export project.Data.Numbers.Integer
                """)),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import all

        main =
            local.Lib.Data.Numbers.Integer
        """,
        projDir);
    try (var ctx = createCtx(projDir)) {
      compileAllModules(ctx);
      var modIr = getModuleIr(ctx, "local.Proj.Main");
      var internalLitName =
          findIR(modIr, Name.Literal.class, lit -> lit.name().contains("internal"));
      assertHasMetadata(
          internalLitName,
          GlobalNames$.MODULE$,
          BindingsMap.Resolution.class,
          res -> {
            assertThat(res.target(), instanceOf(BindingsMap.ResolvedModule.class));
            assertThat(res.target().qualifiedName().toString(), is("local.Lib.Main"));
          });
      assertHasFQNMetadata(
          internalLitName,
          FullyQualifiedNames.ResolvedModule.class,
          resMod -> {
            assertThat(resMod.moduleRef().getName().toString(), is("local.Lib.Main"));
          });
    }
  }

  @Test
  public void nameIsResolved_InInlineAscription_TwoProjects_TypeInSubmodule() throws Exception {
    var libDir = TMP_DIR.newFolder("Lib").toPath();
    var projDir = TMP_DIR.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            srcModule(
                "Data.Numbers",
                """
                type Integer
                """),
            srcModule(
                "Main",
                """
                export project.Data.Numbers.Integer
                """)),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import all

        foo (obj : local.Lib.Data.Numbers.Integer) = obj
        """,
        projDir);
    try (var ctx = createCtx(projDir)) {
      compileAllModules(ctx);
      var modIr = getModuleIr(ctx, "local.Proj.Main");
      var fooMethod = modIr.bindings().head();
      assertThat(fooMethod, is(notNullValue()));
      var defArg =
          findIR(
              modIr,
              DefinitionArgument.Specified.class,
              arg -> {
                if (arg.name() instanceof Name.Literal lit) {
                  return lit.name().equals("obj") && arg.ascribedType().isDefined();
                }
                return false;
              });
      var ascribedTypeName = ((Name.Qualified) defArg.ascribedType().get());
      var res =
          MetadataInteropHelpers.getMetadataOrNull(
              ascribedTypeName, TypeNames$.MODULE$, BindingsMap.Resolution.class);
      assertThat(res.target(), instanceOf(BindingsMap.ResolvedType.class));
      assertThat(res.target().qualifiedName().toString(), is("local.Lib.Data.Numbers.Integer"));
    }
  }

  /**
   * Fully qualified name used in {@link Pattern.Type} is resolved in {@link Patterns} compiler
   * pass. Moreover, this pass resolves it to {@link ResolvedType} - which means full resolution.
   */
  @Test
  public void libNameIsResolved_InCaseBranch() throws Exception {
    var myMod =
        srcModule(
            "My_Module",
            """
            type My_Type
            """);
    var mainMod =
        srcModule(
            "Main",
            """
            import project.My_Module.My_Type

            main =
                x = 42
                case x of
                    _ : local.Proj.My_Module.My_Type -> 21  # Case.Branch
                    _ -> 22
            """);
    withProject(
        "Proj",
        Set.of(myMod, mainMod),
        ctx -> {
          var modIr = getModuleIr(ctx, "local.Proj.Main");
          var caseBranch =
              findIR(
                  modIr,
                  Case.Branch.class,
                  branch -> {
                    if (branch.expression() instanceof Literal.Number num) {
                      return num.value().equals("21");
                    }
                    return false;
                  });
          assertThat(caseBranch, is(notNullValue()));
          var patternType = (Pattern.Type) caseBranch.pattern();
          var tpeName = patternType.tpe();
          assertHasMetadata(
              tpeName,
              Patterns$.MODULE$,
              BindingsMap.Resolution.class,
              resolution -> {
                assertThat(
                    "Resolution target: " + resolution.target() + " should be ResolvedType",
                    resolution.target() instanceof ResolvedType,
                    is(true));
                assertThat(
                    resolution.target().qualifiedName().toString(),
                    containsString("My_Module.My_Type"));
              });
        });
  }

  @Test
  public void libNameIsResolved_FromDifferentProject() throws IOException {
    var projDir = TMP_DIR.newFolder("Proj").toPath();
    var libDir = TMP_DIR.newFolder("Lib").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            srcModule(
                "My_Module",
                """
                type My_Type
                """),
            srcModule(
                "Main",
                """
                export project.My_Module.My_Type
                """)),
        libDir);
    var mainSrc =
        """
        from local.Lib import all

        main =
            local.Lib.My_Module.My_Type
        """;
    ProjectUtils.createProject("Proj", mainSrc, projDir);
    try (var ctx = createCtx(projDir)) {
      compileAllModules(ctx);
      var modIr = getModuleIr(ctx, "local.Proj.Main");
      var location = getLastLocationOf(mainSrc, "Lib");
      var ir = findIrByLocation(modIr, location);
      assertHasFQNMetadata(
          ir,
          FullyQualifiedNames.ResolvedModule.class,
          meta -> {
            assertThat(meta.moduleRef().getName().toString(), is("local.Lib.Main"));
          });
    }
  }

  private void withProject(
      String projName, Set<SourceModule> modules, Consumer<ContextUtils> callback)
      throws IOException {
    var projDir = TMP_DIR.newFolder(projName).toPath();
    ProjectUtils.createProject(projName, modules, projDir);
    try (var ctx = createCtx(projDir)) {
      compileAllModules(ctx);
      callback.accept(ctx);
    }
  }

  private static <T extends IR> T findIR(IR root, Class<T> expectedIRType, Predicate<T> predicate) {
    for (var ir : ScalaConversions.asJava(root.preorder())) {
      if (expectedIRType.isAssignableFrom(ir.getClass())) {
        T castedIr = expectedIRType.cast(ir);
        if (predicate.test(castedIr)) {
          return castedIr;
        }
      }
    }
    return null;
  }

  private <T> void assertHasFQNMetadata(IR ir, Class<T> metaTargetType, Consumer<T> callback) {
    var meta =
        MetadataInteropHelpers.getMetadataOrNull(
            ir, FullyQualifiedNames$.MODULE$, FullyQualifiedNames.FQNResolution.class);
    assertThat("Must have FullyQualifiedNames metadata", meta, is(notNullValue()));
    var isSameTargetClass = meta.target().getClass().isAssignableFrom(metaTargetType);
    assertThat(
        "Unexpected target meta type: " + meta.target().getClass().getName(),
        isSameTargetClass,
        is(true));
    var target = metaTargetType.cast(meta.target());
    callback.accept(target);
  }

  private <T> void assertHasMetadata(
      IR ir, IRProcessingPass pass, Class<T> expectedMetaType, Consumer<T> callback) {
    var meta = MetadataInteropHelpers.getMetadataOrNull(ir, pass, expectedMetaType);
    assertThat(
        "Should have metadata from pass " + pass + " with type " + expectedMetaType.getName(),
        meta,
        is(notNullValue()));
    callback.accept(meta);
  }

  private ContextUtils createCtx(Path projectRoot) {
    return ContextUtils.newBuilder().withProjectRoot(projectRoot).build();
  }

  private static SourceModule srcModule(String moduleName, String src) {
    return new SourceModule(QualifiedName.fromString(moduleName), src);
  }

  private void compileAllModules(ContextUtils ctx) {
    var polyCtx = new PolyglotContext(ctx.context());
    polyCtx.getTopScope().compile(true);
  }

  private Module getModuleIr(ContextUtils ctx, String moduleName) {
    var ensoCtx = ctx.ensoContext();
    var mod = ensoCtx.findModule(moduleName);
    assertThat(mod.isPresent(), is(true));
    var modIr = mod.get().getIr();
    assertThat(modIr, is(notNullValue()));
    return modIr;
  }

  private static Location getLastLocationOf(String moduleSrc, String expr) {
    assertThat("moduleSrc must contain expr", moduleSrc, containsString(expr));
    var start = moduleSrc.lastIndexOf(expr);
    assert start != -1;
    var end = start + expr.length();
    return new Location(start, end);
  }

  private static IR findIrByLocation(IR root, Location location) {
    var ret =
        root.preorder()
            .find(
                ir -> {
                  var irLoc = ir.identifiedLocation();
                  if (irLoc != null) {
                    if (irLoc.location().equals(location)) {
                      return true;
                    }
                  }
                  return false;
                });
    assertThat(ret.isDefined(), is(true));
    return ret.get();
  }
}
