package org.enso.interpreter.test.scope;

import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import org.enso.common.LanguageInfo;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ModuleScopeTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

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
  public void moduleMethodIsRegisteredInModuleScope() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                module_method _ = 42
                """,
                "test.enso")
            .build();
    // ModuleScope is populated in IrToTruffle - at runtime. So we have to evaluate
    // the main module before we inspect the ModuleScope.
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var assocType = mainRuntimeMod.getScope().getAssociatedType();
    assertThat(assocType, is(notNullValue()));
    var moduleMethod = mainRuntimeMod.getScope().getMethodForType(assocType, "module_method");
    assertOnlyFirstArgumentIsSelf(moduleMethod);
  }

  @Test
  public void scopeGetType_DoesNotReturn_EigenType() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type My_Type
                    Value x
                    method self = self.x
                """,
                "test.enso")
            .build();
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var scope = mainRuntimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    assertThat(myType.isEigenType(), is(false));
  }

  @Test
  public void instanceMethodIsRegistered_OnType() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type My_Type
                    Value x
                    method self = self.x
                """,
                "test.enso")
            .build();
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var scope = mainRuntimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    assertThat(myType.isEigenType(), is(false));
    var method = scope.getMethodForType(myType, "method");
    assertThat("My_Type.method is registered", method, is(notNullValue()));
    assertOnlyFirstArgumentIsSelf(method);
    var myEigenType = myType.getEigentype();
    var eigenMethod = scope.getMethodForType(myEigenType, "method");
    assertThat("My_Type.type.method is not registered", eigenMethod, is(nullValue()));
  }

  @Test
  public void staticMethodIsRegistered_OnEigenType() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type My_Type
                    Value x
                    static_method = 42
                """,
                "test.enso")
            .build();
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var scope = mainRuntimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    assertThat(myType.isEigenType(), is(false));
    var myEigenType = myType.getEigentype();
    var staticMethod = scope.getMethodForType(myEigenType, "static_method");
    assertOnlyFirstArgumentIsSelf(staticMethod);
    assertThat("My_Type.type.static_method is registered", staticMethod, is(notNullValue()));
    assertThat(
        "My_Type.static_method is not registered",
        scope.getMethodForType(myType, "static_method"),
        is(nullValue()));
  }

  @Test
  public void instanceExtensionMethodIsRegistered_OnType() throws IOException {
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
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var scope = mainRuntimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    var extensionMethod = scope.getMethodForType(myType, "extension_method");
    assertThat("My_Type.extension_method is registered", extensionMethod, is(notNullValue()));
    assertOnlyFirstArgumentIsSelf(extensionMethod);
    var myEigenType = myType.getEigentype();
    assertThat(
        "My_Type.type.extension_method is not registered",
        scope.getMethodForType(myEigenType, "extension_method"),
        is(nullValue()));
  }

  @Test
  public void constructorIsRegistered_OnEigenType() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type My_Type
                    Value x
                    method self = self.x
                """,
                "test.enso")
            .build();
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var scope = mainRuntimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    assertThat(myType.isEigenType(), is(false));
    var myEigenType = myType.getEigentype();
    var ctor = scope.getMethodForType(myEigenType, "Value");
    assertThat("My_Type.type.Value is registered", ctor, is(notNullValue()));
    assertOnlyFirstArgumentIsSelf(ctor);
    assertThat(
        "My_Type.Value is not registered",
        scope.getMethodForType(myType, "Value"),
        is(nullValue()));
  }

  @Test
  public void fieldGetterIsRegistered_OnType() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type My_Type
                    Value x
                """,
                "test.enso")
            .build();
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var scope = mainRuntimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    var fieldGetter = scope.getMethodForType(myType, "x");
    assertThat("My_Type.x is registered", fieldGetter, is(notNullValue()));
    assertOnlyFirstArgumentIsSelf(fieldGetter);
    var myEigenType = myType.getEigentype();
    assertThat(
        "My_Type.type.x is not registered",
        scope.getMethodForType(myEigenType, "x"),
        is(nullValue()));
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
    try (var ctx = ContextUtils.newBuilder().withProjectRoot(projDir).build()) {
      var polyCtx = new PolyglotContext(ctx.context());
      var mainRuntimeMod = polyCtx.evalModule(mainSrcPath.toFile());
      var mainMethod = mainRuntimeMod.getMethod(mainRuntimeMod.getAssociatedType(), "main").get();
      var mainRes = mainMethod.execute();
      assertThat(mainRes.asInt(), is(2));
      var ensoCtx = ctx.ensoContext();
      var runtimeAbstractMod =
          ensoCtx.getPackageRepository().getLoadedModule("local.Proj.Mod").get();
      var runtimeConcreteMod = Module.fromCompilerModule(runtimeAbstractMod);
      var myType = runtimeConcreteMod.getScope().getType("My_Type", true);
      var staticMethod = runtimeConcreteMod.getScope().getMethodForType(myType, "static_method");
      assertThat(staticMethod, is(notNullValue()));
    }
  }

  @Test
  public void instanceMethod_OnSingletonType_IsRegisteredToEigenType() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type Singleton_Type
                    method self = 42
                """,
                "test.enso")
            .build();
    var mainMod = ctxRule.eval(mainSrc);
    var mainRuntimeMod = (Module) ctxRule.unwrapValue(mainMod);
    var scope = mainRuntimeMod.getScope();
    var singletonType = scope.getType("Singleton_Type", true);
    assertThat(singletonType.isEigenType(), is(true));
    var method = scope.getMethodForType(singletonType, "method");
    assertThat(method, is(notNullValue()));
  }

  /**
   * See <a href="https://github.com/enso-org/enso/issues/11686">#11686</a>
   *
   * @throws IOException
   */
  @Test
  public void methodsAreNotDuplicated() throws IOException {
    var src =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type My_Type
                    Cons x
                    instance_method self = self.x
                    static_method = 42

                My_Type.extension_instance_method self = self.x
                My_Type.extension_static_method = 84
                """,
                "test.enso")
            .build();
    var mod = ctxRule.eval(src);
    var runtimeMod = (Module) ctxRule.unwrapValue(mod);
    var scope = runtimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    var myEigenType = myType.getEigentype();
    var moduleAssocType = scope.getAssociatedType();
    // For all methods we check that they are present on just one of the
    // types: myType, myEigenType and moduleAssocType.
    var methodNamesToCheck =
        List.of(
            "instance_method",
            "static_method",
            "extension_instance_method",
            "extension_static_method");
    for (var methodName : methodNamesToCheck) {
      var onType = scope.getMethodForType(myType, methodName);
      var onEigenType = scope.getMethodForType(myEigenType, methodName);
      var onAssocType = scope.getMethodForType(moduleAssocType, methodName);
      var foundCount = 0;
      if (onType != null) {
        foundCount++;
      }
      if (onEigenType != null) {
        foundCount++;
      }
      if (onAssocType != null) {
        foundCount++;
      }
      assertThat(
          "Method '"
              + methodName
              + "' is present on just one of the types. "
              + "Found on My_Type: "
              + (onType != null)
              + ", "
              + "found on My_Type's eigen type: "
              + (onEigenType != null)
              + ", "
              + "found on module associated type: "
              + (onAssocType != null),
          foundCount,
          is(1));
    }
  }

  /**
   * For all methods it should hold that there is just a single `self` argument defined on the first
   * position.
   */
  @Test
  public void allMethodsHaveOnlyOneSelfArgument() throws IOException {
    var src =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                type My_Type
                    Cons x
                    instance_method self = self.x
                    static_method = 42

                My_Type.extension_instance_method self = self.x
                My_Type.extension_static_method = 84
                """,
                "test.enso")
            .build();
    var mod = ctxRule.eval(src);
    var runtimeMod = (Module) ctxRule.unwrapValue(mod);
    var scope = runtimeMod.getScope();
    var myType = scope.getType("My_Type", true);
    var myEigenType = myType.getEigentype();
    var moduleAssocType = scope.getAssociatedType();
    var methodNamesToCheck =
        List.of(
            "instance_method",
            "static_method",
            "extension_instance_method",
            "extension_static_method");
    for (var methodName : methodNamesToCheck) {
      Function func = null;
      if (scope.getMethodForType(myType, methodName) instanceof Function f) {
        func = f;
      } else if (scope.getMethodForType(myEigenType, methodName) instanceof Function f) {
        func = f;
      } else if (scope.getMethodForType(moduleAssocType, methodName) instanceof Function f) {
        func = f;
      } else {
        fail("Method " + methodName + " not found");
      }
      assertOnlyFirstArgumentIsSelf(func);
    }
  }

  private static void assertOnlyFirstArgumentIsSelf(Function function) {
    var argInfos = function.getSchema().getArgumentInfos();
    var firstArgInfo = argInfos[0];
    assertThat("First arg definition is self", firstArgInfo.getName(), is("self"));
    // There should be no other self in the argument list.
    for (int i = 1; i < argInfos.length; i++) {
      var argInfo = argInfos[i];
      assertThat("No other arg is self", argInfo.getName(), is(not("self")));
    }
  }
}
