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
  @Rule
  public final WithCompilerContext compilerCtx =
      WithCompilerContext.newBuilder()
          .withModifiedCompilerConfig(
              cfg -> cfg.isLintingDisabled(false).staticAnalysisEnabled(true))
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
  public void unusedSymbols_InlineSignature_Parameter() {
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

  @Test
  public void unusedSymbols_InlineSignature_ReturnType() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type A
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.A
            foo -> A = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void unusedSymbols_InlineSignature_TypeCast() {
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
            foo x =
                casted = x : My_Type_1
                casted
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().apply(0);
    expectWarning(imp, List.of("local.Proj.Module.My_Type_2"));
  }

  @Test
  public void unusedSymbols_InTypeAscription_ReturnType() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.T
            foo : T
            foo t = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void unusedSymbols_InTypeAscription_ThrownError() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        type Error
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import T, Error
            foo : T ! Error
            foo t = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void unusedSymbols_InTypeAscription_MultipleThrownErrors() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        type Error_1
        type Error_2
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import T, Error_1, Error_2
            foo : T ! Error_1 | Error_2
            foo t = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void unusedSymbols_InTypeAscription_Complicated() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type A
        type B
        type C
        type D
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import A, B, C, D
            foo : A -> B -> C -> D
            foo a b c d = a + b + c + d
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void unusedSymbols_InTypeAscription_PartiallyAppliedMethod() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type S
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import S, T

            add x y = x + y

            main =
                add_one x = (add 1 x) : S
                add_one 23
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().apply(0);
    expectWarning(imp, List.of("local.Proj.Module.T"));
  }

  @Test
  public void unusedSymbols_ExtensionMethod() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.T
            T.extension_method = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void unusedSymbols_ExtensionMethod_OnModule() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module
            Module.T.extension_method = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void unusedSymbols_ConversionMethod() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        type U
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import T, U
            T.from (that:U) = 42
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

  @Test
  public void typeOnMethodIsUsed() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
            method self = 42
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.T
            main = T.method
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void polyglotImports_AreIgnored() {
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            polyglot java import java.lang.StringBuilder
            polyglot java import java.lang.Double
            main = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void importAll_IsIgnored() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type A
        type B
        A.extension_method self = 42
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import all
            main = A
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedSymbol_InCaseBranch_TypePattern() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.T
            foo x =
                case x of
                    t: T -> t
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedSymbol_InCaseBranch_Constructor() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
            Cons
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module.T import Cons
            foo x =
                case x of
                    Cons -> 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedSymbol_InCaseBranch_TypeConstructor() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
            Cons
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import T
            foo x =
                case x of
                    T.Cons -> 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedSymbol_InCaseBranch_TypeConstructor_WithFields() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
            Cons data
        type S
            Value data
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import S, T
            foo x =
                case x of
                    T.Cons data -> data
                    S.Value d -> d
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedSymbol_InCaseBranch_TypeConstructor_OtherUsages_1() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
            Cons data
        type S
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import S, T
            foo x =
                case x of
                    T.Cons data -> data
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().head();
    expectWarning(imp, List.of("local.Proj.Module.S"));
  }

  @Test
  public void usedSymbol_InCaseBranch_TypeConstructor_OtherUsages_2() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
            Cons data
        type S
        type U
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import S, T, U
            foo x =
                case x of
                    T.Cons data -> data
            """);
    compilerCtx.getCompiler().run(mainMod);
    var imp = mainMod.getIr().imports().head();
    expectWarning(imp, List.of("local.Proj.Module.S", "local.Proj.Module.U"));
  }

  @Test
  public void usedSymbol_InCaseBranch_TypeConstructor_Reexport() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Other_Module"),
        """
        type X
            Cons
        """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        export project.Other_Module.X as T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import T
            foo x =
                case x of
                    T.Cons -> 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedSymbol_InAnnotation_MethodCall() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.T
            @annotation T.method
            foo = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedSymbol_InAnnotation_Expression() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
            Cons
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.Module.T

            method x = x

            @annotation (method x=T.Cons)
            foo = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectNoWarnings(mainMod.getIr());
  }

  @Test
  public void usedExtensionMethodSymbol_FromModule_InsideSyntheticModule() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Lib.A.A"),
        """
        static_method x = x
        """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Lib.Main"),
        """
        export project.A.A
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from local.Lib import A
            main =
                A.static_method 42
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
