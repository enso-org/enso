package org.enso.compiler.pass.lint.unusedimports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;

import java.util.Set;
import java.util.stream.Collectors;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.test.mock.WithCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

public class UsedSymbolsCollectorTest {
  @Rule public final WithCompilerContext compilerCtx = WithCompilerContext.createDefault();

  @Test
  public void collectUsedSymbols_FromSimpleExpression() {
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

            main = My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.Module.My_Type");
  }

  @Test
  public void collectUsedSymbols_FromNestedExpression() {
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

            main =
                func_1 _ =
                    func_2 _ =
                        func_3 _ =
                            My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.Module.My_Type");
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
    expectNoUsedSymbols(mainMod);
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
    expectUsedSymbol(mainMod, "local.Proj.Module.My_Type_2");
  }

  @Test
  public void inlineSignature_Parameter() {
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
    expectUsedSymbol(mainMod, "local.Proj.Module.My_Type_1");
  }

  @Test
  public void inlineSignature_ReturnType() {
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
    expectUsedSymbol(mainMod, "local.Proj.Module.A");
  }

  @Test
  public void typeAscription_ReturnType() {
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
    expectUsedSymbol(mainMod, "local.Proj.Module.T");
  }

  @Test
  public void typeAscription_ThrownError() {
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
    expectUsedSymbols(mainMod, Set.of("local.Proj.Module.T", "local.Proj.Module.Error"));
  }

  @Test
  public void typeAscription_TypeSet() {
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
            foo : A -> B ! C | D
            foo a b = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbols(
        mainMod,
        Set.of(
            "local.Proj.Module.A",
            "local.Proj.Module.B",
            "local.Proj.Module.C",
            "local.Proj.Module.D"));
  }

  @Test
  public void typeAscription_PartiallyAppliedMethod() {
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
    expectUsedSymbol(mainMod, "local.Proj.Module.S");
  }

  @Test
  public void typeCast_NestedMethodBody() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import T
            type My_Type
                method self =
                    func x =
                        (x + 1) : T
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.Module.T");
  }

  @Test
  public void annotation_1() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import T
            @annotation T.method
            foo = 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.Module.T");
  }

  @Test
  public void method_1() {
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
    expectUsedSymbol(mainMod, "local.Proj.Module.method");
  }

  @Test
  public void method_2() {
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
            from project.Module import T
            main = T.method 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.Module.T");
  }

  @Test
  public void caseBranch_1() {
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
    expectUsedSymbol(mainMod, "local.Proj.Module.T.Cons");
  }

  @Test
  public void caseBranch_2() {
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
    expectUsedSymbol(mainMod, "local.Proj.Module.T.Cons");
  }

  @Test
  public void constructors() {
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
    expectUsedSymbols(
        mainMod,
        Set.of(
            "local.Proj.Boolean.Boolean",
            "local.Proj.Boolean.Boolean.False",
            "local.Proj.Boolean.Boolean.True"));
  }

  @Test
  public void constructorInPattern() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type A
        type B
            Value data
        type C
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import A, B, C
            foo x = case x of
                B.Value data -> data
                _ -> 42
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.Module.B.Value");
  }

  @Test
  public void noUsedSymbol_ForUnrelatedImport() {
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
    var usedSymbols = collect(mainMod);
    var imp = mainMod.getIr().imports().apply(1);
    var symsForImp = usedSymbols.getUsedSymbolsForImport(imp);
    assertThat("No used symbols expected, but got: " + symsForImp, symsForImp.isEmpty(), is(true));
  }

  @Test
  public void reexport() {
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
    expectUsedSymbol(mainMod, "local.Proj.Other_Module.My_Type");
  }

  @Test
  public void reexport_Rename() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Other_Module"),
        """
        type Other_Type
        """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        export project.Other_Module.Other_Type as My_Type
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Module import My_Type
            main = My_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.Other_Module.Other_Type");
  }

  @Test
  public void reexport_Case_Branch_TypeConstructor() {
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
    expectUsedSymbol(mainMod, "local.Proj.Other_Module.X.Cons");
  }

  @Test
  public void extensionMethod_ImportAll_DoesNotWork() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Module"),
        """
        type T
        """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.Extensions"),
        """
        import project.Module.T
        T.extension_method = 42
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.Extensions import all
            foo x = x.extension_method
            """);
    compilerCtx.getCompiler().run(mainMod);
    // TODO: extension_method literal does not have any resolution attached.
    // This is responsibility of another pass.
    expectNoUsedSymbols(mainMod);
  }

  /** {@code local.Proj.A} is both synthetic module and a real module. */
  @Test
  public void usedSymbol_FromSyntheticSubmodule() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.A.B"),
        """
        type B_Type
        """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.A"),
        """
        export project.A.B
        type A_Type
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from project.A import B
            main = B.B_Type
            """);
    compilerCtx.getCompiler().run(mainMod);
    expectUsedSymbol(mainMod, "local.Proj.A.B.B_Type");
  }

  /** {@code local.Proj.A} is just synthetic module. */
  @Test
  public void usedExtensionMethod_FromModule_InsideSyntheticModule() {
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
    expectUsedSymbol(mainMod, "local.Lib.A.A.static_method");
  }

  private static UsedSymbols collect(org.enso.compiler.context.CompilerContext.Module mod) {
    var modIr = mod.getIr();
    return UsedSymbolsCollector.collect(modIr, getBindingsMap(modIr));
  }

  private static void expectNoUsedSymbols(org.enso.compiler.context.CompilerContext.Module mod) {
    var usedSymbols = collect(mod);
    for (var i = 0; i < mod.getIr().imports().size(); i++) {
      var imp = mod.getIr().imports().apply(i);
      var symsForImp = usedSymbols.getUsedSymbolsForImport(imp);
      assertThat(symsForImp.isEmpty(), is(true));
    }
  }

  private static void expectUsedSymbol(
      org.enso.compiler.context.CompilerContext.Module mod, String expectedSymbolStr) {
    expectUsedSymbol(mod, 0, QualifiedName.fromString(expectedSymbolStr));
  }

  private static void expectUsedSymbol(
      org.enso.compiler.context.CompilerContext.Module mod, int impIdx, String expectedSymbolStr) {
    expectUsedSymbol(mod, impIdx, QualifiedName.fromString(expectedSymbolStr));
  }

  private static void expectUsedSymbol(
      org.enso.compiler.context.CompilerContext.Module mod,
      int impIdx,
      QualifiedName expectedSymbol) {
    var usedSymbols = collect(mod);
    var imp = getImport(mod, impIdx);
    expectUsedSymbol(usedSymbols, imp, expectedSymbol);
  }

  private static void expectUsedSymbol(
      UsedSymbols usedSymbols, Import imp, String expectedSymbolStr) {
    expectUsedSymbol(usedSymbols, imp, QualifiedName.fromString(expectedSymbolStr));
  }

  private static void expectUsedSymbol(
      UsedSymbols usedSymbols, Import imp, QualifiedName expectedSymbol) {
    var actualSymbols = usedSymbols.getUsedSymbolsForImport(imp);
    assertThat("Single symbol expected", actualSymbols.size(), is(1));
    assertThat(actualSymbols, contains(expectedSymbol));
  }

  private static void expectUsedSymbols(
      org.enso.compiler.context.CompilerContext.Module mod, Set<String> expectedSymbols) {
    expectUsedSymbols(mod, 0, qualified(expectedSymbols));
  }

  private static void expectUsedSymbols(
      org.enso.compiler.context.CompilerContext.Module mod,
      int impIdx,
      Set<QualifiedName> expectedSymbols) {
    var usedSymbols = collect(mod);
    var imp = getImport(mod, impIdx);
    expectUsedSymbols(usedSymbols, imp, expectedSymbols);
  }

  private static void expectUsedSymbols(
      UsedSymbols usedSymbols, Import imp, Set<QualifiedName> expectedSymbols) {
    var actualSymbols = usedSymbols.getUsedSymbolsForImport(imp);
    assertThat(actualSymbols, is(expectedSymbols));
  }

  private static BindingsMap getBindingsMap(Module modIr) {
    return MetadataInteropHelpers.getMetadata(modIr, BindingAnalysis$.MODULE$, BindingsMap.class);
  }

  private static Import getImport(
      org.enso.compiler.context.CompilerContext.Module mod, int impIdx) {
    return getImport(mod.getIr(), impIdx);
  }

  private static Import getImport(Module moduleIr, int impIdx) {
    return moduleIr.imports().apply(impIdx);
  }

  private static Set<QualifiedName> qualified(Set<String> names) {
    return names.stream().map(QualifiedName::fromString).collect(Collectors.toUnmodifiableSet());
  }
}
