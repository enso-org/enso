package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeInferencePropagation;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ModuleUtils;
import org.enso.test.utils.ProjectUtils;
import org.graalvm.polyglot.Source;
import org.junit.Ignore;
import org.junit.Test;
import scala.Option;

public class TypeInferenceTest extends StaticAnalysisTest {
  @Test
  public void zeroAryCheck() throws Exception {
    final URI uri = new URI("memory://zeroAryModuleMethodCheck.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x

                const -> My_Type = My_Type.Value 42

                foo =
                    x = const
                    x
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method foo = ModuleUtils.findStaticMethod(module, "foo");
    assertAtomType("zeroAryModuleMethodCheck.My_Type", ModuleUtils.findAssignment(foo.body(), "x"));
  }

  @Test
  public void functionReturnCheck() throws Exception {
    final URI uri = new URI("memory://functionReturnCheck.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x

                add x y -> My_Type = My_Type.Value (x.x+y.x)

                foo z =
                    a = My_Type.Value 42
                    b = add a z
                    b
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    String myType = "functionReturnCheck.My_Type";

    // The result of `add a z` should be `My_Type` as guaranteed by the return type check of `add`.
    assertAtomType(myType, ModuleUtils.findAssignment(foo.body(), "b"));
  }

  @Test
  public void argChecks() throws Exception {
    final URI uri = new URI("memory://argChecks.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                f1 (x1 : My_Type) =
                    y1 = x1
                    My_Type.Value (y1.v + y1.v)

                f2 : My_Type -> My_Type
                f2 x2 =
                    y2 = x2
                    My_Type.Value (y2.v + y2.v)

                f3 (x3 : My_Type) -> My_Type = My_Type.Value (x3.v + x3.v)
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var myType = "argChecks.My_Type";

    var f1 = ModuleUtils.findStaticMethod(module, "f1");
    var f2 = ModuleUtils.findStaticMethod(module, "f2");
    var f3 = ModuleUtils.findStaticMethod(module, "f3");

    assertAtomType(myType, ModuleUtils.findAssignment(f1, "y1"));
    assertNoInferredType(ModuleUtils.findAssignment(f2, "y2"));

    assertEquals("My_Type -> My_Type", getInferredType(f1).toString());
    // f2 gets argument as Any, because the doc-signature is not checked
    assertEquals("Any -> My_Type", getInferredType(f2).toString());
    assertEquals("My_Type -> My_Type", getInferredType(f3).toString());
  }

  @Test
  public void ascribedExpressions() throws Exception {
    final URI uri = new URI("memory://ascribedExpressions.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x

                f x =
                    y = (x : My_Type)
                    My_Type.Value (y.x + y.x)
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = ModuleUtils.findStaticMethod(module, "f");

    String myType = "ascribedExpressions.My_Type";
    assertAtomType(myType, ModuleUtils.findAssignment(f.body(), "y"));
  }

  @Test
  public void advancedAscribedExpressions() throws Exception {
    final URI uri = new URI("memory://advancedAscribedExpressions.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x
                type Other_Type
                    Value y
                f z =
                    y1 = (z : My_Type | Other_Type)
                    y2 = (z : My_Type & Other_Type)
                    My_Type.Value (y1.x + y2.x)
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = ModuleUtils.findStaticMethod(module, "f");

    var y1Type = getInferredType(ModuleUtils.findAssignment(f.body(), "y1"));
    if (y1Type instanceof TypeRepresentation.SumType sumType) {
      var gotSet =
          new HashSet<>(sumType.types().stream().map(TypeRepresentation::toString).toList());
      assertEquals(Set.of("My_Type", "Other_Type"), gotSet);
    } else {
      fail("y1 should be a sum type, but got " + y1Type);
    }

    var y2Type = getInferredType(ModuleUtils.findAssignment(f.body(), "y2"));
    if (y2Type instanceof TypeRepresentation.IntersectionType intersectionType) {
      var gotSet =
          new HashSet<>(
              intersectionType.types().stream().map(TypeRepresentation::toString).toList());
      assertEquals(Set.of("My_Type", "Other_Type"), gotSet);
    } else {
      fail("y2 should be an intersection type, but got " + y2Type);
    }
  }

  @Test
  public void ascribedFunctionType() throws Exception {
    final URI uri = new URI("memory://ascribedFunctionType.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x
                type Other_Type
                    Value y
                f z w =
                    f1 = (z : My_Type -> Other_Type)
                    f2 = (w : My_Type -> My_Type -> Other_Type)
                    f2 (f1 (My_Type.Value 42))
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = ModuleUtils.findStaticMethod(module, "f");

    // Here we will only know that both f1 and f2 are Any -> Any - because the ascribed check only
    // really performs a
    // `is_a Function` check, we do not know anything about the argument nor return type of this
    // function,
    // unfortunately.
    TypeRepresentation primitiveFunctionType =
        new TypeRepresentation.ArrowType(TypeRepresentation.ANY, TypeRepresentation.ANY);
    assertEquals(
        primitiveFunctionType, getInferredType(ModuleUtils.findAssignment(f.body(), "f1")));
    assertEquals(
        primitiveFunctionType, getInferredType(ModuleUtils.findAssignment(f.body(), "f2")));
  }

  @Test
  public void literals() throws Exception {
    final URI uri = new URI("memory://literals.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                f =
                    x = 42
                    y = "foo"
                    z = 1.5
                    w = [1, 2, 3]
                    x.to_text + y + z.to_text + w.to_text
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = ModuleUtils.findStaticMethod(module, "f");

    assertAtomType("Standard.Base.Data.Numbers.Integer", ModuleUtils.findAssignment(f, "x"));
    assertAtomType("Standard.Base.Data.Text.Text", ModuleUtils.findAssignment(f, "y"));
    assertAtomType("Standard.Base.Data.Numbers.Float", ModuleUtils.findAssignment(f, "z"));
    assertAtomType("Standard.Base.Data.Vector.Vector", ModuleUtils.findAssignment(f, "w"));
  }

  @Test
  public void bindingsFlow() throws Exception {
    final URI uri = new URI("memory://bindingsFlow.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                foo x =
                    y = (x : My_Type)
                    z = y
                    w = z
                    w
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var myType = "bindingsFlow.My_Type";

    assertAtomType(myType, ModuleUtils.findAssignment(foo, "w"));
  }

  @Test
  public void checkedArgumentTypes() throws Exception {
    final URI uri = new URI("memory://checkedArgumentTypes.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                foo (x1 : My_Type) x2 =
                    y1 = x1
                    y2 = x2
                    [y1, y2]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var myType = "checkedArgumentTypes.My_Type";

    // Type from argument
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "y1"));

    // No type
    assertNoInferredType(ModuleUtils.findAssignment(foo, "y2"));
  }

  @Test
  public void innerFunctionType() throws Exception {
    final URI uri = new URI("memory://innerFunctionType.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                foo =
                    f (x : My_Type) (y : My_Type) -> My_Type = My_Type.Value x.v+y.v

                    f1 = f
                    y = f (My_Type.Value 1) (My_Type.Value 2)
                    [y, f1]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var f1Type = getInferredType(ModuleUtils.findAssignment(foo, "f1"));
    assertEquals("My_Type -> (My_Type -> My_Type)", f1Type.toString());

    // and result of application is typed as the return type:
    assertAtomType("innerFunctionType.My_Type", ModuleUtils.findAssignment(foo, "y"));
  }

  @Test
  public void zeroArgConstructor() throws Exception {
    final URI uri = new URI("memory://zeroArgConstructor.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Singleton
                foo =
                    # x = zeroArgConstructor.My_Type
                    x = My_Type.Singleton
                    x
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var myType = "zeroArgConstructor.My_Type";
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x"));
  }

  @Test
  public void multiArgConstructor() throws Exception {
    final URI uri = new URI("memory://multiArgConstructor.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x y z
                foo =
                    x = My_Type.Value 1 2 3
                    x
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var myType = "multiArgConstructor.My_Type";
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x"));
  }

  @Test
  public void nonexistentConstructor() throws Exception {
    final URI uri = new URI("memory://nonexistentConstructor.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x y z
                foo =
                    x = My_Type.Non_Existent 1
                    x
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    var x = ModuleUtils.findAssignment(foo, "x");

    assertNoInferredType(x);
    assertEquals(
        List.of(
            new Warning.NoSuchMethod(
                x.expression().identifiedLocation(),
                "constructor `Non_Existent` on type (type My_Type)")),
        ModuleUtils.getImmediateDiagnostics(x.expression()));
  }

  @Test
  public void constructorWithDefaults() throws Exception {
    final URI uri = new URI("memory://constructorWithDefaults.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value x y=100 z=200
                    All_Defaults a=1000 b=2000
                foo =
                    x1 = My_Type.Value 1 2 3
                    x2 = My_Type.Value 1 2
                    x3 = My_Type.Value 1
                    x4 = My_Type.Value
                    x5 = My_Type.Value 1 ...
                    x6 = My_Type.All_Defaults
                    x7 = My_Type.All_Defaults ...
                    [x1, x2, x3, x4, x5, x6, x7]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var myType = "constructorWithDefaults.My_Type";

    // The commented out expressions document the desired behaviour - we correctly infer which
    // arguments were defaulted.
    // Before that is working, we just ensure we did not infer any 'unexpected' type for the
    // results.
    // assertAtomType(myType, findAssignment(foo, "x1"));
    assertNoInferredType(ModuleUtils.findAssignment(foo, "x1"));

    // assertAtomType(myType, findAssignment(foo, "x2"));
    assertNoInferredType(ModuleUtils.findAssignment(foo, "x2"));

    // assertAtomType(myType, findAssignment(foo, "x3"));
    assertNoInferredType(ModuleUtils.findAssignment(foo, "x3"));

    assertNotEquals(
        Optional.of(myType), getInferredTypeOption(ModuleUtils.findAssignment(foo, "x4")));
    assertNotEquals(
        Optional.of(myType), getInferredTypeOption(ModuleUtils.findAssignment(foo, "x5")));

    // assertAtomType(myType, findAssignment(foo, "x6"));
    assertNoInferredType(ModuleUtils.findAssignment(foo, "x6"));

    assertNotEquals(
        Optional.of(myType), getInferredTypeOption(ModuleUtils.findAssignment(foo, "x7")));
  }

  @Test
  public void commonIfThenElse() throws Exception {
    final URI uri = new URI("memory://commonIfThenElse.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                f x =
                  y = if x == 10 then 1 else 2
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertAtomType("Standard.Base.Data.Numbers.Integer", ModuleUtils.findAssignment(f, "y"));
  }

  @Test
  public void commonCase() throws Exception {
    final URI uri = new URI("memory://commonCase.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                f x =
                  y = case x of
                    1 -> My_Type.Value 1
                    2 -> My_Type.Value 20
                    _ -> My_Type.Value 300
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var myType = "commonCase.My_Type";
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertAtomType(myType, ModuleUtils.findAssignment(f, "y"));
  }

  @Test
  public void inferBoundsFromCaseAlias() throws Exception {
    final URI uri = new URI("memory://inferBoundsFromCaseAlias.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                f x =
                  y = case x of
                    i : My_Type -> i
                    _ -> My_Type.Value 0
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var myType = "inferBoundsFromCaseAlias.My_Type";
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertAtomType(myType, ModuleUtils.findAssignment(f, "y"));
  }

  /**
   * This is more complex than inferBoundsFromCaseAlias, as it needs to add a type constraint only
   * in one branch. We will need to ensure that we duplicate the local scopes in each branch to
   * avoid bad sharing.
   */
  @Ignore("TODO for much much later: equality bounds in case")
  @Test
  public void inferEqualityBoundsFromCase() throws Exception {
    final URI uri = new URI("memory://inferEqualityBoundsFromCase.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                f x =
                  y = case x of
                    _ : My_Type -> x
                    _ -> My_Type.Value 42
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var myType = "inferEqualityBoundsFromCase.My_Type";
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertAtomType(myType, ModuleUtils.findAssignment(f, "y"));
  }

  @Ignore("TODO for much much later: equality bounds in case")
  @Test
  public void inferEqualityBoundsFromCaseLiteral() throws Exception {
    final URI uri = new URI("memory://inferEqualityBoundsFromCaseLiteral.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                f x =
                  y = case x of
                    1 -> x
                    "foo" -> x
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertSumType(ModuleUtils.findAssignment(f, "y"), "Integer", "Text");
  }

  @Ignore("TODO for much much later: equality bounds in case")
  @Test
  public void inferEqualityBoundsFromCaseEdgeCase() throws Exception {
    // This test ensures that the equality bound from _:Other_Type is only applicable in its branch
    // and does not 'leak' to other branches.
    final URI uri = new URI("memory://inferEqualityBoundsFromCaseEdgeCase.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                type Other_Type
                    Value o
                f x =
                  y = case x of
                    _ : Other_Type -> My_Type.Value 42
                    _ : My_Type -> x
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var myType = "inferEqualityBoundsFromCaseEdgeCase.My_Type";
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertAtomType(myType, ModuleUtils.findAssignment(f, "y"));
  }

  @Test
  public void sumTypeFromCase() throws Exception {
    final URI uri = new URI("memory://sumTypeFromCase.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                type Other_Type
                    Value o
                f x =
                  y = case x of
                    1 -> My_Type.Value 42
                    2 -> Other_Type.Value 23
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertSumType(ModuleUtils.findAssignment(f, "y"), "My_Type", "Other_Type");
  }

  @Test
  public void sumTypeFromIf() throws Exception {
    final URI uri = new URI("memory://sumTypeFromIf.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                f x =
                  y = if x == 1 then "foo" else 42
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertSumType(ModuleUtils.findAssignment(f, "y"), "Text", "Integer");
  }

  @Test
  public void sumTypeFromIfWithoutElse() throws Exception {
    final URI uri = new URI("memory://sumTypeFromIf.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                f x =
                  y = if x == 1 then "foo"
                  y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = ModuleUtils.findStaticMethod(module, "f");
    assertSumType(ModuleUtils.findAssignment(f, "y"), "Text", "Nothing");
  }

  @Test
  public void typeInferenceWorksInsideMemberMethods() throws Exception {
    final URI uri = new URI("memory://typeInferenceWorksInsideMemberMethods.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                    static_method (x : My_Type) =
                        y = x
                        z = My_Type.Value 23
                        w = 42
                        [y, z, w]

                    member_method self (x : My_Type) =
                        y = x
                        z = My_Type.Value 23
                        w = 42
                        [y, z, w]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var myType = "typeInferenceWorksInsideMemberMethods.My_Type";

    var staticMethod = ModuleUtils.findMemberMethod(module, "My_Type", "static_method");
    assertAtomType(myType, ModuleUtils.findAssignment(staticMethod, "y"));
    assertAtomType(myType, ModuleUtils.findAssignment(staticMethod, "z"));
    assertAtomType(
        "Standard.Base.Data.Numbers.Integer", ModuleUtils.findAssignment(staticMethod, "w"));

    var memberMethod = ModuleUtils.findMemberMethod(module, "My_Type", "member_method");
    assertAtomType(myType, ModuleUtils.findAssignment(memberMethod, "y"));
    assertAtomType(myType, ModuleUtils.findAssignment(memberMethod, "z"));
    assertAtomType(
        "Standard.Base.Data.Numbers.Integer", ModuleUtils.findAssignment(memberMethod, "w"));
  }

  @Ignore("TODO: self resolution")
  @Test
  public void typeInferenceOfSelf() throws Exception {
    final URI uri = new URI("memory://typeInferenceOfSelf.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                    member_method self =
                        y = self
                        y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = ModuleUtils.findMemberMethod(module, "My_Type", "member_method");
    var myType = "typeInferenceOfSelf.My_Type";
    assertAtomType(myType, ModuleUtils.findAssignment(f, "y"));
  }

  @Test
  public void notInvokable() throws Exception {
    final URI uri = new URI("memory://notInvokable.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                foo unknown =
                    x1 = 0 1
                    x2 = "a" 2
                    x3 = unknown 3
                    [x1, x2, x3]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var x1 = ModuleUtils.findAssignment(foo, "x1");
    assertEquals(
        List.of(new Warning.NotInvokable(x1.expression().identifiedLocation(), "Integer")),
        ModuleUtils.getImmediateDiagnostics(x1.expression()));

    var x2 = ModuleUtils.findAssignment(foo, "x2");
    assertEquals(
        List.of(new Warning.NotInvokable(x2.expression().identifiedLocation(), "Text")),
        ModuleUtils.getImmediateDiagnostics(x2.expression()));

    var x3 = ModuleUtils.findAssignment(foo, "x3");
    assertEquals(
        "x3 should not contain any warnings",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(x3.expression()));
  }

  /**
   * Such signatures are not checked yet, but the syntax _is_ allowed and it is used in some places
   * for documentation purposes, so it should not be triggering any errors.
   */
  @Test
  public void noErrorInParametricTypeSignatures() throws Exception {
    final URI uri = new URI("memory://noErrorInParametricTypeSignatures.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type a
                    Value v
                type Other_Type
                    Value (v : My_Type Other_Type)

                foo1 : My_Type Other_Type -> My_Type Other_Type
                foo1 v = v

                foo2 (v : My_Type Other_Type) -> My_Type Other_Type = v
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(module));
  }

  @Test
  public void noTypeErrorIfConversionExists() throws Exception {
    final URI uri = new URI("memory://noTypeErrorIfConversionExists.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                type Other_Type
                    Value o
                Other_Type.from (that : My_Type) = Other_Type.Value that.v+1000

                function_taking_other o:Other_Type =
                    o.o

                foo =
                    x = My_Type.Value 12
                    y = function_taking_other x
                    y
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var y = ModuleUtils.findAssignment(foo, "y");
    assertEquals(
        "valid conversion should ensure there is no type error",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(y.expression()));
  }

  @Test
  public void noTypeErrorIfConversionExistsInTypeScope() throws Exception {
    final URI uriA = new URI("memory://local.Project1.typeDef.enso");
    final Source srcA =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                type Other_Type
                    Value o
                Other_Type.from (that : My_Type) = Other_Type.Value that.v+1000
                """,
                uriA.getAuthority())
            .uri(uriA)
            .buildLiteral();
    compile(srcA);

    final URI uriB = new URI("memory://noTypeErrorIfConversionExistsInTypeScope.enso");
    final Source srcB =
        Source.newBuilder(
                "enso",
                """
                from local.Project1.typeDef import My_Type, Other_Type

                function_taking_other o:Other_Type =
                    o.o

                foo =
                    x = My_Type.Value 12
                    y = function_taking_other x
                    y
                """,
                uriB.getAuthority())
            .uri(uriB)
            .buildLiteral();

    var moduleB = compile(srcB);
    var foo = ModuleUtils.findStaticMethod(moduleB, "foo");

    var y = ModuleUtils.findAssignment(foo, "y");
    assertEquals(
        "valid conversion should ensure there is no type error",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(y.expression()));
  }

  @Test
  public void noTypeErrorIfConversionIsImported() throws Exception {
    final URI uriA = new URI("memory://local.Project1.typeDef.enso");
    final Source srcA =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                type Other_Type
                    Value o
                """,
                uriA.getAuthority())
            .uri(uriA)
            .buildLiteral();
    compile(srcA);

    final URI uriB = new URI("memory://local.Project1.conversionDef.enso");
    final Source srcB =
        Source.newBuilder(
                "enso",
                """
                from local.Project1.typeDef import My_Type, Other_Type
                Other_Type.from (that : My_Type) = Other_Type.Value that.v+1000
                """,
                uriB.getAuthority())
            .uri(uriB)
            .buildLiteral();
    compile(srcB);

    final URI uriC = new URI("memory://errorIfNotImported.enso");
    final Source srcC =
        Source.newBuilder(
                "enso",
                """
                from local.Project1.typeDef import My_Type, Other_Type

                function_taking_other o:Other_Type =
                    o.o

                foo =
                    x = My_Type.Value 12
                    y = function_taking_other x
                    y
                """,
                uriC.getAuthority())
            .uri(uriC)
            .buildLiteral();

    var moduleC = compile(srcC);
    var fooC = ModuleUtils.findStaticMethod(moduleC, "foo");
    var yC = ModuleUtils.findAssignment(fooC, "y");
    // Conversion is not imported, so there should be a type error
    assertTypeMismatch(yC.expression(), "Other_Type", "My_Type");

    var uriD = new URI("memory://noTypeErrorIfConversionIsImported.enso");
    var srcD =
        Source.newBuilder(
                "enso",
                """
                from local.Project1.typeDef import My_Type, Other_Type
                from local.Project1.conversionDef import all

                function_taking_other o:Other_Type =
                    o.o

                foo =
                    x = My_Type.Value 12
                    y = function_taking_other x
                    y
                """,
                uriD.getAuthority())
            .uri(uriD)
            .buildLiteral();
    var moduleD = compile(srcD);
    var fooD = ModuleUtils.findStaticMethod(moduleD, "foo");
    var yD = ModuleUtils.findAssignment(fooD, "y");
    assertEquals(
        "valid conversion should ensure there is no type error",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(yD.expression()));
  }

  @Test
  public void typeErrorFunctionToObject() throws Exception {
    final URI uri = new URI("memory://typeErrorFunctionToObject.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                foo =
                    f x = x
                    takes_my_type (m : My_Type) = m
                    y1 = takes_my_type f
                    g (x : My_Type) -> My_Type = x
                    y2 = takes_my_type g

                    takes_function (f : Any -> Any) = f
                    y3 = takes_function (My_Type.Value 123)
                    y4 = takes_function My_Type.Value
                    y5 = takes_function f
                    [y1, y2, y3, y4, y5]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var y1 = ModuleUtils.findAssignment(foo, "y1");
    assertTypeMismatch(y1.expression(), "My_Type", "Any -> Any");

    var y2 = ModuleUtils.findAssignment(foo, "y2");
    assertTypeMismatch(y2.expression(), "My_Type", "My_Type -> My_Type");

    var y3 = ModuleUtils.findAssignment(foo, "y3");
    assertTypeMismatch(y3.expression(), "Any -> Any", "My_Type");

    // Not-applied constructor _is_ a function
    var y4 = ModuleUtils.findAssignment(foo, "y4");
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(y4));

    var y5 = ModuleUtils.findAssignment(foo, "y5");
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(y5));
  }

  @Test
  public void typeErrorInLocalCall() throws Exception {
    final URI uri = new URI("memory://typeErrorInLocalCall.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                type Other_Type
                    Value o
                foo =
                    bar (x : Other_Type) = x
                    y = My_Type.Value 10
                    z = bar y
                    z
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var z = ModuleUtils.findAssignment(foo, "z");
    var arg =
        switch (z.expression()) {
          case Application.Prefix app -> app.arguments().head();
          default ->
              throw new AssertionError(
                  "Expected " + z.showCode() + " to be an application expression.");
        };
    var typeError = new Warning.TypeMismatch(arg.identifiedLocation(), "Other_Type", "My_Type");
    assertEquals(List.of(typeError), ModuleUtils.getImmediateDiagnostics(arg));
  }

  @Ignore("TODO: distinguish return type ascription (no conversions) from regular one: #12292")
  @Test
  public void typeErrorInReturn() throws Exception {
    final URI uri = new URI("memory://typeErrorInReturn.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                foo =
                    x -> My_Type = 10
                    x
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var x = ModuleUtils.findAssignment(foo, "x");
    assertTypeMismatch(x.expression(), "My_Type", "Integer");
  }

  @Test
  public void integerIsSubclassOfNumber() throws Exception {
    final URI uri = new URI("memory://notInvokable.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                from Standard.Base import Integer, Number

                foo =
                    num -> Number = 42
                    neg n:Integer -> Integer = -n
                    neg num
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    foo.preorder()
        .foreach(
            (ir) -> {
              if (ir.getDiagnostics().toList().nonEmpty()) {
                fail(
                    "There should be no warnings "
                        + ir.getDiagnostics().toList()
                        + " at "
                        + ir.showCode());
              }
              return null;
            });
  }

  @Test
  public void noTypeErrorIfUnsure() throws Exception {
    final URI uri = new URI("memory://notInvokable.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                foo unknown =
                    bar (x : My_Type) = x
                    baz -> My_Type = unknown
                    y = bar unknown
                    z = (unknown : My_Type)
                    [y, z]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var y = ModuleUtils.findAssignment(foo, "y");
    assertEquals(
        "y should not contain any warnings",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(y.expression()));

    var z = ModuleUtils.findAssignment(foo, "z");
    assertEquals(
        "z should not contain any warnings",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(z.expression()));

    var baz = ModuleUtils.findAssignment(foo, "baz");
    assertEquals(
        "baz should not contain any warnings",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(baz.expression()));
  }

  @Test
  public void globalMethodTypes() throws Exception {
    final URI uri = new URI("memory://globalMethodTypes.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                const -> My_Type = My_Type.Value 23
                check (x : My_Type) -> My_Type = x

                foo =
                    x1 = const
                    x2 = check
                    x3 = check const
                    [x1, x2, x3]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var myType = "globalMethodTypes.My_Type";

    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x1"));
    assertEquals(
        "My_Type -> My_Type", getInferredType(ModuleUtils.findAssignment(foo, "x2")).toString());
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x3"));
  }

  @Test
  public void memberMethodCalls() throws Exception {
    final URI uri = new URI("memory://memberMethodCalls.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                    zero_arg self -> My_Type = My_Type.Value [self.v]
                    one_arg self (x : My_Type) -> My_Type = My_Type.Value [self.v, x.v]

                    static_zero -> My_Type = My_Type.Value 42
                    static_one (x : My_Type) -> My_Type = My_Type.Value [x.v, 1]

                My_Type.extension_method self -> My_Type = My_Type.Value [self.v, 2]

                foo =
                    inst = My_Type.Value 23
                    x1 = inst.zero_arg
                    x2 = inst.one_arg inst
                    x3 = My_Type.static_zero
                    x4 = My_Type.static_one inst

                    # And extension methods
                    x5 = inst.extension_method
                    [x1, x2, x3, x4, x5]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var myType = "memberMethodCalls.My_Type";

    assertAtomType(myType, ModuleUtils.findAssignment(foo, "inst"));
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x1"));
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x2"));
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x3"));
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x4"));
    assertAtomType(myType, ModuleUtils.findAssignment(foo, "x5"));
  }

  @Test
  public void staticCallWithWrongType() throws Exception {
    final URI uri = new URI("memory://staticCallWithWrongType.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                    member_method self = [self.v]

                type Other_Type
                    Constructor v

                    member_method = [self.v, self.v]

                foo =
                    other = Other_Type.Constructor 44
                    x1 = My_Type.member_method self=other
                    x1
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var x1 = ModuleUtils.findAssignment(foo, "x1");
    assertTypeMismatch(x1.expression(), "My_Type", "Other_Type");
  }

  @Test
  public void defaultArgumentWithWrongType() throws Exception {
    final URI uri = new URI("memory://defaultArgumentWithWrongType.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                type Other_Type
                    Constructor v

                foo (arg : My_Type = Other_Type.Constructor 1) = arg
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    var fooBody = foo.body();
    if (!(fooBody instanceof Function.Lambda fooLambda)) {
      fail("Expected the body of the function to be a lambda, but got " + fooBody);
    } else {
      var arg = fooLambda.arguments().find((a) -> a.name().name().equals("arg")).get();
      assertTypeMismatch(arg.defaultValue().get(), "My_Type", "Other_Type");
    }
  }

  @Ignore("TODO")
  @Test
  public void returnWrongType() throws Exception {
    final URI uri = new URI("memory://returnWrongType.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                type Other_Type
                    Constructor v

                foo -> My_Type = Other_Type.Constructor 1
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    assertTypeMismatch(foo, "My_Type", "Other_Type");
  }

  @Test
  public void callingFieldGetters() throws Exception {
    final URI uri = new URI("memory://callingFieldGetters.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Constructor_1 (field_a : Typ_X) (field_b : Typ_Y)
                    Constructor_2 (field_b : Typ_Z)
                    Constructor_3 (field_c : Typ_Z)
                    Constructor_4 (field_c : Typ_Z)
                    Constructor_5 field_d

                type Typ_X
                type Typ_Y
                type Typ_Z

                foo (instance : My_Type) =
                    x_a = instance.field_a
                    x_b = instance.field_b
                    x_c = instance.field_c
                    x_d = instance.field_d
                    [x_a, x_b, x_c, x_d]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    assertAtomType("callingFieldGetters.Typ_X", ModuleUtils.findAssignment(foo, "x_a"));
    // We don't know which constructor was used, so if the field appears in many constructors, it
    // resolves to a sum type
    assertSumType(ModuleUtils.findAssignment(foo, "x_b"), "Typ_Y", "Typ_Z");

    // We have two constructors with field `field_c`, but they have the same type so the sum type
    // should have been simplified
    assertAtomType("callingFieldGetters.Typ_Z", ModuleUtils.findAssignment(foo, "x_c"));

    // We shouldn't get a No_Such_Method error on a field with no type ascription:
    var x_d = ModuleUtils.findAssignment(foo, "x_d");
    assertEquals(
        "Field access should not yield any warnings",
        List.of(),
        ModuleUtils.getDescendantsDiagnostics(x_d));
  }

  @Test
  public void noSuchMethodStaticCheck() throws Exception {
    final URI uri = new URI("memory://noSuchMethodStaticCheck.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                import Standard.Base.Any.Any

                type My_Type
                    Value v

                    method_one self = 42
                    static_method = 44

                foo =
                    inst = My_Type.Value 23
                    x1 = inst.method_one
                    x2 = inst.method_two
                    x3 = inst.to_text
                    x4 = inst.is_error
                    x5 = inst.static_method
                    [x1, x2, x3, x4, x5]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    var x1 = ModuleUtils.findAssignment(foo, "x1");
    var x2 = ModuleUtils.findAssignment(foo, "x2");
    var x3 = ModuleUtils.findAssignment(foo, "x3");
    var x4 = ModuleUtils.findAssignment(foo, "x4");
    var x5 = ModuleUtils.findAssignment(foo, "x5");

    // member method is defined
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(x1.expression()));

    // this method is not found
    assertEquals(
        List.of(
            new Warning.NoSuchMethod(
                x2.expression().identifiedLocation(),
                "member method `method_two` on type My_Type")),
        ModuleUtils.getImmediateDiagnostics(x2.expression()));

    // delegating to Any
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(x3.expression()));
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(x4.expression()));

    // calling a static method on an instance _does not work_, so we get a warning telling there's
    // no such _member_ method
    assertEquals(
        List.of(
            new Warning.NoSuchMethod(
                x5.expression().identifiedLocation(),
                "member method `static_method` on type My_Type")),
        ModuleUtils.getImmediateDiagnostics(x5.expression()));
  }

  @Test
  public void noSuchMethodInsideMethodWithDefaultArgs() throws Exception {
    final URI uri = new URI("memory://noSuchMethodInsideMethodWithArgs.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                import Standard.Base.Any.Any

                type My_Type
                    Value v
                    method_one self = 42

                foo arg1 arg2="default" =
                    inst = My_Type.Value 23
                    x1 = inst.method_one
                    x2 = inst.nonexistent
                    [x1, x2]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    var x1 = ModuleUtils.findAssignment(foo, "x1");
    var x2 = ModuleUtils.findAssignment(foo, "x2");

    // member method is defined
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(x1.expression()));

    // this method is not found
    assertEquals(
        List.of(
            new Warning.NoSuchMethod(
                x2.expression().identifiedLocation(),
                "member method `nonexistent` on type My_Type")),
        ModuleUtils.getImmediateDiagnostics(x2.expression()));
  }

  @Test
  public void alwaysKnowsMethodsOfAny() throws Exception {
    final URI uri = new URI("memory://alwaysKnowsMethodsOfAny.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                foo x =
                    txt1 = x.to_text
                    txt2 = 42.to_text
                    txt3 = (My_Type.Value 1).to_text

                    bool = (x == x)
                    [txt1, txt2, txt3, bool]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    assertAtomType("Standard.Base.Data.Text.Text", ModuleUtils.findAssignment(foo, "txt1"));
    assertAtomType("Standard.Base.Data.Text.Text", ModuleUtils.findAssignment(foo, "txt2"));
    assertAtomType("Standard.Base.Data.Text.Text", ModuleUtils.findAssignment(foo, "txt3"));

    assertAtomType("Standard.Base.Data.Boolean.Boolean", ModuleUtils.findAssignment(foo, "bool"));
  }

  @Ignore("TODO: self resolution")
  @Test
  public void noSuchMethodOnSelf() throws Exception {
    final URI uri = new URI("memory://noSuchMethodOnSelf.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v

                    method_one self = 42
                    method_two self =
                        x1 = self.method_one
                        x2 = self.non_existent_method
                        [x1, x2]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var method_two = ModuleUtils.findMemberMethod(module, "My_Type", "method_two");
    var x1 = ModuleUtils.findAssignment(method_two, "x1");
    var x2 = ModuleUtils.findAssignment(method_two, "x2");

    // member method is defined
    assertEquals(List.of(), ModuleUtils.getDescendantsDiagnostics(x1.expression()));

    // this method is not found
    assertEquals(
        List.of(
            new Warning.NoSuchMethod(
                x2.expression().identifiedLocation(),
                "member method `non_existent_method` on type My_Type")),
        ModuleUtils.getImmediateDiagnostics(x2.expression()));
  }

  @Test
  public void callingExtensionMethodDefinedElsewhere() throws Exception {
    final URI uriA = new URI("memory://local.Project1.Mod_A.enso");
    final Source srcA =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                """,
                uriA.getAuthority())
            .uri(uriA)
            .buildLiteral();
    compile(srcA);

    final URI uriB = new URI("memory://local.Project1.Mod_B.enso");
    final Source srcB =
        Source.newBuilder(
                "enso",
                """
                import local.Project1.Mod_A.My_Type

                type Typ_X
                    Value a
                type Typ_Y
                    Value a

                My_Type.member self -> Typ_X = Typ_X.Value self
                My_Type.static -> Typ_Y = Typ_Y.Value 32
                """,
                uriB.getAuthority())
            .uri(uriB)
            .buildLiteral();
    compile(srcB);

    final URI uriC = new URI("memory://local.Project1.Mod_C.enso");
    final Source srcC =
        Source.newBuilder(
                "enso",
                """
                import local.Project1.Mod_A.My_Type
                from local.Project1.Mod_B import all

                foo =
                    inst = My_Type.Value 23
                    x1 = inst.member
                    x2 = My_Type.static
                    [x1, x2]
                """,
                uriC.getAuthority())
            .uri(uriC)
            .buildLiteral();
    var modC = compile(srcC);
    var foo = ModuleUtils.findStaticMethod(modC, "foo");

    assertAtomType("local.Project1.Mod_B.Typ_X", ModuleUtils.findAssignment(foo, "x1"));
    assertAtomType("local.Project1.Mod_B.Typ_Y", ModuleUtils.findAssignment(foo, "x2"));
  }

  @Test
  public void callingReexportedExtensionMethods() throws Exception {
    // Base type definition
    final URI uriA = new URI("memory://local.Project1.Mod_A.enso");
    final Source srcA =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    Value v
                """,
                uriA.getAuthority())
            .uri(uriA)
            .buildLiteral();
    compile(srcA);

    // Extension methods defined in another module
    final URI uriB = new URI("memory://local.Project1.Mod_B.enso");
    final Source srcB =
        Source.newBuilder(
                "enso",
                """
                import local.Project1.Mod_A.My_Type

                type Typ_X
                    Value a
                type Typ_Y
                    Value a

                My_Type.member self -> Typ_X = Typ_X.Value self
                My_Type.static -> Typ_Y = Typ_Y.Value 32
                """,
                uriB.getAuthority())
            .uri(uriB)
            .buildLiteral();
    compile(srcB);

    // Re-exports of the type and the extension method
    final URI uriC = new URI("memory://local.Project1.Mod_C.enso");
    final Source srcC =
        Source.newBuilder(
                "enso",
                """
                export local.Project1.Mod_A.My_Type
                export local.Project1.Mod_B.member
                """,
                uriC.getAuthority())
            .uri(uriC)
            .buildLiteral();
    compile(srcC);

    final URI uriD = new URI("memory://local.Project1.Mod_D.enso");
    final Source srcD =
        Source.newBuilder(
                "enso",
                """
                from local.Project1.Mod_C import all

                foo =
                    inst = My_Type.Value 23
                    x1 = inst.member
                    x2 = My_Type.static
                    [x1, x2]
                """,
                uriD.getAuthority())
            .uri(uriD)
            .buildLiteral();
    var modD = compile(srcD);
    var foo = ModuleUtils.findStaticMethod(modD, "foo");

    assertAtomType("local.Project1.Mod_B.Typ_X", ModuleUtils.findAssignment(foo, "x1"));
    assertAtomType("local.Project1.Mod_B.Typ_Y", ModuleUtils.findAssignment(foo, "x2"));
  }

  @Test
  public void resolveImportedConstructor() throws Exception {
    final URI uri = new URI("memory://local.Project1.Mod_A.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                from project.Mod_A.My_Type import My_Constructor

                type My_Type
                    My_Constructor v

                foo =
                    x1 = My_Constructor 1
                    x1
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    var x1 = ModuleUtils.findAssignment(foo, "x1");
    assertAtomType("local.Project1.Mod_A.My_Type", x1);
  }

  @Ignore("TODO: for later")
  @Test
  public void resolveFQNConstructor() throws Exception {
    final URI uri = new URI("memory://local.Project1.Mod_A.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type My_Type
                    My_Constructor v

                foo =
                    x1 = local.Project1.Mod_A.My_Type.My_Constructor 1
                    x1
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    var x1 = ModuleUtils.findAssignment(foo, "x1");
    assertAtomType("local.Project1.Mod_A.My_Type", x1);
  }

  public static Source anyPrecedenceTestSource() throws URISyntaxException {
    final URI uri = new URI("memory://local.Project1.Mod_A.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                import Standard.Base.Any.Any

                type A
                    A_Value
                type B
                    B_Value
                type C
                    C_Value
                type D
                    D_Value
                type E
                    E_Value

                Any.method self -> A = A.A_Value
                Any.static_method -> D = D.D_Value

                type My_Type
                    Value

                    method self -> B = B.B_Value
                    static_method -> E = E.E_Value

                type Other_Type
                    Value

                method -> C = C.C_Value

                foo =
                    x1 = Other_Type.Value.method
                    x2 = My_Type.Value.method
                    x3 = method
                    x4 = My_Type.method
                    x5 = Any.static_method
                    x6 = My_Type.static_method
                    [x1, x2, x3, x4, x5, x6]
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();
    return src;
  }

  @Ignore("TODO: missing IR on Numbers")
  @Test
  public void overrideMethodOnNumberThroughAny() throws URISyntaxException {
    final URI uri = new URI("memory://local.Project1.Mod_A.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type A
                    A_Value

                Any.method self -> A = A.A_Value

                foo =
                    x1 = 42.method
                    x1
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");
    var x1 = ModuleUtils.findAssignment(foo, "x1");
    assertAtomType("local.Project1.Mod_A.A", x1);
  }

  @Test
  public void precedenceOfMethodsOnAny() throws URISyntaxException {
    var module = compile(anyPrecedenceTestSource());
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    // Other_Type dispatches to parent - Any and gets A
    var x1 = ModuleUtils.findAssignment(foo, "x1");
    assertAtomType("local.Project1.Mod_A.A", x1);

    // My_Type dispatches to overridden and gets B
    var x2 = ModuleUtils.findAssignment(foo, "x2");
    assertAtomType("local.Project1.Mod_A.B", x2);

    // module method overrides Any method - we get C
    var x3 = ModuleUtils.findAssignment(foo, "x3");
    assertAtomType("local.Project1.Mod_A.C", x3);

    // Calling the Any method statically on a type calls the Any implementation (it's not a static
    // syntax for the override)
    var x4 = ModuleUtils.findAssignment(foo, "x4");
    assertAtomType("local.Project1.Mod_A.A", x4);

    var x6 = ModuleUtils.findAssignment(foo, "x5");
    assertAtomType("local.Project1.Mod_A.D", x6);

    var x7 = ModuleUtils.findAssignment(foo, "x6");
    assertAtomType("local.Project1.Mod_A.E", x7);
  }

  @Test
  public void missingFunctionArgumentWarning() throws Exception {
    final URI uri = new URI("memory://missingFunctionArgumentWarning.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                my_function a b c = [a, b, c]
                foo =
                    # This call has no effect as the result is discarded but no function was called either.
                    my_function 1
                    0
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = ModuleUtils.findStaticMethod(module, "foo");

    var allDiagnostics = ModuleUtils.getDescendantsDiagnostics(foo);
    Optional<Diagnostic> diagnostic =
        allDiagnostics.stream().filter(diag -> diag instanceof Warning.DiscardedValue).findFirst();
    assertTrue(
        "The DiscardedWarning should be found among " + allDiagnostics, diagnostic.isPresent());
    Warning.DiscardedValue discardedWarning = (Warning.DiscardedValue) diagnostic.get();
    assertEquals("Any -> (Any -> Any)", discardedWarning.discardedType());
  }

  @Test
  public void staticTypeCheckerReportsWarningsOnProject() throws IOException {
    var mainSrc =
        """
        bar =
            1.non_existent_method

        main =
            42
        """;
    Path projDir = Files.createTempDirectory("enso-tests");
    ProjectUtils.createProject("Proj", mainSrc, projDir);
    var out = new ByteArrayOutputStream();
    var ctxBuilder =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "true")
                        .option(RuntimeOptions.ENABLE_STATIC_ANALYSIS, "true")
                        .option(RuntimeOptions.STRICT_ERRORS, "true")
                        .currentWorkingDirectory(projDir.getParent())
                        .out(out)
                        .err(out)
                        .logHandler(out));
    ProjectUtils.testProjectRun(
        ctxBuilder,
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(42));
          assertThat(
              out.toString(),
              containsString(
                  "Calling member method `non_existent_method` on type Integer will result in a"
                      + " No_Such_Method error"));
        });
  }

  private TypeRepresentation getInferredType(IR ir) {
    var option = getInferredTypeOption(ir);
    assertTrue(
        "Expecting " + ir.showCode() + " to contain an inferred type within metadata.",
        option.isPresent());
    return option.get();
  }

  private Optional<TypeRepresentation> getInferredTypeOption(IR ir) {
    Option<ProcessingPass.Metadata> metadata = ir.passData().get(TypeInferencePropagation.INSTANCE);
    if (metadata.isEmpty()) {
      return Optional.empty();
    } else {
      InferredType inferred = (InferredType) metadata.get();
      return Optional.of(inferred.type());
    }
  }

  private void assertNoInferredType(IR ir) {
    Option<ProcessingPass.Metadata> metadata = ir.passData().get(TypeInferencePropagation.INSTANCE);
    assertTrue(
        "Expecting "
            + ir.showCode()
            + " to contain no inferred type within metadata, but it has "
            + metadata,
        metadata.isEmpty());
  }

  private void assertAtomType(String fqn, IR ir) {
    var option = getInferredTypeOption(ir);
    if (option.isEmpty()) {
      fail(
          "Expected "
              + ir.showCode()
              + " to have Atom type "
              + fqn
              + ", but no type metadata was found.");
    }

    var type = option.get();
    if (type instanceof TypeRepresentation.AtomType atomType) {
      assertEquals(
          "Expected " + ir.showCode() + " to have the right atom type: ",
          fqn,
          atomType.fqn().toString());
    } else {
      fail("Expected " + ir.showCode() + " to have an Atom type " + fqn + ", but got " + type);
    }
  }

  private void assertSumType(IR ir, String... shortNames) {
    var type = getInferredType(ir);
    if (type instanceof TypeRepresentation.SumType sumType) {
      var gotSet =
          new HashSet<>(sumType.types().stream().map(TypeRepresentation::toString).toList());
      assertEquals(Set.of(shortNames), gotSet);
    } else {
      fail("Expected " + ir.showCode() + " to have a SumType, but got " + type);
    }
  }

  private void assertTypeMismatch(IR ir, String expectedType, String gotType) {
    var diagnostics = ModuleUtils.getDescendantsDiagnostics(ir);
    assertThat("exactly 1 diagnostic expected but got " + diagnostics, diagnostics.size() == 1);

    var diagnostic = diagnostics.get(0);
    if (!(diagnostic instanceof Warning.TypeMismatch typeMismatch)) {
      throw new AssertionError("Expected Warning.TypeMismatch but got " + diagnostic);
    }

    assertEquals(expectedType, typeMismatch.expectedType());
    assertEquals(gotType, typeMismatch.actualType());
  }
}
