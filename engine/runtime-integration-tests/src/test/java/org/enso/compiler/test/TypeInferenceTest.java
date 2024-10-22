package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeInferencePropagation;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
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
    Method foo = findStaticMethod(module, "foo");
    assertAtomType("zeroAryModuleMethodCheck.My_Type", findAssignment(foo.body(), "x"));
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
    var foo = findStaticMethod(module, "foo");
    String myType = "functionReturnCheck.My_Type";

    // The result of `add a z` should be `My_Type` as guaranteed by the return type check of `add`.
    assertAtomType(myType, findAssignment(foo.body(), "b"));
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

    var f1 = findStaticMethod(module, "f1");
    var f2 = findStaticMethod(module, "f2");
    var f3 = findStaticMethod(module, "f3");

    assertAtomType(myType, findAssignment(f1, "y1"));
    assertNoInferredType(findAssignment(f2, "y2"));

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
    Method f = findStaticMethod(module, "f");

    String myType = "ascribedExpressions.My_Type";
    assertAtomType(myType, findAssignment(f.body(), "y"));
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
    Method f = findStaticMethod(module, "f");

    var y1Type = getInferredType(findAssignment(f.body(), "y1"));
    if (y1Type instanceof TypeRepresentation.SumType sumType) {
      var gotSet =
          new HashSet<>(sumType.types().stream().map(TypeRepresentation::toString).toList());
      assertEquals(Set.of("My_Type", "Other_Type"), gotSet);
    } else {
      fail("y1 should be a sum type, but got " + y1Type);
    }

    var y2Type = getInferredType(findAssignment(f.body(), "y2"));
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
    Method f = findStaticMethod(module, "f");

    // Here we will only know that both f1 and f2 are Any -> Any - because the ascribed check only
    // really performs a
    // `is_a Function` check, we do not know anything about the argument nor return type of this
    // function,
    // unfortunately.
    TypeRepresentation primitiveFunctionType =
        new TypeRepresentation.ArrowType(TypeRepresentation.ANY, TypeRepresentation.ANY);
    assertEquals(primitiveFunctionType, getInferredType(findAssignment(f.body(), "f1")));
    assertEquals(primitiveFunctionType, getInferredType(findAssignment(f.body(), "f2")));
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
    Method f = findStaticMethod(module, "f");

    assertAtomType("Standard.Base.Data.Numbers.Integer", findAssignment(f, "x"));
    assertAtomType("Standard.Base.Data.Text.Text", findAssignment(f, "y"));
    assertAtomType("Standard.Base.Data.Numbers.Float", findAssignment(f, "z"));
    assertAtomType("Standard.Base.Data.Vector.Vector", findAssignment(f, "w"));
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
    var foo = findStaticMethod(module, "foo");

    var myType = "bindingsFlow.My_Type";

    assertAtomType(myType, findAssignment(foo, "w"));
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
    var foo = findStaticMethod(module, "foo");

    var myType = "checkedArgumentTypes.My_Type";

    // Type from argument
    assertAtomType(myType, findAssignment(foo, "y1"));

    // No type
    assertNoInferredType(findAssignment(foo, "y2"));
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
    var foo = findStaticMethod(module, "foo");

    var f1Type = getInferredType(findAssignment(foo, "f1"));
    assertEquals("My_Type -> (My_Type -> My_Type)", f1Type.toString());

    // and result of application is typed as the return type:
    assertAtomType("innerFunctionType.My_Type", findAssignment(foo, "y"));
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
    var foo = findStaticMethod(module, "foo");

    var myType = "zeroArgConstructor.My_Type";
    assertAtomType(myType, findAssignment(foo, "x"));
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
    var foo = findStaticMethod(module, "foo");

    var myType = "multiArgConstructor.My_Type";
    assertAtomType(myType, findAssignment(foo, "x"));
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
    var foo = findStaticMethod(module, "foo");

    var myType = "constructorWithDefaults.My_Type";

    // The commented out expressions document the desired behaviour - we correctly infer which
    // arguments were defaulted.
    // Before that is working, we just ensure we did not infer any 'unexpected' type for the
    // results.
    // assertAtomType(myType, findAssignment(foo, "x1"));
    assertNoInferredType(findAssignment(foo, "x1"));

    // assertAtomType(myType, findAssignment(foo, "x2"));
    assertNoInferredType(findAssignment(foo, "x2"));

    // assertAtomType(myType, findAssignment(foo, "x3"));
    assertNoInferredType(findAssignment(foo, "x3"));

    assertNotEquals(Optional.of(myType), getInferredTypeOption(findAssignment(foo, "x4")));
    assertNotEquals(Optional.of(myType), getInferredTypeOption(findAssignment(foo, "x5")));

    // assertAtomType(myType, findAssignment(foo, "x6"));
    assertNoInferredType(findAssignment(foo, "x6"));

    assertNotEquals(Optional.of(myType), getInferredTypeOption(findAssignment(foo, "x7")));
  }

  @Ignore("TODO: ifte")
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
    var f = findStaticMethod(module, "f");
    assertAtomType("Standard.Base.Data.Numbers.Integer", findAssignment(f, "y"));
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
    var f = findStaticMethod(module, "f");
    assertAtomType(myType, findAssignment(f, "y"));
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
    var f = findStaticMethod(module, "f");
    assertAtomType(myType, findAssignment(f, "y"));
  }

  /**
   * This is more complex than inferBoundsFromCaseAlias, as it needs to add a type constraint only
   * in one branch. We will need to ensure that we duplicate the local scopes in each branch to
   * avoid bad sharing.
   */
  @Ignore("TODO")
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
    var f = findStaticMethod(module, "f");
    assertAtomType(myType, findAssignment(f, "y"));
  }

  @Ignore("TODO")
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
    var f = findStaticMethod(module, "f");
    assertSumType(findAssignment(f, "y"), "Integer", "Text");
  }

  @Ignore("TODO")
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
    var f = findStaticMethod(module, "f");
    assertAtomType(myType, findAssignment(f, "y"));
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
    var f = findStaticMethod(module, "f");
    assertSumType(findAssignment(f, "y"), "My_Type", "Other_Type");
  }

  @Ignore
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
    var f = findStaticMethod(module, "f");
    assertSumType(findAssignment(f, "y"), "Text", "Integer");
  }

  @Ignore
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
    var f = findStaticMethod(module, "f");
    assertSumType(findAssignment(f, "y"), "Text", "Nothing");
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

    var staticMethod = findMemberMethod(module, "My_Type", "static_method");
    assertAtomType(myType, findAssignment(staticMethod, "y"));
    assertAtomType(myType, findAssignment(staticMethod, "z"));
    assertAtomType("Standard.Base.Data.Numbers.Integer", findAssignment(staticMethod, "w"));

    var memberMethod = findMemberMethod(module, "My_Type", "member_method");
    assertAtomType(myType, findAssignment(memberMethod, "y"));
    assertAtomType(myType, findAssignment(memberMethod, "z"));
    assertAtomType("Standard.Base.Data.Numbers.Integer", findAssignment(memberMethod, "w"));
  }

  @Ignore("TODO")
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
    var f = findMemberMethod(module, "My_Type", "member_method");
    var myType = "typeInferenceOfSelf.My_Type";
    assertAtomType(myType, findAssignment(f, "y"));
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
    var foo = findStaticMethod(module, "foo");

    var x1 = findAssignment(foo, "x1");
    assertEquals(
        List.of(new Warning.NotInvokable(x1.expression().identifiedLocation(), "Integer")),
        getImmediateDiagnostics(x1.expression()));

    var x2 = findAssignment(foo, "x2");
    assertEquals(
        List.of(new Warning.NotInvokable(x2.expression().identifiedLocation(), "Text")),
        getImmediateDiagnostics(x2.expression()));

    var x3 = findAssignment(foo, "x3");
    assertEquals(
        "x3 should not contain any warnings",
        List.of(),
        getDescendantsDiagnostics(x3.expression()));
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
    assertEquals(List.of(), getDescendantsDiagnostics(module));
  }

  @Ignore("We cannot report type errors until we check there are no Conversions")
  @Test
  public void typeErrorFromAscription() throws Exception {
    final URI uri = new URI("memory://typeErrorFromAscription.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    type My_Type
                        Value v
                    type Other_Type
                        Value o
                    foo =
                        x = My_Type.Value 12
                        y = (x : Other_Type)
                        y
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var y = findAssignment(foo, "y");
    var typeError =
        new Warning.TypeMismatch(y.expression().identifiedLocation(), "Other_Type", "My_Type");
    assertEquals(List.of(typeError), getDescendantsDiagnostics(y.expression()));
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
                    Other_type.from (that : My_Type) = Other_Type.Value that.v+1000
                    foo =
                        x = My_Type.Value 12
                        y = (x : Other_Type)
                        y
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var y = findAssignment(foo, "y");
    assertEquals(
        "valid conversion should ensure there is no type error",
        List.of(),
        getDescendantsDiagnostics(y.expression()));
  }

  @Ignore("We cannot report type errors until we check there are no Conversions")
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
                        y = (f : My_Type)
                        g (x : My_Type) -> My_Type = x
                        z = (g : My_Type)
                        [y, z]
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var y = findAssignment(foo, "y");
    var typeError1 =
        new Warning.TypeMismatch(y.expression().identifiedLocation(), "My_Type", "(Any -> Any)");
    assertEquals(List.of(typeError1), getDescendantsDiagnostics(y.expression()));

    var z = findAssignment(foo, "z");
    var typeError2 =
        new Warning.TypeMismatch(
            z.expression().identifiedLocation(), "My_Type", "My_Type -> My_Type");
    assertEquals(List.of(typeError2), getDescendantsDiagnostics(z.expression()));
  }

  @Ignore("We cannot report type errors until we check there are no Conversions")
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
    var foo = findStaticMethod(module, "foo");

    var z = findAssignment(foo, "z");
    var arg =
        switch (z.expression()) {
          case Application.Prefix app -> app.arguments().head();
          default -> throw new AssertionError(
              "Expected " + z.showCode() + " to be an application expression.");
        };
    var typeError = new Warning.TypeMismatch(arg.identifiedLocation(), "Other_Type", "My_Type");
    assertEquals(List.of(typeError), getImmediateDiagnostics(arg));
  }

  @Ignore("We cannot report type errors until we check there are no Conversions")
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
    var foo = findStaticMethod(module, "foo");

    var x = findAssignment(foo, "x");
    var typeError =
        new Warning.TypeMismatch(x.expression().identifiedLocation(), "My_Type", "Integer");
    assertEquals(List.of(typeError), getDescendantsDiagnostics(x.expression()));
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
    var foo = findStaticMethod(module, "foo");

    var y = findAssignment(foo, "y");
    assertEquals(
        "y should not contain any warnings", List.of(), getDescendantsDiagnostics(y.expression()));

    var z = findAssignment(foo, "z");
    assertEquals(
        "z should not contain any warnings", List.of(), getDescendantsDiagnostics(z.expression()));

    var baz = findAssignment(foo, "baz");
    assertEquals(
        "baz should not contain any warnings",
        List.of(),
        getDescendantsDiagnostics(baz.expression()));
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
    var foo = findStaticMethod(module, "foo");

    var myType = "globalMethodTypes.My_Type";

    assertAtomType(myType, findAssignment(foo, "x1"));
    assertEquals("My_Type -> My_Type", getInferredType(findAssignment(foo, "x2")).toString());
    assertAtomType(myType, findAssignment(foo, "x3"));
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

                        # And calling member methods through static syntax:
                        x5 = My_Type.zero_arg inst
                        x6 = My_Type.one_arg inst

                        # And extension methods
                        x7 = inst.extension_method
                        x8 = My_Type.extension_method inst
                        [x1, x2, x3, x4, x5, x6, x7, x8]
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var myType = "memberMethodCalls.My_Type";

    assertAtomType(myType, findAssignment(foo, "inst"));
    assertAtomType(myType, findAssignment(foo, "x1"));
    assertAtomType(myType, findAssignment(foo, "x2"));
    assertAtomType(myType, findAssignment(foo, "x3"));
    assertAtomType(myType, findAssignment(foo, "x4"));
    assertAtomType(myType, findAssignment(foo, "x5"));

    // The function in x6 was not fully applied - still expecting 1 arg:
    assertEquals("My_Type -> My_Type", getInferredType(findAssignment(foo, "x6")).toString());

    assertAtomType(myType, findAssignment(foo, "x7"));
    assertAtomType(myType, findAssignment(foo, "x8"));
  }

  @Ignore(
      "TODO: error can only be reported when we can rule out there is no Other_Type -> My_Type"
          + " conversion")
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
                x1 = My_Type.member_method other
                x1
            """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var x1 = findAssignment(foo, "x1");
    var typeError =
        new Warning.TypeMismatch(x1.expression().identifiedLocation(), "My_Type", "Other_Type");
    assertEquals(List.of(typeError), getDescendantsDiagnostics(x1.expression()));
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
    var foo = findStaticMethod(module, "foo");

    assertAtomType("callingFieldGetters.Typ_X", findAssignment(foo, "x_a"));
    // We don't know which constructor was used, so if the field appears in many constructors, it
    // resolves to a sum type
    assertSumType(findAssignment(foo, "x_b"), "Typ_Y", "Typ_Z");

    // We have two constructors with field `field_c`, but they have the same type so the sum type
    // should have been simplified
    assertAtomType("callingFieldGetters.Typ_Z", findAssignment(foo, "x_c"));

    // We shouldn't get a No_Such_Method error on a field with no type ascription:
    var x_d = findAssignment(foo, "x_d");
    assertEquals(
        "Field access should not yield any warnings", List.of(), getDescendantsDiagnostics(x_d));
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
    var foo = findStaticMethod(module, "foo");
    var x1 = findAssignment(foo, "x1");
    var x2 = findAssignment(foo, "x2");
    var x3 = findAssignment(foo, "x3");
    var x4 = findAssignment(foo, "x4");
    var x5 = findAssignment(foo, "x5");

    // member method is defined
    assertEquals(List.of(), getDescendantsDiagnostics(x1.expression()));

    // this method is not found
    assertEquals(
        List.of(
            new Warning.NoSuchMethod(
                x2.expression().identifiedLocation(),
                "member method `method_two` on type My_Type")),
        getImmediateDiagnostics(x2.expression()));

    // delegating to Any
    assertEquals(List.of(), getDescendantsDiagnostics(x3.expression()));
    assertEquals(List.of(), getDescendantsDiagnostics(x4.expression()));

    // calling a static method on an instance _does not work_, so we get a warning telling there's
    // no such _member_ method
    assertEquals(
        List.of(
            new Warning.NoSuchMethod(
                x5.expression().identifiedLocation(),
                "member method `static_method` on type My_Type")),
        getImmediateDiagnostics(x5.expression()));
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
    var foo = findStaticMethod(module, "foo");

    assertAtomType("Standard.Base.Data.Text.Text", findAssignment(foo, "txt1"));
    assertAtomType("Standard.Base.Data.Text.Text", findAssignment(foo, "txt2"));
    assertAtomType("Standard.Base.Data.Text.Text", findAssignment(foo, "txt3"));

    assertAtomType("Standard.Base.Data.Boolean.Boolean", findAssignment(foo, "bool"));
  }

  @Test
  public void callingExtensionMethodDefinedElsewhere() throws Exception {
    final URI uriA = new URI("memory://local.Project1.modA.enso");
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

    final URI uriB = new URI("memory://local.Project1.modB.enso");
    final Source srcB =
        Source.newBuilder(
                "enso",
                """
                    import local.Project1.modA.My_Type

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

    final URI uriC = new URI("memory://local.Project1.modC.enso");
    final Source srcC =
        Source.newBuilder(
                "enso",
                """
                    import local.Project1.modA.My_Type
                    from local.Project1.modB import all

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
    var foo = findStaticMethod(modC, "foo");

    assertAtomType("local.Project1.modB.Typ_X", findAssignment(foo, "x1"));
    assertAtomType("local.Project1.modB.Typ_Y", findAssignment(foo, "x2"));
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
}
