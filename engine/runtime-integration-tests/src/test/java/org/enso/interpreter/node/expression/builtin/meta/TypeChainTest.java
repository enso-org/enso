package org.enso.interpreter.node.expression.builtin.meta;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class TypeChainTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();
  private static Value typeOf;
  private static Value normalType;
  private static Value singletonType;

  @BeforeClass
  public static void initTypeOf() {
    typeOf =
        ctx.evalModule(
            """
            import Standard.Base.Meta

            main = Meta.type_of
            """);
    normalType =
        ctx.evalModule(
            """
            type Normal_Type
                Cons a

            main = Normal_Type
            """);
    singletonType =
        ctx.evalModule(
            """
            type Singleton_Type

            main = Singleton_Type
            """);
  }

  @AfterClass
  public static void releaseReferences() {
    typeOf = null;
    normalType = null;
    singletonType = null;
  }

  /** {@code allTypes(Text) == [Text, Any]} */
  @Test
  public void textChain() {
    var type = typeOf.execute("Hello World!");
    var raw = (Type) ctx.unwrapValue(type);
    var all = raw.allTypes(ctx.ensoContext());

    var exp1 = ctx.ensoContext().getBuiltins().text();
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals("allTypes(Text) == [Text, Any]", new Object[] {exp1, exp2}, all);
  }

  /** {@code allTypes(Text.type) == [Text.type, Any]} */
  @Test
  public void textTypeChain() {
    var textType = typeOf.execute("Ciao");
    var textTypeType = typeOf.execute(textType);
    var raw = (Type) ctx.unwrapValue(textTypeType);
    var all = raw.allTypes(ctx.ensoContext());

    var exp1 = ctx.ensoContext().getBuiltins().text().getEigentype();
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals("allTypes(Text.type) == [Text.type, Any]", new Object[] {exp1, exp2}, all);
  }

  /** {@code typeof(Text.type) == Text.type} */
  @Test
  public void textEigeintypeChain() {
    var textType = typeOf.execute("Ahoj");
    var textTypeType = typeOf.execute(textType);
    var loop = typeOf.execute(textTypeType);
    assertEquals("Eigentype is the last type - then we loop", textTypeType, loop);
  }

  @Test
  public void textModuleChain() {
    var code =
        """
        import Standard.Base.Data.Text
        main = Text
        """;
    var textModule = ctx.evalModule(code);
    assertEquals("Standard.Base.Data.Text", textModule.getMetaQualifiedName());

    var rawType = (Type) ctx.unwrapValue(textModule);
    var module = rawType.getDefinitionScope().getModule();
    var associatedType = rawType.getDefinitionScope().getAssociatedType();
    assertEquals("Module's type is its associated type", rawType, associatedType);
    assertTrue("Module associated type is eigentype", rawType.isEigenType());

    var exp1 = module.getScope().getAssociatedType();
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals(
        "Text.type and Any", new Object[] {exp1, exp2}, rawType.allTypes(ctx.ensoContext()));
  }

  /** {@code allTypes(Integer) == [Integer, Number, Any]} */
  @Test
  public void integerChain() {
    var numberType = ctx.ensoContext().getBuiltins().number().getNumber();
    var integerType = ctx.ensoContext().getBuiltins().number().getInteger();
    var anyType = ctx.ensoContext().getBuiltins().any();
    var allTypes = integerType.allTypes(ctx.ensoContext());
    assertArrayEquals(
        "allTypes(Integer) == [Integer, Number, Any]",
        new Object[] {integerType, numberType, anyType},
        allTypes);
  }

  /** {@code allTypes(Any) == [Any]} */
  @Test
  public void anyChain() {
    var any = ctx.ensoContext().getBuiltins().any();
    var all = any.allTypes(ctx.ensoContext());

    assertArrayEquals("allTypes(Any) == [Any]", new Object[] {any}, all);
  }

  /** {@code allTypes(Any.type) == [Any.type, Any]} */
  @Test
  public void anyEigentypeChain() {
    var any = ctx.ensoContext().getBuiltins().any();
    var anyType = typeOf.execute(any);
    assertEquals("Any.type", anyType.toString());
    var anyTypeType = typeOf.execute(anyType);
    assertEquals("Type of Any.type is again Any.type", anyType, anyTypeType);
    var raw = (Type) ctx.unwrapValue(anyTypeType);
    var all = raw.allTypes(ctx.ensoContext());

    var anyEigenType = any.getEigentype();
    var anyTypeExpected = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals(
        "allTypes(Any.type) == [Any.type, Any]", new Object[] {anyEigenType, anyTypeExpected}, all);
  }

  /** {@code allTypes(Error) == [Error, Any]} */
  @Test
  public void errorChain() {
    var errType = ctx.ensoContext().getBuiltins().dataflowError();
    var all = errType.allTypes(ctx.ensoContext());

    var exp1 = errType;
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals("allTypes(Error) == [Error, Any]", new Object[] {exp1, exp2}, all);
  }

  /** {@code allTypes(Normal_Type) == [Normal_Type, Any]} */
  @Test
  public void normalTypeChain() {
    var raw = (Type) ctx.unwrapValue(normalType);
    assertThat("Is not eigen type", raw.isEigenType(), is(false));
    var all = raw.allTypes(ctx.ensoContext());

    var exp1 = raw;
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals(
        "allTypes(Normal_Type) == [Normal_Type, Any]", new Object[] {exp1, exp2}, all);
  }

  /** {@code allTypes(Normal_Type.type) == [Normal_Type.type, Any]} */
  @Test
  public void normalEigenTypeChain() {
    var normalTypeType = typeOf.execute(normalType);
    var raw = (Type) ctx.unwrapValue(normalTypeType);
    assertThat("Is eigen type", raw.isEigenType(), is(true));
    var all = raw.allTypes(ctx.ensoContext());

    var exp1 = raw;
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals(
        "allTypes(Normal_Type.type) == [Normal_Type.type, Any]", new Object[] {exp1, exp2}, all);
  }

  @Test
  public void singletonTypeChain() {
    var raw = (Type) ctx.unwrapValue(singletonType);
    assertThat("Is eigen type", raw.isEigenType(), is(true));
    var all = raw.allTypes(ctx.ensoContext());

    var exp1 = raw;
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals(
        "allTypes(Singleton_Type.type) == [Singleton_Type.type, Any]",
        new Object[] {exp1, exp2},
        all);
  }
}
