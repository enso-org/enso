package org.enso.interpreter.node.expression.builtin.meta;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class TypeChainTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();
  private static Value typeOf;

  @BeforeClass
  public static void initTypeOf() {
    typeOf =
        ctx.evalModule(
            """
            import Standard.Base.Meta

            main = Meta.type_of
            """);
  }

  @Test
  public void textChain() {
    var type = typeOf.execute("Text");
    var raw = (Type) ctx.unwrapValue(type);
    var all = raw.allTypes(ctx.ensoContext());

    var exp1 = ctx.ensoContext().getBuiltins().text();
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals("Text type and Any", new Object[] {exp1, exp2}, all);
  }

  @Test
  public void textTypeChain() {
    var textType = typeOf.execute("Text");
    var textTypeType = typeOf.execute(textType);
    var raw = (Type) ctx.unwrapValue(textTypeType);
    var all = raw.allTypes(ctx.ensoContext());

    var exp1 = ctx.ensoContext().getBuiltins().text().getEigentype();
    var exp2 = ctx.ensoContext().getBuiltins().any();
    assertArrayEquals("Text.type and Any", new Object[] {exp1, exp2}, all);
  }

  @Test
  public void textEigeintypeChain() {
    var textType = typeOf.execute("Text");
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
}
