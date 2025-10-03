package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import org.enso.interpreter.Constants.Names;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public final class MethodResolutionTest {
  private static MethodResolverNode methodResolverNode;
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @BeforeClass
  public static void initCtx() {
    methodResolverNode = MethodResolverNode.getUncached();
  }

  @AfterClass
  public static void disposeCtx() {
    methodResolverNode = null;
  }

  @Test
  public void resolveStaticMethodFromAny() {
    var myTypeVal =
        ctxRule.evalModule(
            """
            from Standard.Base import all

            type My_Type
                method self = 42

            main = My_Type
            """);
    var myType = unwrapType(myTypeVal);
    var symbol = UnresolvedSymbol.build("to_display_text", myType.getDefinitionScope());
    var func = methodResolverNode.executeResolution(myType, symbol);
    assertThat("to_display_text method is found", func, is(notNullValue()));
    assertSingleSelfArgument(func);
  }

  @Test
  public void resolveInstanceMethodFromMyType() {
    var myTypeVal =
        ctxRule.evalModule(
            """
            type My_Type
                method self = 42

            main = My_Type
            """,
            "Module",
            "main");
    var myType = unwrapType(myTypeVal);
    var symbol = UnresolvedSymbol.build("method", myType.getDefinitionScope());
    var func = methodResolverNode.executeResolution(myType, symbol);
    assertThat("method is found", func, is(notNullValue()));
    assertSingleSelfArgument(func);
  }

  @Test
  public void resolveStaticMethodFromMyType() {
    var myTypeVal =
        ctxRule.evalModule(
            """
            type My_Type
                method = 42

            main = My_Type
            """,
            "Module",
            "main");
    var myType = unwrapType(myTypeVal);
    var symbol = UnresolvedSymbol.build("method", myType.getDefinitionScope());
    var func = methodResolverNode.executeResolution(myType, symbol);
    assertThat("method is found", func, is(notNullValue()));
    assertSingleSelfArgument(func);
  }

  @Test
  public void resolveExtensionMethodFromMyType() {
    var myTypeVal =
        ctxRule.evalModule(
            """
            type My_Type
            My_Type.method = 42

            main = My_Type
            """,
            "Module",
            "main");
    var myType = unwrapType(myTypeVal);
    var symbol = UnresolvedSymbol.build("method", myType.getDefinitionScope());
    var func = methodResolverNode.executeResolution(myType, symbol);
    assertThat("method is found", func, is(notNullValue()));
    assertSingleSelfArgument(func);
  }

  private void assertSingleSelfArgument(Function func) {
    assertThat("Has single self argument", func.getSchema().getArgumentsCount(), is(1));
    assertThat(
        "Has single self argument",
        func.getSchema().getArgumentInfos()[0].getName(),
        is(Names.SELF_ARGUMENT));
  }

  private Type unwrapType(Value val) {
    var unwrapped = ctxRule.unwrapValue(val);
    return (Type) unwrapped;
  }
}
