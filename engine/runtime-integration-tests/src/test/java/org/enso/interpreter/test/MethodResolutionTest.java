package org.enso.interpreter.test;

import static org.enso.test.utils.ContextUtils.createDefaultContext;
import static org.enso.test.utils.ContextUtils.evalModule;
import static org.enso.test.utils.ContextUtils.executeInContext;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import org.enso.interpreter.Constants.Names;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public final class MethodResolutionTest {
  private static MethodResolverNode methodResolverNode;
  private static Context ctx;

  @BeforeClass
  public static void initCtx() {
    ctx = createDefaultContext();
    executeInContext(
        ctx,
        () -> {
          methodResolverNode = MethodResolverNode.getUncached();
          return null;
        });
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
    methodResolverNode = null;
  }

  @Test
  public void resolveStaticMethodFromAny() {
    var myTypeVal =
        evalModule(
            ctx,
            """
        from Standard.Base import all

        type My_Type
            method self = 42

        main = My_Type
        """);
    executeInContext(
        ctx,
        () -> {
          var myType = unwrapType(myTypeVal);
          var symbol = UnresolvedSymbol.build("to_display_text", myType.getDefinitionScope());
          var func = methodResolverNode.executeResolution(myType, symbol);
          assertThat("to_display_text method is found", func, is(notNullValue()));
          assertSingleSelfArgument(func);
          return null;
        });
  }

  @Test
  public void resolveInstanceMethodFromMyType() {
    var myTypeVal =
        evalModule(
            ctx,
            """
        type My_Type
            method self = 42

        main = My_Type
        """,
            "Module",
            "main");
    executeInContext(
        ctx,
        () -> {
          var myType = unwrapType(myTypeVal);
          var symbol = UnresolvedSymbol.build("method", myType.getDefinitionScope());
          var func = methodResolverNode.executeResolution(myType, symbol);
          assertThat("method is found", func, is(notNullValue()));
          assertSingleSelfArgument(func);
          return null;
        });
  }

  @Test
  public void resolveStaticMethodFromMyType() {
    var myTypeVal =
        evalModule(
            ctx,
            """
        type My_Type
            method = 42

        main = My_Type
        """,
            "Module",
            "main");
    executeInContext(
        ctx,
        () -> {
          var myType = unwrapType(myTypeVal);
          var symbol = UnresolvedSymbol.build("method", myType.getDefinitionScope());
          var func = methodResolverNode.executeResolution(myType, symbol);
          assertThat("method is found", func, is(notNullValue()));
          assertSingleSelfArgument(func);
          return null;
        });
  }

  @Test
  public void resolveExtensionMethodFromMyType() {
    var myTypeVal =
        evalModule(
            ctx,
            """
        type My_Type
        My_Type.method = 42

        main = My_Type
        """,
            "Module",
            "main");
    executeInContext(
        ctx,
        () -> {
          var myType = unwrapType(myTypeVal);
          var symbol = UnresolvedSymbol.build("method", myType.getDefinitionScope());
          var func = methodResolverNode.executeResolution(myType, symbol);
          assertThat("method is found", func, is(notNullValue()));
          assertSingleSelfArgument(func);
          return null;
        });
  }

  private void assertSingleSelfArgument(Function func) {
    assertThat("Has single self argument", func.getSchema().getArgumentsCount(), is(1));
    assertThat(
        "Has single self argument",
        func.getSchema().getArgumentInfos()[0].getName(),
        is(Names.SELF_ARGUMENT));
  }

  private Type unwrapType(Value val) {
    var unwrapped = ContextUtils.unwrapValue(ctx, val);
    return (Type) unwrapped;
  }
}
