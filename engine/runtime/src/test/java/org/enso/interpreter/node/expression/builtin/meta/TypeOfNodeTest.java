package org.enso.interpreter.node.expression.builtin.meta;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.TruffleObject;
import java.lang.System.Logger.Level;
import java.nio.file.Paths;
import java.util.concurrent.Callable;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class TypeOfNodeTest {

  private static Context ctx;

  @BeforeClass
  public static void initCtx() throws Exception {
    ctx = defaultContextBuilder().build();
  }

  @AfterClass
  public static void disposeCtx() throws Exception {
    ctx.close();
  }

  @Test
  public void typeOfUnknownSymbol() throws Exception {
    assertType(UnresolvedSymbol.build("unknown_name", null), "Function", false);
  }

  @Test
  public void primeThenTypeUnknownSymbol() throws Exception {
    assertType(UnresolvedSymbol.build("unknown_name", null), "Function", true);
  }

  @Test
  public void typeOfUnknownConstructor() throws Exception {
    assertType(UnresolvedConstructor.build("Unknown_Name"), "Function", false);
  }

  @Test
  public void primeThenTypeUnknownConstructor() throws Exception {
    assertType(UnresolvedConstructor.build("Unknown_Name"), "Function", true);
  }

  private void assertType(Object symbol, String expectedTypeName, boolean withPriming) {
    executeInContext(
        ctx,
        () -> {
          var node = TypeOfNode.build();

          if (withPriming) {
            class ForeignObject implements TruffleObject {}
            var foreignType = node.execute(new ForeignObject());
            assertTrue(
                "Empty foreign is unknown: " + foreignType, foreignType instanceof DataflowError);
          }
          var symbolType = node.execute(symbol);
          var symbolTypeValue = ctx.asValue(symbolType);
          assertTrue("It is meta object: " + symbolTypeValue, symbolTypeValue.isMetaObject());
          assertEquals(expectedTypeName, symbolTypeValue.getMetaSimpleName());
          return null;
        });
  }

  private static Context.Builder defaultContextBuilder(String... languages) {
    return Context.newBuilder(languages)
        .allowExperimentalOptions(true)
        .allowIO(IOAccess.ALL)
        .allowAllAccess(true)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
        .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
        .logHandler(System.err)
        .option(RuntimeOptions.STRICT_ERRORS, "true")
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath());
  }

  private static Value executeInContext(Context ctx, Callable<Object> callable) {
    // Force initialization of the context
    ctx.eval("enso", "42");
    var err = new Exception[1];
    ctx.getPolyglotBindings()
        .putMember(
            "testSymbol",
            (ProxyExecutable)
                (Value... args) -> {
                  try {
                    return callable.call();
                  } catch (Exception e) {
                    err[0] = e;
                    return null;
                  }
                });
    var res = ctx.getPolyglotBindings().getMember("testSymbol").execute();
    if (err[0] != null) {
      throw raise(RuntimeException.class, err[0]);
    }
    return res;
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
