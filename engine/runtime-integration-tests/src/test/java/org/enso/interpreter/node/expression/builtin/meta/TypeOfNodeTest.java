package org.enso.interpreter.node.expression.builtin.meta;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.test.TestBase;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class TypeOfNodeTest extends TestBase {

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
}
