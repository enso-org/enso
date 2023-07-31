package org.enso.interpreter.node.callable;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.math.BigInteger;
import org.enso.interpreter.node.callable.resolver.HostMethodCallNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.test.TestBase;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class PolyglotCallTypeTest extends TestBase {

  private static Context ctx;

  public PolyglotCallTypeTest() {}

  @BeforeClass
  public static void setupCtx() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void closeCtx() {
    ctx.close();
  }

  @Test
  public void javaBigIntegerDispatch() {
    var big = new BigInteger("4324908174321000432143143778956741");
    var val = unwrapValue(ctx, ctx.asValue(big));
    var sym = UnresolvedSymbol.build("+", null);
    var typ = HostMethodCallNode.getPolyglotCallType(val, sym, InteropLibrary.getUncached());
    assertEquals(HostMethodCallNode.PolyglotCallType.CONVERT_TO_BIG_INT, typ);
    assertFalse(typ.isInteropLibrary());
  }

  @Test
  public void textDispatch() {
    var val = "a text";
    var sym = UnresolvedSymbol.build("+", null);
    var typ = HostMethodCallNode.getPolyglotCallType(val, sym, InteropLibrary.getUncached());
    assertEquals(HostMethodCallNode.PolyglotCallType.CONVERT_TO_TEXT, typ);
    assertFalse(typ.isInteropLibrary());
  }

  @Test
  public void longDispatch() {
    var val = 4L;
    var sym = UnresolvedSymbol.build("+", null);
    var typ = HostMethodCallNode.getPolyglotCallType(val, sym, InteropLibrary.getUncached());
    assertEquals(HostMethodCallNode.PolyglotCallType.CONVERT_TO_BIG_INT, typ);
    assertFalse(typ.isInteropLibrary());
  }
}
