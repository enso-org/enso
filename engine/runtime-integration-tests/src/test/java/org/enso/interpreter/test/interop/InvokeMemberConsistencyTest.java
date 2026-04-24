package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.util.ArrayList;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class InvokeMemberConsistencyTest {
  @ClassRule
  public static final ContextUtils CTX = ContextUtils.newBuilder().assertGC(false).build();

  @Parameterized.Parameter(0)
  public Object raw;

  @Parameterized.Parameters
  public static Object[][] allPossibleEnsoInterpreterValues() throws Exception {
    CTX.context().enter();
    var g = ValuesGenerator.create(CTX);
    var data = new ArrayList<Object[]>();
    for (var value : g.allValues()) {
      var raw = CTX.unwrapValue(value);
      if (raw instanceof TruffleObject) {
        data.add(new Object[] {raw});
      }
    }
    CTX.context().leave();
    return data.toArray(new Object[0][]);
  }

  @Test
  public void unknownIdentifierUncached() {
    assertInteropInvoke("Uncached version", raw, InteropLibrary.getUncached());
  }

  @Test
  public void unknownIdentifierCached() {

    var trn = new TestRootNode();
    var n = InteropLibrary.getFactory().createDispatched(10);
    n = trn.insert(n);

    assertInteropInvoke("Cached version", raw, n);
  }

  private static void assertInteropInvoke(String msg, Object raw, InteropLibrary node) {
    try {
      var res = node.invokeMember(raw, "unknownIdentifier");
      fail("Expecting exception, now a result: " + res);
    } catch (UnknownIdentifierException good) {
      assertEquals(
          "UnknownIdentifierException is expected",
          "unknownIdentifier",
          good.getUnknownIdentifier());
    } catch (UnsupportedMessageException ex) {
      // that's OK
    } catch (Exception ex) {
      throw new AssertionError(msg + " got " + ex.getClass().getName() + " for " + raw, ex);
    }
  }
}
