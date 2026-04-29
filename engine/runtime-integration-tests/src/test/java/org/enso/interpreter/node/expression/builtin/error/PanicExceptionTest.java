package org.enso.interpreter.node.expression.builtin.error;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.io.PrintWriter;
import java.io.StringWriter;
import org.enso.interpreter.node.expression.foreign.HostValueToEnsoNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.PolyglotException;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class PanicExceptionTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();
  private static final InteropLibrary interop = InteropLibrary.getUncached();

  private static CatchPanicNode catchPanicNode;
  private static HostValueToEnsoNode hostValueToEnsoNode;
  private static TestRootNode testRootNode;

  @BeforeClass
  public static void initContextAndData() {
    catchPanicNode = CatchPanicNode.build();
    hostValueToEnsoNode = HostValueToEnsoNode.build();
    testRootNode = new TestRootNode();
    testRootNode.insertChildren(catchPanicNode, hostValueToEnsoNode);
  }

  @AfterClass
  public static void disposeContext() {
    catchPanicNode = null;
    hostValueToEnsoNode = null;
    testRootNode = null;
  }

  @Test
  public void panicExceptionMessageForAssertionError() throws UnsupportedMessageException {
    var leak = ctxRule.ensoContext();
    var text = Text.create("Some text for the exception");
    var thrown = new java.lang.AssertionError(text.toString());
    var ex = new PanicException(leak, text, thrown, null);
    assertEquals(text.toString(), ex.getMessage());
    var msg = InteropLibrary.getUncached().getExceptionMessage(ex);
    assertEquals(text, msg);
  }

  @Test
  public void panicExceptionGetStackTrace() throws UnsupportedMessageException {
    var fn =
        ctxRule.evalModule(
            """
            from Standard.Base import all

            throw n =
                if n == 0 then
                    Panic.throw "now"
                else
                    x = throw n-1
                    x+1
            """,
            "throw.enso",
            "throw");

    try {
      var none = fn.execute(10);
      fail("Not expecting any result: " + none);
    } catch (PolyglotException ex) {
      var panic = (PanicException) ctxRule.unwrapValue(ex.getGuestObject());
      assertStackTraces(10, ex, panic);
    }
  }

  private void assertStackTraces(int depth, Exception first, Exception other) {
    var sw = new StringWriter();
    var pw = new PrintWriter(sw);
    first.printStackTrace(pw);
    other.printStackTrace(pw);
    pw.close();
    for (var i = 0; i < depth; i++) {
      var polyElem = first.getStackTrace()[i];
      var truffleElem = other.getStackTrace()[i];
      assertEquals(sw.toString(), polyElem, truffleElem);
    }
  }
}
