package org.enso.interpreter.node.expression.builtin.error;

import static org.junit.Assert.assertEquals;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import org.enso.interpreter.node.expression.foreign.HostValueToEnsoNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
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
}
