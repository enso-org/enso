package org.enso.interpreter.node.expression.builtin.error;

import static org.junit.Assert.assertEquals;

import com.oracle.truffle.api.interop.InteropLibrary;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class PanicExceptionTest {

  private static final InteropLibrary interop = InteropLibrary.getUncached();

  private static Context context;
  private static CatchPanicNode catchPanicNode;
  private static HostValueToEnsoNode hostValueToEnsoNode;
  private static TestRootNode testRootNode;

  @BeforeClass
  public static void initContextAndData() {
    context = ContextUtils.createDefaultContext();
    ContextUtils.executeInContext(
        context,
        () -> {
          catchPanicNode = CatchPanicNode.build();
          hostValueToEnsoNode = HostValueToEnsoNode.build();
          testRootNode = new TestRootNode();
          testRootNode.insertChildren(catchPanicNode, hostValueToEnsoNode);
          return null;
        });
  }

  @AfterClass
  public static void disposeContext() {
    context.close();
    context = null;
  }

  @Test
  public void panicExceptionMessageForAssertionError() throws Exception {
    ContextUtils.executeInContext(
        context,
        () -> {
          var text = Text.create("Some text for the exception");
          var thrown = new java.lang.AssertionError(text.toString());
          var ex = new PanicException(text, thrown, null);
          assertEquals(text.toString(), ex.getMessage());
          var msg = InteropLibrary.getUncached().getExceptionMessage(ex);
          assertEquals(text, msg);
          return null;
        });
  }
}
