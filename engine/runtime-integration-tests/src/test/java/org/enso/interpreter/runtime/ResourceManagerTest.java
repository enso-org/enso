package org.enso.interpreter.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.test.utils.ContextUtils;
import org.enso.testkit.ReportLogsOnFailureRule;
import org.junit.Rule;
import org.junit.Test;

public class ResourceManagerTest {

  public ResourceManagerTest() {}

  @Rule(order = Integer.MIN_VALUE)
  public ReportLogsOnFailureRule appenderRule = new ReportLogsOnFailureRule();

  @Test
  public void runFinalizersAtTheEnd() {
    var obj = new ResourceToGc();
    var fn = new FnCallback();

    try (var ctx = ContextUtils.createWithDefaultLogLevel()) {
      var ensoContext = ctx.ensoContext();

      ensoContext.getResourceManager().register(obj, fn);
      assertNull("Not invoked yet", fn.args);
    }

    assertNotNull("Callback invoked when thread closed", fn.args);
    assertEquals("Callback invoked with one argument", fn.args.length, 1);
    assertEquals("Called by our thread", fn.thread, Thread.currentThread());
    assertEquals("Callback called for the registered `obj`", obj, fn.args[0]);
  }

  final class ResourceToGc implements TruffleObject {}

  @ExportLibrary(InteropLibrary.class)
  final class FnCallback implements TruffleObject {
    Object[] args;
    Thread thread;

    @ExportMessage
    Object execute(Object[] args) {
      this.args = args;
      this.thread = Thread.currentThread();
      return this;
    }

    @ExportMessage
    boolean isExecutable() {
      return true;
    }
  }
}
