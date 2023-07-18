package org.enso.interpreter.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.junit.Test;

public class ResourceManagerTest extends TestBase {

  public ResourceManagerTest() {}

  @Test
  public void runFinalizersAtTheEnd() {
    var obj = new ResourceToGc();
    var fn = new FnCallback();

    try (var ctx = createDefaultContext()) {
      ctx.enter();
      var ensoContext =
          (EnsoContext)
              ctx.getBindings(LanguageInfo.ID)
                  .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                  .asHostObject();

      ensoContext.getResourceManager().register(obj, fn);
      assertNull("Not invoked yet", fn.args);
    }

    assertNotNull("Callback invoked when thread closed", fn.args);
    assertNotNull("Callback invoked with one argument", fn.args.length);
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
