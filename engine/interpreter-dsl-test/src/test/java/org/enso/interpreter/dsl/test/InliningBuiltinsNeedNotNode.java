package org.enso.interpreter.dsl.test;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import static org.junit.Assert.assertNotNull;

@BuiltinMethod(type = "InliningBuiltins", name = "need_not", inlineable = true)
final class InliningBuiltinsNeedNotNode extends Node {

  long execute(VirtualFrame frame, long a, long b) {
    assertNotNull("Some frame is still provided", frame);
    return a + b;
  }
}
