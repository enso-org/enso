package org.enso.interpreter.dsl.test;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.junit.Assert;

@BuiltinMethod(type = "InliningBuiltins", name = "out")
final class InliningBuiltinsOutNode extends Node {

  long execute(VirtualFrame frame, long a, long b) {
    Assert.assertNotNull("VirtualFrame is always provided " + frame);
    return a + b;
  }

}
