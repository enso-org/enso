package org.enso.interpreter.dsl.test;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "InliningBuiltins", name = "in")
final class InliningBuiltinsInNode extends Node {

  long execute(long a, long b) {
    return a + b;
  }
}
