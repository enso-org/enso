package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;

@GenerateInline(false)
abstract class ScalarOperationNode extends Node {
  abstract Object executeOp(Object a, Object b) throws UnsupportedMessageException;
}
