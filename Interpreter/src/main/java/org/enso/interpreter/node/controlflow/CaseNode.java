package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.runtime.Atom;

public abstract class CaseNode extends Node {
  public void markTail() {}
  public abstract void execute(VirtualFrame frame, Atom target) throws UnexpectedResultException;
}
