package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

public class GetFieldNode extends RootNode {
  private final int index;

  public GetFieldNode(TruffleLanguage<?> language, int index) {
    super(language);
    this.index = index;
  }

  public Stateful execute(VirtualFrame frame) {
    Atom atom = (Atom) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, atom.getFields()[index]);
  }
}
