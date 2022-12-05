package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@NodeInfo(
    shortName = "get_cons",
    description = "A base for auto-generated module-level atom constructor getters.")
public class QualifiedAccessorNode extends RootNode {
  private final AtomConstructor atomConstructor;

  /**
   * Creates a new instance of this node.
   *
   * @param language the current language instance.
   * @param atomConstructor the constructor to return.
   */
  public QualifiedAccessorNode(TruffleLanguage<?> language, AtomConstructor atomConstructor) {
    super(language);
    this.atomConstructor = atomConstructor;
  }

  /**
   * Executes the node, returning the predefined constructor.
   *
   * @param frame current execution frame
   * @return the constant constructor
   */
  public Object execute(VirtualFrame frame) {
    if (atomConstructor.getArity() == 0) {
      return atomConstructor.newInstance();
    } else {
      return atomConstructor;
    }
  }
}
