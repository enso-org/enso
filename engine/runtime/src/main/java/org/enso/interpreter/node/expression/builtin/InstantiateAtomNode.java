package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

/** This node represents the process of instantiating an atom at runtime. */
@NodeInfo(shortName = "constructor::", description = "An atom instantiation at runtime.")
public class InstantiateAtomNode extends RootNode {
  private @Child ExpressionNode instantiator;
  private final String name;

  private InstantiateAtomNode(Language language, String name, ExpressionNode instantiator) {
    super(language);
    this.name = name;
    this.instantiator = instantiator;
  }

  /**
   * Executes this node.
   *
   * @param frame the language frame being executed
   * @return the result of executing this node
   */
  @Override
  public Stateful execute(VirtualFrame frame) {
    return new Stateful(
        Function.ArgumentsHelper.getState(frame.getArguments()),
        instantiator.executeGeneric(frame));
  }

  /**
   * Returns a string representation of this node.
   *
   * @return a string representation of this node
   */
  @Override
  public String getName() {
    return name;
  }

  /**
   * Creates an instance of this node.
   *
   * @param language the language for which the node is created
   * @param name the name of the atom being instantated
   * @param instantiator the expression used to instantiate the atom
   * @return an instance of this node
   */
  public static InstantiateAtomNode build(
      Language language, String name, ExpressionNode instantiator) {
    return new InstantiateAtomNode(language, name, instantiator);
  }
}
