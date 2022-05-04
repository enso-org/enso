package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** Represents a type constructor definition. */
@NodeInfo(shortName = "Cons", description = "Represents a constructor definition")
public abstract class ConstructorNode extends ExpressionNode {
  private final AtomConstructor constructor;

  ConstructorNode(AtomConstructor constructor) {
    this.constructor = constructor;
  }

  /**
   * Creates an instance of this node.
   *
   * @param constructor the atom constructor to represent
   * @return a truffle node representing {@code constructor}
   */
  public static ConstructorNode build(AtomConstructor constructor) {
    return ConstructorNodeGen.create(constructor);
  }

  /**
   * Executes the type constructor definition.
   *
   * @param frame the frame to execute in
   * @return the constructor of the type defined
   */
  @Specialization
  Object doExecute(VirtualFrame frame) {
    var builtins = Context.get(this).getBuiltins();
    if (constructor == builtins.bool().getTrue()) {
      return true;
    }
    if (constructor == builtins.bool().getFalse()) {
      return false;
    }
    if (constructor.getArity() == 0) {
      return constructor.newInstance();
    }
    return constructor;
  }
}
