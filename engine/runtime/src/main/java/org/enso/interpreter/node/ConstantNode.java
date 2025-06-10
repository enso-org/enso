package org.enso.interpreter.node;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.compiler.context.LocalScope;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.scope.ModuleScope;

public final class ConstantNode extends EnsoRootNode {
  private final Object constant;

  /**
   * Creates a new instance of this node.
   *
   * @param language the current language instance.
   * @param moduleScope the scope
   * @param constant the value to return.
   */
  public ConstantNode(EnsoLanguage language, ModuleScope moduleScope, Object constant) {
    super(language, LocalScope.empty(), moduleScope, constant.toString(), null);
    this.constant = constant;
  }

  /**
   * Executes the node, returning the predefined constructor.
   *
   * @param frame current execution frame
   * @return the constant constructor
   */
  public Object execute(VirtualFrame frame) {
    return constant;
  }
}
