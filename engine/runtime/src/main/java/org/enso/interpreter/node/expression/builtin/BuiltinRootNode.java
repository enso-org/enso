package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.EnsoLanguage;
import org.enso.pkg.QualifiedName;

/** Root node for use by all the builtin functions. */
@NodeInfo(shortName = "BuiltinRoot", description = "Root node for builtin functions.")
public abstract class BuiltinRootNode extends RootNode {
  protected BuiltinRootNode(EnsoLanguage language) {
    super(language);
  }

  protected QualifiedName moduleName = null;
  protected QualifiedName typeName = null;

  /** Get the module name where the builtin is defined. */
  public QualifiedName getModuleName() {
    return moduleName;
  }

  /** Set the module name where the builtin is defined. */
  public void setModuleName(QualifiedName moduleName) {
    this.moduleName = moduleName;
  }

  /** Get the type name of the builtin. */
  public QualifiedName getTypeName() {
    return typeName;
  }

  /** Set the type name of the builtin. */
  public void setTypeName(QualifiedName typeName) {
    this.typeName = typeName;
  }

  /**
   * Executes this node's logic, returning a pair of return value and the new state.
   *
   * @param frame current execution frame
   * @return the result value of executing the logic.
   */
  @Override
  public abstract Object execute(VirtualFrame frame);

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public abstract String getName();
}
