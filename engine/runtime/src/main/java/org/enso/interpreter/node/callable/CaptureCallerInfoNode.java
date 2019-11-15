package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;

/**
 * Captures the current caller info to pass to functions requiring it.
 *
 * <p>The information captured includes current execution frame, as well as static context,
 * including the local and module scope metadata.
 */
@NodeInfo(
    description = "Captures the caller info for use in functions called from this node's scope")
public class CaptureCallerInfoNode extends Node {
  private @CompilerDirectives.CompilationFinal LocalScope localScope;
  private @CompilerDirectives.CompilationFinal ModuleScope moduleScope;

  /**
   * Captures the caller info for use in functions called from the current scope.
   *
   * @param frame current execution frame
   * @return caller information for the current scope
   */
  public CallerInfo execute(VirtualFrame frame) {
    if (localScope == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      EnsoRootNode rootNode = (EnsoRootNode) getRootNode();
      localScope = rootNode.getLocalScope();
      moduleScope = rootNode.getModuleScope();
    }
    return new CallerInfo(frame.materialize(), localScope, moduleScope);
  }

  /**
   * Creates an instance of this node.
   *
   * @return an instance of this node
   */
  public static CaptureCallerInfoNode build() {
    return new CaptureCallerInfoNode();
  }
}
