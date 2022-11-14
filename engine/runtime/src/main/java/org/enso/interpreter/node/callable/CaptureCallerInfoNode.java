package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
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
@GenerateUncached
public abstract class CaptureCallerInfoNode extends Node {
  /**
   * Creates an instance of this node.
   *
   * @return an instance of this node
   */
  public static CaptureCallerInfoNode build() {
    return CaptureCallerInfoNodeGen.create();
  }

  /**
   * Captures the caller info for use in functions called from the current scope.
   *
   * @param frame current execution frame
   * @return caller information for the current scope
   */
  public abstract CallerInfo execute(MaterializedFrame frame);

  static class ScopeInfo {
    private final LocalScope localScope;
    private final ModuleScope moduleScope;

    public ScopeInfo(LocalScope localScope, ModuleScope moduleScope) {
      this.localScope = localScope;
      this.moduleScope = moduleScope;
    }

    public LocalScope getLocalScope() {
      return localScope;
    }

    public ModuleScope getModuleScope() {
      return moduleScope;
    }
  }

  ScopeInfo buildScopeInfo() {
    EnsoRootNode rootNode = (EnsoRootNode) getRootNode();
    return new ScopeInfo(rootNode.getLocalScope(), rootNode.getModuleScope());
  }

  @CompilerDirectives.TruffleBoundary
  ScopeInfo buildUncachedScopeInfo() {
    RootCallTarget ct =
        Truffle.getRuntime()
            .iterateFrames(frameInstance -> (RootCallTarget) frameInstance.getCallTarget());
    EnsoRootNode rootNode = (EnsoRootNode) ct.getRootNode();
    return new ScopeInfo(rootNode.getLocalScope(), rootNode.getModuleScope());
  }

  @Specialization
  CallerInfo doCapture(
      MaterializedFrame frame,
      @Cached(value = "buildScopeInfo()", uncached = "buildUncachedScopeInfo()")
          ScopeInfo scopeInfo) {
    return new CallerInfo(frame, scopeInfo.getLocalScope(), scopeInfo.getModuleScope());
  }
}
