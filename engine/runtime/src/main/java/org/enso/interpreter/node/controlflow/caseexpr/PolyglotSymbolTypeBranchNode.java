package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.node.expression.builtin.meta.IsSameObjectNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;

/** An implementation of the case expression specialised to working on polyglot types. */
@NodeInfo(shortName = "PolyglotSymbolTypeMatch")
@SuppressWarnings("truffle-splitting")
public abstract class PolyglotSymbolTypeBranchNode extends BranchNode {

  private final Object polyglotSymbol;
  private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
  private final CountingConditionProfile profile = CountingConditionProfile.create();

  PolyglotSymbolTypeBranchNode(
      Object polyglotSymbol, RootCallTarget functionNode, boolean terminalBranch) {
    super(functionNode, terminalBranch);
    this.polyglotSymbol = polyglotSymbol;
  }

  /**
   * Creates a node to handle the by-type polyglot symbol case.
   *
   * @param polyglotSymbol polyglotSymbol representing the type to match against
   * @param functionNode the function to execute in this case
   * @return a catch-all node
   */
  public static PolyglotSymbolTypeBranchNode build(
      Object polyglotSymbol, RootCallTarget functionNode, boolean terminalBranch) {
    return PolyglotSymbolTypeBranchNodeGen.create(polyglotSymbol, functionNode, terminalBranch);
  }

  @Specialization
  public void doPolyglotValue(
      VirtualFrame frame,
      Object state,
      Object target,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      if (interop.hasMetaObject(target)) {
        var tpeOfTarget = interop.getMetaObject(target);
        var test = isSameObject.execute(polyglotSymbol, tpeOfTarget);
        if (!test && interop.hasMetaParents(tpeOfTarget)) {
          test = findPolyglotSymbolInTypeHierarchy(tpeOfTarget);
        }
        if (profile.profile(test)) {
          accept(frame, state, new Object[] {target});
        }
      }
    } catch (InteropException e) {
      Atom err = reportError(polyglotSymbol, target);
      throw new PanicException(err, this);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private boolean findPolyglotSymbolInTypeHierarchy(Object type)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    if (isSameObject.execute(polyglotSymbol, type)) {
      return true;
    }
    var iop = InteropLibrary.getUncached();
    if (!iop.hasMetaParents(type)) {
      return false;
    }
    var parents = iop.getMetaParents(type);
    var len = iop.getArraySize(parents);
    for (var i = 0L; i < len; i++) {
      var p = iop.readArrayElement(parents, i);
      if (findPolyglotSymbolInTypeHierarchy(p)) {
        return true;
      }
    }
    return false;
  }

  @CompilerDirectives.TruffleBoundary
  private Atom reportError(Object expected, Object target) {
    var builtins = EnsoContext.get(this).getBuiltins();
    return builtins
        .error()
        .makeCompileError("unable to check if " + target + " is an instance of " + expected);
  }
}
