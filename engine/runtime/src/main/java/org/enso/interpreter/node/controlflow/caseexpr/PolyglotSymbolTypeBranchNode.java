package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.node.expression.builtin.meta.IsSameObjectNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;

/** An implementation of the case expression specialised to working on polyglot types. */
@NodeInfo(shortName = "PolyglotSymbolTypeMatch")
public abstract class PolyglotSymbolTypeBranchNode extends BranchNode {

  private final Object polyglotSymbol;
  private @Child TypeOfNode typeOfNode = TypeOfNode.build();
  private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
  private final CountingConditionProfile profile = CountingConditionProfile.create();
  private final CountingConditionProfile subtypeProfile = CountingConditionProfile.create();

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
    Object tpeOfTarget = typeOfNode.execute(target);
    boolean test = isSameObject.execute(polyglotSymbol, tpeOfTarget);
    if (profile.profile(test)) {
      accept(frame, state, new Object[] {target});
    } else {
      try {
        if (subtypeProfile.profile(
            interop.isMetaObject(polyglotSymbol)
                && interop.isMetaInstance(polyglotSymbol, target))) {
          accept(frame, state, new Object[] {target});
        }
      } catch (UnsupportedMessageException e) {
        Atom err = reportError(polyglotSymbol, target);
        throw new PanicException(err, this);
      }
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Atom reportError(Object expected, Object target) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    return builtins
        .error()
        .makeCompileError("unable to check if " + target + " is an instance of " + polyglotSymbol);
  }
}
