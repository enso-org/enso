package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.StructsLibrary;

/** An implementation of the case expression specialised to working on constructors. */
@NodeInfo(shortName = "ConstructorMatch")
public abstract class ConstructorBranchNode extends BranchNode {
  private final AtomConstructor matcher;
  private final CountingConditionProfile profile = CountingConditionProfile.create();

  ConstructorBranchNode(AtomConstructor matcher, RootCallTarget branch, boolean terminalBranch) {
    super(branch, terminalBranch);
    this.matcher = matcher;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matcher the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static ConstructorBranchNode build(
      AtomConstructor matcher, RootCallTarget branch, boolean terminalBranch) {
    return ConstructorBranchNodeGen.create(matcher, branch, terminalBranch);
  }

  @Specialization
  void doAtom(
      VirtualFrame frame, Object state, Atom target, @Cached FieldsAsArrayNode toArrayNode) {
    if (profile.profile(matcher == target.getConstructor())) {
      var arr = toArrayNode.executeFields(target);
      accept(frame, state, arr);
    }
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}

  abstract static class FieldsAsArrayNode extends Node {
    abstract Object[] executeFields(Atom target);

    @Specialization(
        limit = "3",
        guards = {"atom.getConstructor() == cons"})
    @ExplodeLoop
    Object[] fieldsFromAtomCached(
        Atom atom,
        @Cached("atom.getConstructor()") AtomConstructor cons,
        @CachedLibrary(limit = "3") StructsLibrary structs) {
      var arr = new Object[cons.getArity()];
      for (var i = 0; i < arr.length; i++) {
        arr[i] = structs.getField(atom, i);
      }
      return arr;
    }

    @Specialization
    @TruffleBoundary
    Object[] fieldsFromAtomUncached(Atom atom) {
      return fieldsFromAtomCached(atom, atom.getConstructor(), StructsLibrary.getUncached());
    }
  }
}
