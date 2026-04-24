package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.expression.builtin.meta.IsValueOfTypeNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
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
      VirtualFrame frame,
      Object state,
      Atom target,
      @Shared @CachedLibrary(limit = "3") StructsLibrary structsLib) {
    if (profile.profile(matcher == target.getConstructor())) {
      var arr = fieldsFromObject(target, matcher, structsLib);
      accept(frame, state, arr);
    }
  }

  @Specialization
  void doMultiValue(
      VirtualFrame frame,
      Object state,
      EnsoMultiValue target,
      @Cached IsValueOfTypeNode isValueOfTypeNode,
      @Shared @CachedLibrary(limit = "3") StructsLibrary structsLib,
      @Cached EnsoMultiValue.CastToNode castNode) {
    var expectedType = matcher.getType();
    if (profile.profile(isValueOfTypeNode.execute(expectedType, target, false))) {
      // replacement is the narrowed (type) value.
      var replacement = castNode.findTypeOrNull(expectedType, target, true, false);
      assert replacement != null : "Must find the type, when isValueOfTypeNode is true";
      if (replacement instanceof EnsoMultiValue mv
          && mv.firstDispatchValue() instanceof Atom replacementAtom) {
        if (matcher != replacementAtom.getConstructor()) {
          // The narrowed value atom has a different constructor - it cannot match.
          return;
        }
      }
      var arr = fieldsFromObject(replacement, matcher, structsLib);
      accept(frame, state, arr);
    }
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}

  @ExplodeLoop
  private static Object[] fieldsFromObject(
      Object obj, AtomConstructor cons, StructsLibrary structsLib) {
    CompilerAsserts.partialEvaluationConstant(cons);
    var arr = new Object[cons.getArity()];
    var fields = cons.getFields();
    for (var i = 0; i < arr.length; i++) {
      // Wrap suspended fields in functions, so that they are not evaluated
      // too eagerly.
      if (fields[i].isSuspended() && obj instanceof Atom atom) {
        var func = createSuspendedFieldFunction(atom, i);
        arr[i] = func;
      } else {
        arr[i] = structsLib.getField(obj, i);
      }
    }
    return arr;
  }

  @TruffleBoundary
  private static Function createSuspendedFieldFunction(Atom atom, int fieldIdx) {
    var getFieldNode = new GetSuspendedFieldNode(atom, fieldIdx);
    return Function.fullyApplied(getFieldNode.getCallTarget());
  }

  private static final class GetSuspendedFieldNode extends RootNode {
    private final Atom atom;
    private final int fieldIdx;
    @Child private StructsLibrary structsLib = StructsLibrary.getFactory().createDispatched(3);

    GetSuspendedFieldNode(Atom atom, int fieldIdx) {
      super(EnsoLanguage.get(null));
      this.atom = atom;
      this.fieldIdx = fieldIdx;
    }

    @Override
    public Object execute(VirtualFrame frame) {
      // Forces evaluation of the suspended field.
      return structsLib.getField(atom, fieldIdx);
    }
  }
}
