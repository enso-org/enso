package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.util.Arrays;
import org.enso.interpreter.node.expression.builtin.ordering.HasCustomComparatorNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;

@GenerateUncached
public abstract class EqualsAtomNode extends Node {

  public static EqualsAtomNode build() {
    return EqualsAtomNodeGen.create();
  }

  public abstract boolean execute(Atom left, Atom right);

  static EqualsNode[] createEqualsNodes(int size) {
    EqualsNode[] nodes = new EqualsNode[size];
    Arrays.fill(nodes, EqualsNode.build());
    return nodes;
  }

  @Specialization(guards = {
      "selfCtorCached == self.getConstructor()"
  }, limit = "10")
  @ExplodeLoop
  boolean equalsAtoms(
      Atom self,
      Atom other,
      @Cached("self.getConstructor()") AtomConstructor selfCtorCached,
      @Cached(value = "selfCtorCached.getFields().length", allowUncached = true) int fieldsLenCached,
      @Cached(value = "createEqualsNodes(fieldsLenCached)", allowUncached = true) EqualsNode[] fieldEqualsNodes,
      @Cached ConditionProfile constructorsNotEqualProfile,
      @CachedLibrary(limit = "5") StructsLibrary structsLib
  ) {
    if (constructorsNotEqualProfile.profile(
        self.getConstructor() != other.getConstructor()
    )) {
      return false;
    }
    var selfFields = structsLib.getFields(self);
    var otherFields = structsLib.getFields(other);
    assert selfFields.length == otherFields.length : "Constructors are same, atoms should have the same number of fields";

    CompilerAsserts.partialEvaluationConstant(fieldsLenCached);
    for (int i = 0; i < fieldsLenCached; i++) {
      boolean fieldsAreEqual = fieldEqualsNodes[i].execute(
        selfFields[i], otherFields[i]
      );
      if (!fieldsAreEqual) {
        return false;
      }
    }
    return true;
  }

  @CompilerDirectives.TruffleBoundary
  @Specialization(replaces = "equalsAtoms")
  boolean equalsAtomsUncached(Atom self, Atom other) {
    if (self.getConstructor() != other.getConstructor()) {
      return false;
    }
    Object[] selfFields = StructsLibrary.getUncached().getFields(self);
    Object[] otherFields = StructsLibrary.getUncached().getFields(other);
    if (selfFields.length != otherFields.length) {
      return false;
    }
    for (int i = 0; i < selfFields.length; i++) {
      boolean areFieldsSame = EqualsNodeGen.getUncached().execute(selfFields[i], otherFields[i]);
      if (!areFieldsSame) {
        return false;
      }
    }
    return true;
  }

}
