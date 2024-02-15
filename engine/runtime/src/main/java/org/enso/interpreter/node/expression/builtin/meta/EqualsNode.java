package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@BuiltinMethod(
    type = "Any",
    name = "==",
    description =
        """
      Compares self with other object and returns True iff `self` is exactly the same as
      the other object, including all its transitively accessible properties or fields,
      False otherwise.

      Can handle arbitrary objects, including all foreign objects.

      Does not throw dataflow errors or panics.

      Note that this is different than `Meta.is_same_object`, which checks whether two
      references point to the same object on the heap. Moreover, `Meta.is_same_object`
      implies `Any.==` for all object with the exception of `Number.nan`.
      """)
@GenerateUncached
public abstract class EqualsNode extends Node {

  public static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  public abstract boolean execute(@AcceptsError Object self, @AcceptsError Object right);

  @Specialization
  boolean sameTypeCheck(
      Object self,
      Object other,
      @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached EqualsSimpleNode node) {
    var areEqual = node.execute(self, other);
    return areEqual;
  }
}
