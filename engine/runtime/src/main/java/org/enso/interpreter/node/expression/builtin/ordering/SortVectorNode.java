package org.enso.interpreter.node.expression.builtin.ordering;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.text.AnyToTextNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.util.Collections.ArrayListObj;

/**
 * Sorts a vector with elements that have only Default_Comparator, thus, only elements with a
 * builtin type, which is the most common scenario for sorting.
 *
 * TODO: Max number of attached Incomparable values warnings?
 *  - hardcode or pass via a new parameter to Vector.sort?
 */
@BuiltinMethod(
    type = "Vector",
    name = "sort_builtin",
    description = "Returns a sorted vector."
)
@GenerateUncached
public abstract class SortVectorNode extends Node {
  public static SortVectorNode build() {
    return SortVectorNodeGen.create();
  }

  /**
   * Sorts a vector with elements that have only Default_Comparator, thus, only builtin types.
   *
   * @param self      Vector that has elements with only Default_Comparator, that are elements with
   *                  builtin types.
   * @param ascending -1 for descending, 1 for ascending
   * @return A new, sorted vector
   */
  public abstract Object execute(@AcceptsError Object self, long ascending);

  @Specialization(guards = {
      "interop.hasArrayElements(self)"
  })
  Object sortCached(Object self, long ascending,
      @Cached LessThanNode lessThanNode,
      @Cached EqualsNode equalsNode,
      @Cached HostValueToEnsoNode hostValueToEnsoNode,
      @Cached TypeOfNode typeOfNode,
      @Cached AnyToTextNode toTextNode,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "5") WarningsLibrary warningsLib) {
    EnsoContext ctx = EnsoContext.get(this);
    Object[] elems;
    try {
      long size = interop.getArraySize(self);
      assert size < Integer.MAX_VALUE;
      elems = new Object[(int) size];
      for (int i = 0; i < size; i++) {
        if (interop.isArrayElementReadable(self, i)) {
          elems[i] = hostValueToEnsoNode.execute(
              interop.readArrayElement(self, i)
          );
        } else {
          throw new PanicException(
              ctx.getBuiltins().error().makeUnsupportedArgumentsError(
                  new Object[]{self},
                  "Cannot read array element at index " + i + " of " + self
              ),
              this
          );
        }
      }
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
    var comparator = new Comparator(lessThanNode, equalsNode, typeOfNode, toTextNode, ascending > 0);
    Arrays.sort(elems, comparator);
    var vector = Vector.fromArray(new Array(elems));

    // Check for the warnings attached from the Comparator
    Warning[] currWarns = null;
    if (comparator.encounteredWarnings()) {
      currWarns = (Warning[]) comparator.getWarnings();
    }
    if (currWarns != null) {
      return WithWarnings.appendTo(vector, new ArrayRope<>(currWarns));
    } else {
      return vector;
    }
  }

  private int typeOrder(Object object, TypeOfNode typeOfNode) {
    var ctx = EnsoContext.get(this);
    var builtins = ctx.getBuiltins();
    if (isNothing(object, ctx)) {
      return 200;
    }
    var type = typeOfNode.execute(object);
    if (type == builtins.number().getNumber()
        || type == builtins.number().getInteger()
        || type == builtins.number().getDecimal()) {
      if (object instanceof Double dbl && dbl.isNaN()) {
        return 100;
      } else {
        return 1;
      }
    }
    else if (type == builtins.text()) {
      return 2;
    }
    else if (type == builtins.bool().getType()) {
      return 3;
    }
    else if (type == builtins.date()) {
      return 4;
    }
    else if (type == builtins.dateTime()) {
      return 5;
    }
    else if (type == builtins.duration()) {
      return 6;
    } else {
      throw new IllegalStateException("Unexpected type: " + type);
    }
  }

  private boolean isTrue(Object object) {
    return Boolean.TRUE.equals(object);
  }

  private boolean isNothing(Object object) {
    return isNothing(object, EnsoContext.get(this));
  }

  private boolean isNothing(Object object, EnsoContext ctx) {
    return object == ctx.getBuiltins().nothing();
  }

  private final class Comparator implements java.util.Comparator<Object> {

    private final LessThanNode lessThanNode;
    private final EqualsNode equalsNode;
    private final TypeOfNode typeOfNode;
    private final AnyToTextNode toTextNode;
    private final boolean ascending;
    private final ArrayListObj<Warning> warnings = new ArrayListObj<>();

    private Comparator(LessThanNode lessThanNode, EqualsNode equalsNode, TypeOfNode typeOfNode,
        AnyToTextNode toTextNode, boolean ascending) {
      this.lessThanNode = lessThanNode;
      this.equalsNode = equalsNode;
      this.typeOfNode = typeOfNode;
      this.toTextNode = toTextNode;
      this.ascending = ascending;
    }

    @Override
    public int compare(Object x, Object y) {
      if (equalsNode.execute(x, y)) {
        return 0;
      } else {
        // Check if x < y
        Object xLessThanYRes = lessThanNode.execute(x, y);
        if (isNothing(xLessThanYRes)) {
          // x and y are incomparable - this can happen if x and y are different types
          attachIncomparableValuesWarning(x, y);
          return compareTypes(x, y);
        } else if (isTrue(xLessThanYRes)) {
          return ascending ? -1 : 1;
        } else {
          // Check if x > y
          Object yLessThanXRes = lessThanNode.execute(y, x);
          if (isTrue(yLessThanXRes)) {
            return ascending ? 1 : -1;
          } else {
            // yLessThanXRes is either Nothing or False
            attachIncomparableValuesWarning(y, x);
            return compareTypes(y, x);
          }
        }
      }
    }

    private int compareTypes(Object x, Object y) {
      int res =Integer.compare(
          typeOrder(x, typeOfNode),
          typeOrder(y, typeOfNode)
      );
      return ascending ? res : -res;
    }

    private void attachIncomparableValuesWarning(Object x, Object y) {
      var xStr = toTextNode.execute(x).toString();
      var yStr = toTextNode.execute(y).toString();
      var payload = Text.create("Values " + xStr + " and " + yStr + " are incomparable");
      var sortNode = SortVectorNode.this;
      var warn = Warning.create(EnsoContext.get(sortNode), payload, sortNode);
      warnings.add(warn);
    }

    private boolean encounteredWarnings() {
      return warnings.size() > 0;
    }

    private Object[] getWarnings() {
      Warning[] warns = new Warning[warnings.size()];
      warns = warnings.toArray(warns.getClass());
      return warns;
    }
  }
}
