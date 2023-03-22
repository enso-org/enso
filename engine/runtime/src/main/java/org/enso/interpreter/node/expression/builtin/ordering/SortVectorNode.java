package org.enso.interpreter.node.expression.builtin.ordering;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.text.AnyToTextNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.state.State;

/**
 * Sorts a vector with elements that have only Default_Comparator, thus, only elements with a
 * builtin type, which is the most common scenario for sorting.
 * <p>
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
   * @param self             Vector that has elements with only Default_Comparator, that are
   *                         elements with builtin types.
   * @param ascending        -1 for descending, 1 for ascending
   * @param comparators      Vector of comparators, with the same length of self. This is gather in
   *                         the Enso code, because doing that in this builtin would be difficult.
   * @param compareFunctions Vector of `Comparator.compare` functions gathered from the comparators
   * @param byFunc           If Nothing, then the default `by` function should be used. The default
   *                         `by` function is `Ordering.compare`.
   * @return A new, sorted vector
   */
  public abstract Object execute(State state, @AcceptsError Object self, long ascending,
      Object comparators,
      Object compareFunctions, Object byFunc);

  /**
   * Sorts primitive values, i.e., values with only Default_Comparator. We can optimize this case.
   * It is important that `byFunc` is Nothing, i.e., has the default value. In that case, we can
   * hard code the partial ordering for the primitive values. If `byFunc` is a custom user function,
   * it can redefine the default partial ordering of the primitive values, which requires
   * topological sort.
   */
  @Specialization(guards = {
      "interop.hasArrayElements(self)",
      "areAllDefaultComparators(interop, comparators)",
      "isNothing(byFunc)"
  })
  Object sortPrimitives(State state, Object self, long ascending, Object comparators,
      Object compareFunctions, Object byFunc,
      @Cached LessThanNode lessThanNode,
      @Cached EqualsNode equalsNode,
      @Cached HostValueToEnsoNode hostValueToEnsoNode,
      @Cached TypeOfNode typeOfNode,
      @Cached AnyToTextNode toTextNode,
      @Cached BranchProfile warningEncounteredProfile,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
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
      throw new IllegalStateException("Should not reach here", e);
    }
    var javaComparator = new PrimitiveValueComparator(lessThanNode, equalsNode, typeOfNode,
        toTextNode, ascending > 0);
    try {
      return sortPrimitiveVector(elems, javaComparator, warningEncounteredProfile);
    } catch (CompareException e) {
      return DataflowError.withoutTrace(
          incomparableValuesError(e.leftOperand, e.rightOperand), this);
    }
  }

  private TruffleObject sortPrimitiveVector(Object[] elems,
      PrimitiveValueComparator javaComparator, BranchProfile warningEncounteredProfile)
      throws CompareException {
    Arrays.sort(elems, javaComparator);
    var sortedVector = Vector.fromArray(new Array(elems));

    if (javaComparator.encounteredWarnings()) {
      warningEncounteredProfile.enter();
      CompilerDirectives.transferToInterpreter();
      Warning[] warns = javaComparator.getWarnings()
          .stream()
          .map(Text::create)
          .map(text -> Warning.create(EnsoContext.get(this), text, this))
          .toArray(Warning[]::new);
      return WithWarnings.appendTo(sortedVector, new ArrayRope<>(warns));
    } else {
      return sortedVector;
    }
  }

  @TruffleBoundary
  @Specialization(guards = {
      "interop.hasArrayElements(self)",
  })
  Object sortGeneric(State state, Object self, long ascending, Object comparatorsArray,
      Object compareFuncsArray, Object byFunc,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached LessThanNode lessThanNode,
      @Cached EqualsNode equalsNode,
      @Cached TypeOfNode typeOfNode,
      @Cached AnyToTextNode toTextNode,
      @Cached(value = "build()", uncached = "build()") CallOptimiserNode callNode) {
    // Split into groups
    List<Object> elems = readInteropArray(interop, self);
    List<Type> comparators = readInteropArray(interop, comparatorsArray);
    List<Function> compareFuncs = readInteropArray(interop, compareFuncsArray);
    List<Group> groups = splitByComparators(elems, comparators, compareFuncs);

    // TODO: Attach warnings
    // Prepare input for PrimitiveValueComparator and GenericComparator and sort the elements within groups
    var ctx = EnsoContext.get(this);
    Atom less = ctx.getBuiltins().ordering().newLess();
    Atom equal = ctx.getBuiltins().ordering().newEqual();
    Atom greater = ctx.getBuiltins().ordering().newGreater();
    List<Object> resultVec = new ArrayList<>();
    try {
      for (var group : groups) {
        Comparator<Object> javaComparator;
        if (isPrimitiveGroup(group)) {
          javaComparator = new PrimitiveValueComparator(
              lessThanNode,
              equalsNode,
              typeOfNode,
              toTextNode,
              ascending > 0
          );
        } else {
          Function compareFunc = isNothing(byFunc) ? group.compareFunc : (Function) byFunc;
          javaComparator = new GenericComparator(
              ascending > 0,
              compareFunc,
              group.comparator,
              callNode,
              state,
              less,
              equal,
              greater
          );
        }
        group.elems.sort(javaComparator);
        resultVec.addAll(group.elems);
      }
      return Vector.fromArray(new Array(resultVec.toArray()));
    } catch (CompareException e) {
      return DataflowError.withoutTrace(
          incomparableValuesError(e.leftOperand, e.rightOperand), this);
    }
  }

  private List<Group> splitByComparators(List<Object> elements, List<Type> comparators,
      List<Function> compareFuncs) {
    assert elements.size() == comparators.size();
    assert elements.size() == compareFuncs.size();
    // Mapping of FQN of comparator to groups
    Map<String, Group> groupMap = new HashMap<>();
    for (int i = 0; i < elements.size(); i++) {
      Object elem = elements.get(i);
      Type comparator = comparators.get(i);
      Function compareFunc = compareFuncs.get(i);
      String qualifiedName = comparator.getQualifiedName().toString();

      if (!groupMap.containsKey(qualifiedName)) {
        groupMap.put(
            qualifiedName,
            new Group(new ArrayList<>(), comparator, compareFunc)
        );
      }
      var group = groupMap.get(qualifiedName);
      group.elems.add(elem);
    }

    // Sort groups by the FQN of their comparator, with the default comparator
    // being the first one (the first group).
    String defCompFQN = getDefaultComparatorQualifiedName();
    return groupMap
        .entrySet()
        .stream()
        .sorted((entry1, entry2) -> {
          var fqn1 = entry1.getKey();
          var fqn2 = entry2.getKey();
          if (fqn1.equals(defCompFQN)) {
            return -1;
          } else if (fqn2.equals(defCompFQN)) {
            return 1;
          } else {
            return fqn1.compareTo(fqn2);
          }
        })
        .map(Entry::getValue)
        .collect(Collectors.toList());
  }

  private String getDefaultComparatorQualifiedName() {
    return EnsoContext.get(this).getBuiltins().defaultComparator().getType().getQualifiedName()
        .toString();
  }

  /**
   * A group is "primitive" iff its comparator is the default comparator.
   */
  private boolean isPrimitiveGroup(Group group) {
    return group.comparator.getQualifiedName().toString().equals(
        getDefaultComparatorQualifiedName()
    );
  }

  private Object incomparableValuesError(Object left, Object right) {
    return EnsoContext.get(this).getBuiltins().error().makeIncomparableValues(left, right);
  }

  /**
   * Helper slow-path method to conveniently gather elements from interop arrays into a java list
   */
  @SuppressWarnings("unchecked")
  private <T> List<T> readInteropArray(InteropLibrary interop, Object vector) {
    try {
      int size = (int) interop.getArraySize(vector);
      List<T> res = new ArrayList<>(size);
      for (int i = 0; i < size; i++) {
        T elem = (T) interop.readArrayElement(vector, i);
        res.add(elem);
      }
      return res;
    } catch (UnsupportedMessageException | InvalidArrayIndexException | ClassCastException e) {
      throw new IllegalStateException("Should not be reachable", e);
    }
  }

  private int getBuiltinTypeCost(Object builtinType) {
    assert isBuiltinType(builtinType);
    var builtins = EnsoContext.get(this).getBuiltins();
    if (builtinType == builtins.number().getNumber()
        || builtinType == builtins.number().getInteger()
        || builtinType == builtins.number().getDecimal()) {
      return 1;
    } else if (builtinType == builtins.text()) {
      return 2;
    } else if (builtinType == builtins.bool().getType()) {
      return 3;
    } else if (builtinType == builtins.date()) {
      return 4;
    } else if (builtinType == builtins.dateTime()) {
      return 5;
    } else if (builtinType == builtins.duration()) {
      return 6;
    } else if (builtinType == builtins.vector()) {
      // vectors are incomparable, but we want to sort them before Nothings and NaNs.
      return 50;
    } else {
      // Type is not a builtin type
      throw new IllegalStateException("Should be a builtin type: " + builtinType);
    }
  }

  private boolean isBuiltinType(Object type) {
    var builtins = EnsoContext.get(this).getBuiltins();
    return
        builtins.number().getNumber() == type ||
            builtins.number().getDecimal() == type ||
            builtins.number().getInteger() == type ||
            builtins.nothing() == type ||
            builtins.text() == type ||
            builtins.bool().getType() == type ||
            builtins.date() == type ||
            builtins.dateTime() == type ||
            builtins.duration() == type ||
            builtins.vector() == type;
  }

  private boolean isTrue(Object object) {
    return Boolean.TRUE.equals(object);
  }

  /**
   * Returns true iff the given array of comparators is all Default_Comparator
   */
  boolean areAllDefaultComparators(InteropLibrary interop, Object comparators) {
    assert interop.hasArrayElements(comparators);
    var ctx = EnsoContext.get(this);
    try {
      int compSize = (int) interop.getArraySize(comparators);
      for (int i = 0; i < compSize; i++) {
        assert interop.isArrayElementReadable(comparators, i);
        Object comparator = interop.readArrayElement(comparators, i);
        if (!isDefaultComparator(comparator, ctx)) {
          return false;
        }
      }
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException("Should not be reachable", e);
    }
    return true;
  }

  boolean isDefaultComparator(Object object, EnsoContext ctx) {
    return ctx.getBuiltins().defaultComparator().getType() == object;
  }

  private boolean isNan(Object object) {
    return object instanceof Double dbl && dbl.isNaN();
  }

  boolean isNothing(Object object) {
    return isNothing(object, EnsoContext.get(this));
  }

  private boolean isNothing(Object object, EnsoContext ctx) {
    return object == ctx.getBuiltins().nothing();
  }

  /**
   * Group of elements grouped by comparator.
   *
   * @param elems       Elements of the group.
   * @param comparator  Comparator for the elems, i.e., it should hold that
   *                    {@code elems.each it-> (Comparable.from it) == comparator}.
   * @param compareFunc `Comparator.compare` function extracted from the comparator.
   */
  private record Group(
      List<Object> elems,
      Type comparator,
      Function compareFunc
  ) {

  }

  /**
   * Comparator for comparing primitive values (which implies that they have Default_Comparator),
   * which the default `by` method parameter.
   */
  private class PrimitiveValueComparator implements java.util.Comparator<Object> {

    private final LessThanNode lessThanNode;
    private final EqualsNode equalsNode;
    private final TypeOfNode typeOfNode;
    private final AnyToTextNode toTextNode;
    private final boolean ascending;
    private final Set<String> warnings = new HashSet<>();

    private PrimitiveValueComparator(LessThanNode lessThanNode, EqualsNode equalsNode,
        TypeOfNode typeOfNode,
        AnyToTextNode toTextNode, boolean ascending) {
      this.lessThanNode = lessThanNode;
      this.equalsNode = equalsNode;
      this.typeOfNode = typeOfNode;
      this.toTextNode = toTextNode;
      this.ascending = ascending;
    }

    @Override
    public int compare(Object x, Object y) {
      return comparePrimitiveValues(x, y);
    }

    int comparePrimitiveValues(Object x, Object y) {
      if (equalsNode.execute(x, y)) {
        return 0;
      } else {
        // Check if x < y
        Object xLessThanYRes = lessThanNode.execute(x, y);
        if (isNothing(xLessThanYRes)) {
          // x and y are incomparable - this can happen if x and y are different types
          attachIncomparableValuesWarning(x, y);
          return handleIncomparablePrimitives(x, y);
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
            return handleIncomparablePrimitives(y, x);
          }
        }
      }
    }

    /**
     * Incomparable primitive values have either different builtin types, or are Nothing or NaN. All
     * these cases are handled specifically - we hardcode the order of these incomparable values.
     */
    private int handleIncomparablePrimitives(Object x, Object y) {
      // "Nothing > NaN"
      int xCost = getPrimitiveValueCost(x);
      int yCost = getPrimitiveValueCost(y);
      int res = Integer.compare(xCost, yCost);
      return ascending ? res : -res;
    }

    private int getPrimitiveValueCost(Object object) {
      if (isNothing(object)) {
        return 200;
      } else if (isNan(object)) {
        return 100;
      } else {
        var type = typeOfNode.execute(object);
        return getBuiltinTypeCost(type);
      }
    }

    @TruffleBoundary
    private void attachIncomparableValuesWarning(Object x, Object y) {
      var xStr = toTextNode.execute(x).toString();
      var yStr = toTextNode.execute(y).toString();
      String warnText = "Values " + xStr + " and " + yStr + " are incomparable";
      warnings.add(warnText);
    }

    private boolean encounteredWarnings() {
      return !warnings.isEmpty();
    }

    private Set<String> getWarnings() {
      return warnings;
    }
  }

  /**
   * Comparator for any values. This comparator compares the values by calling back to Enso (by
   * {@link #compareFunc}), rather than using compare nodes (i.e. {@link LessThanNode}). directly,
   * as opposed to {@link PrimitiveValueComparator}.
   */
  private class GenericComparator implements java.util.Comparator<Object> {

    private final boolean ascending;
    /**
     * Either function from `by` parameter to the `Vector.sort` method, or the `compare` function
     * extracted from the comparator for the appropriate group.
     */
    private final Function compareFunc;
    private final Type comparator;
    private final CallOptimiserNode callNode;
    private final State state;
    private final Atom less;
    private final Atom equal;
    private final Atom greater;


    private GenericComparator(
        boolean ascending,
        Function compareFunc,
        Type comparator, CallOptimiserNode callNode, State state, Atom less, Atom equal,
        Atom greater) {
      assert compareFunc != null;
      assert comparator != null;
      this.comparator = comparator;
      this.state = state;
      this.ascending = ascending;
      this.compareFunc = compareFunc;
      this.callNode = callNode;
      this.less = less;
      this.equal = equal;
      this.greater = greater;
    }

    @Override
    public int compare(Object x, Object y) {
      // We are calling a static method here, so we need to pass the Comparator type as the
      // self (first) argument.
      Object res = callNode.executeDispatch(compareFunc, null, state,
          new Object[]{comparator, x, y});
      if (res == less) {
        return ascending ? -1 : 1;
      } else if (res == equal) {
        return 0;
      } else if (res == greater) {
        return ascending ? 1 : -1;
      } else {
        // res is either Nothing, or Incomparable_Values. Either way, it means that x and y are incomparable.
        // This case is not supported yet, as it requires topological sorting.
        // We cannot detect if the result was actually returned from the default comparator (it
        // could have been transitively called), so we just bailout.
        throw new CompareException(x, y);
      }
    }
  }

  private static final class CompareException extends RuntimeException {

    final Object leftOperand;
    final Object rightOperand;

    private CompareException(Object leftOperand, Object rightOperand) {
      this.leftOperand = leftOperand;
      this.rightOperand = rightOperand;
    }
  }
}
