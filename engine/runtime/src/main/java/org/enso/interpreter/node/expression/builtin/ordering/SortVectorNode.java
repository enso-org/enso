package org.enso.interpreter.node.expression.builtin.ordering;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.util.ArrayList;
import java.util.Arrays;
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
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.text.AnyToTextNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

/**
 * Sorts a vector with elements that have only Default_Comparator, thus, only elements with a
 * builtin type, which is the most common scenario for sorting.
 */
@BuiltinMethod(type = "Vector", name = "sort_builtin", description = "Returns a sorted vector.")
@GenerateUncached
public abstract class SortVectorNode extends Node {

  private static final int MAX_SORT_WARNINGS = 10;

  public static SortVectorNode build() {
    return SortVectorNodeGen.create();
  }

  /**
   * Sorts a vector with elements that have only Default_Comparator, thus, only builtin types.
   *
   * @param self Vector that has elements with only Default_Comparator, that are elements with
   *     builtin types.
   * @param ascending -1 for descending, 1 for ascending
   * @param comparators Vector of comparators, with the same length of self. This is gather in the
   *     Enso code, because doing that in this builtin would be difficult. If {@code onFunc}
   *     parameter is not {@code Nothing}, comparators are gathered from the result of {@code
   *     onFunc} projection.
   * @param compareFunctions Vector of `Comparator.compare` functions gathered from the comparators
   * @param byFunc If Nothing, then the default `by` function should be used. The default `by`
   *     function is `Ordering.compare`.
   * @param onFunc If Nothing, then the default identity function should be used.
   * @param problemBehavior A long representation of `Problem_Behavior`. Ignore is 0, Report_warning
   *     is 1, and Report_Error is 2.
   * @return A new, sorted vector.
   */
  public abstract Object execute(
      State state,
      @AcceptsError Object self,
      long ascending,
      Object comparators,
      Object compareFunctions,
      Object byFunc,
      Object onFunc,
      long problemBehavior);

  /**
   * Sorts primitive values, i.e., values with only Default_Comparator. We can optimize this case.
   * It is important that `byFunc` is Nothing, i.e., has the default value. In that case, we can
   * hard code the partial ordering for the primitive values. If `byFunc` is a custom user function,
   * it can redefine the default partial ordering of the primitive values, which requires
   * topological sort.
   */
  @Specialization(
      guards = {
        "interop.hasArrayElements(self)",
        "areAllDefaultComparators(lengthNode, atNode, comparators)",
        "interop.isNull(byFunc)",
        "interop.isNull(onFunc)"
      })
  Object sortPrimitives(
      State state,
      Object self,
      long ascending,
      Object comparators,
      Object compareFunctions,
      Object byFunc,
      Object onFunc,
      long problemBehavior,
      @Shared("lessThanNode") @Cached LessThanNode lessThanNode,
      @Shared("equalsNode") @Cached EqualsNode equalsNode,
      @Shared("lengthNode") @Cached ArrayLikeLengthNode lengthNode,
      @Shared("atNode") @Cached ArrayLikeAtNode atNode,
      @Shared("typeOfNode") @Cached TypeOfNode typeOfNode,
      @Shared("anyToTextNode") @Cached AnyToTextNode toTextNode,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    EnsoContext ctx = EnsoContext.get(this);
    Object[] elems;
    long longSize = 0L;
    try {
      longSize = lengthNode.executeLength(self);
      int size = Math.toIntExact(longSize);
      elems = new Object[size];
      for (int i = 0; i < size; i++) {
        elems[i] = atNode.executeAt(self, i);
      }
    } catch (ArithmeticException | InvalidArrayIndexException e) {
      throw invalidArrayIndexException(e, longSize);
    }
    var javaComparator =
        createDefaultComparator(
            lessThanNode, equalsNode, typeOfNode, toTextNode, ascending, problemBehavior, interop);
    try {
      return sortPrimitiveVector(elems, javaComparator);
    } catch (CompareException e) {
      return DataflowError.withoutTrace(
          incomparableValuesError(e.leftOperand, e.rightOperand), this);
    }
  }

  @TruffleBoundary
  private DefaultSortComparator createDefaultComparator(
      LessThanNode lessThanNode,
      EqualsNode equalsNode,
      TypeOfNode typeOfNode,
      AnyToTextNode toTextNode,
      long ascending,
      long problemBehaviorNum,
      InteropLibrary interop) {
    return new DefaultSortComparator(
        lessThanNode,
        equalsNode,
        typeOfNode,
        toTextNode,
        ascending > 0,
        ProblemBehavior.fromInt((int) problemBehaviorNum),
        interop);
  }

  @TruffleBoundary
  @Specialization(
      guards = {
        "interop.hasArrayElements(self)",
      })
  Object sortGeneric(
      State state,
      Object self,
      long ascending,
      Object comparatorsArray,
      Object compareFuncsArray,
      Object byFunc,
      Object onFunc,
      long problemBehaviorNum,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "5") WarningsLibrary warningsLib,
      @CachedLibrary(limit = "5") TypesLibrary typesLib,
      @Shared("lessThanNode") @Cached LessThanNode lessThanNode,
      @Shared("equalsNode") @Cached EqualsNode equalsNode,
      @Shared("typeOfNode") @Cached TypeOfNode typeOfNode,
      @Shared("lengthNode") @Cached ArrayLikeLengthNode lengthNode,
      @Shared("atNode") @Cached ArrayLikeAtNode atNode,
      @Shared("anyToTextNode") @Cached AnyToTextNode toTextNode,
      @Cached MethodResolverNode methodResolverNode,
      @Cached(value = "build()", uncached = "build()") CallOptimiserNode callNode) {
    var problemBehavior = ProblemBehavior.fromInt((int) problemBehaviorNum);
    // Split into groups
    List<Object> elems = readInteropArray(lengthNode, atNode, warningsLib, self);
    List<Type> comparators = readInteropArray(lengthNode, atNode, warningsLib, comparatorsArray);
    List<Function> compareFuncs =
        readInteropArray(lengthNode, atNode, warningsLib, compareFuncsArray);
    List<Group> groups = splitByComparators(elems, comparators, compareFuncs);

    // Prepare input for DefaultSortComparator and GenericSortComparator and sort the elements
    // within groups
    var ctx = EnsoContext.get(this);
    Atom less = ctx.getBuiltins().ordering().newLess();
    Atom equal = ctx.getBuiltins().ordering().newEqual();
    Atom greater = ctx.getBuiltins().ordering().newGreater();
    Set<String> gatheredWarnings = new HashSet<>();
    List<Object> resultVec = new ArrayList<>();
    try {
      for (var group : groups) {
        SortComparator javaComparator;
        if (interop.isNull(byFunc) && interop.isNull(onFunc) && isPrimitiveGroup(group)) {
          javaComparator =
              new DefaultSortComparator(
                  lessThanNode,
                  equalsNode,
                  typeOfNode,
                  toTextNode,
                  ascending > 0,
                  problemBehavior,
                  interop);
        } else {
          Object compareFunc = interop.isNull(byFunc) ? group.compareFunc : byFunc;
          javaComparator =
              new GenericSortComparator(
                  ascending > 0,
                  compareFunc,
                  onFunc,
                  problemBehavior,
                  group.comparator,
                  callNode,
                  toTextNode,
                  state,
                  less,
                  equal,
                  greater,
                  interop,
                  typesLib,
                  methodResolverNode);
        }
        group.elems.sort(javaComparator);
        if (javaComparator.hasWarnings()) {
          gatheredWarnings.addAll(javaComparator.getEncounteredWarnings());
        }
        resultVec.addAll(group.elems);
      }
      var sortedVector = ArrayLikeHelpers.asVectorWithCheckAt(resultVec.toArray());
      // Attach gathered warnings along with different comparators warning
      switch (problemBehavior) {
        case REPORT_ERROR -> {
          if (groups.size() > 1) {
            assert groups.get(0).elems.size() > 0 : "Groups should not be empty";
            assert groups.get(1).elems.size() > 0 : "Groups should not be empty";
            var firstIncomparableElem = groups.get(0).elems.get(0);
            var secondIncomparableElem = groups.get(1).elems.get(0);
            var err =
                ctx.getBuiltins()
                    .error()
                    .makeIncomparableValues(firstIncomparableElem, secondIncomparableElem);
            return DataflowError.withoutTrace(err, this);
          } else {
            // Just one comparator, different from Default_Comparator
            if (!gatheredWarnings.isEmpty()) {
              throw new UnsupportedOperationException("unimplemented");
            } else {
              return sortedVector;
            }
          }
        }
        case REPORT_WARNING -> {
          return attachDifferentComparatorsWarning(
              gatheredWarnings.isEmpty()
                  ? sortedVector
                  : attachWarnings(sortedVector, gatheredWarnings),
              groups);
        }
        case IGNORE -> {
          return sortedVector;
        }
        default -> throw EnsoContext.get(this).raiseAssertionPanic(this, "unreachable", null);
      }
    } catch (CompareException e) {
      return DataflowError.withoutTrace(
          incomparableValuesError(e.leftOperand, e.rightOperand), this);
    }
  }

  @TruffleBoundary(allowInlining = true)
  private Object sortPrimitiveVector(Object[] elems, DefaultSortComparator javaComparator)
      throws CompareException {
    Arrays.sort(elems, javaComparator);
    var sortedVector = ArrayLikeHelpers.asVectorWithCheckAt(elems);

    if (javaComparator.hasWarnings()) {
      return attachWarnings(sortedVector, javaComparator.getEncounteredWarnings());
    } else {
      return sortedVector;
    }
  }

  private List<Group> splitByComparators(
      List<Object> elements, List<Type> comparators, List<Function> compareFuncs) {
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
        groupMap.put(qualifiedName, new Group(new ArrayList<>(), comparator, compareFunc));
      }
      var group = groupMap.get(qualifiedName);
      group.elems.add(elem);
    }

    // Sort groups by the FQN of their comparator, with the default comparator
    // being the first one (the first group).
    String defCompFQN = getDefaultComparatorQualifiedName();
    return groupMap.entrySet().stream()
        .sorted(
            (entry1, entry2) -> {
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

  private Object attachWarnings(Object vector, Set<String> warnings) {
    var ctx = EnsoContext.get(this);
    var warnArray =
        warnings.stream()
            .map(Text::create)
            .map(text -> Warning.create(ctx, text, this))
            .limit(MAX_SORT_WARNINGS)
            .toArray(Warning[]::new);
    return WithWarnings.appendTo(ctx, vector, warnArray.length < warnings.size(), warnArray);
  }

  private Object attachDifferentComparatorsWarning(Object vector, List<Group> groups) {
    var diffCompsMsg =
        groups.stream()
            .map(Group::comparator)
            .map(comparator -> comparator.getQualifiedName().toString())
            .collect(Collectors.joining(", "));
    var text = Text.create("Different comparators: [" + diffCompsMsg + "]");
    var ctx = EnsoContext.get(this);
    var warn = Warning.create(ctx, text, this);
    return WithWarnings.appendTo(ctx, vector, false, warn);
  }

  private String getDefaultComparatorQualifiedName() {
    return EnsoContext.get(this)
        .getBuiltins()
        .defaultComparator()
        .getType()
        .getQualifiedName()
        .toString();
  }

  /** A group is "primitive" iff its comparator is the default comparator. */
  private boolean isPrimitiveGroup(Group group) {
    return group
        .comparator
        .getQualifiedName()
        .toString()
        .equals(getDefaultComparatorQualifiedName());
  }

  private Object incomparableValuesError(Object left, Object right) {
    return EnsoContext.get(this).getBuiltins().error().makeIncomparableValues(left, right);
  }

  /**
   * Helper slow-path method to conveniently gather elements from interop arrays into a java list
   */
  @SuppressWarnings("unchecked")
  private <T> List<T> readInteropArray(
      ArrayLikeLengthNode lengthNode,
      ArrayLikeAtNode atNode,
      WarningsLibrary warningsLib,
      Object vector) {
    var longSize = 0L;
    try {
      longSize = lengthNode.executeLength(vector);
      int size = Math.toIntExact(longSize);
      List<T> res = new ArrayList<>(size);
      for (int i = 0; i < size; i++) {
        Object elem = atNode.executeAt(vector, i);
        if (warningsLib.hasWarnings(elem)) {
          elem = warningsLib.removeWarnings(elem);
        }
        res.add((T) elem);
      }
      return res;
    } catch (UnsupportedMessageException | InvalidArrayIndexException | ClassCastException e) {
      throw invalidArrayIndexException(e, longSize);
    }
  }

  private int getBuiltinTypeCost(Object builtinType) {
    assert isBuiltinType(builtinType);
    var builtins = EnsoContext.get(this).getBuiltins();
    if (builtinType == builtins.number().getNumber()
        || builtinType == builtins.number().getInteger()
        || builtinType == builtins.number().getFloat()) {
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
      throw EnsoContext.get(this)
          .raiseAssertionPanic(this, "Should be a builtin type: " + builtinType, null);
    }
  }

  private boolean isBuiltinType(Object type) {
    var builtins = EnsoContext.get(this).getBuiltins();
    return builtins.number().getNumber() == type
        || builtins.number().getFloat() == type
        || builtins.number().getInteger() == type
        || builtins.nothing() == type
        || builtins.text() == type
        || builtins.bool().getType() == type
        || builtins.date() == type
        || builtins.dateTime() == type
        || builtins.duration() == type
        || builtins.vector() == type;
  }

  private boolean isTrue(Object object) {
    return Boolean.TRUE.equals(object);
  }

  /** Returns true iff the given array of comparators is all Default_Comparator */
  boolean areAllDefaultComparators(
      ArrayLikeLengthNode lengthNode, ArrayLikeAtNode atNode, Object comparators) {
    var ctx = EnsoContext.get(this);
    var longSize = 0L;
    try {
      longSize = lengthNode.executeLength(comparators);
      int compSize = (int) longSize;
      for (int i = 0; i < compSize; i++) {
        Object comparator = atNode.executeAt(comparators, i);
        if (!isDefaultComparator(comparator, ctx)) {
          return false;
        }
      }
    } catch (ArithmeticException | InvalidArrayIndexException e) {
      throw invalidArrayIndexException(e, longSize);
    }
    return true;
  }

  boolean isDefaultComparator(Object object, EnsoContext ctx) {
    return ctx.getBuiltins().defaultComparator().getType() == object;
  }

  private boolean isNan(Object object) {
    return object instanceof Double dbl && dbl.isNaN();
  }

  private PanicException invalidArrayIndexException(Exception e, long size) {
    var index = e instanceof InvalidArrayIndexException ex ? ex.getInvalidIndex() : 0L;
    var ctx = EnsoContext.get(this);
    var payload = ctx.getBuiltins().error().makeInvalidArrayIndex(index, size);
    throw new PanicException(payload, this);
  }

  private enum ProblemBehavior {
    IGNORE,
    REPORT_WARNING,
    REPORT_ERROR;

    static ProblemBehavior fromInt(int intValue) {
      return switch (intValue) {
        case 0 -> IGNORE;
        case 1 -> REPORT_WARNING;
        case 2 -> REPORT_ERROR;
        default -> throw new IllegalArgumentException(Integer.toString(intValue));
      };
    }
  }

  /**
   * Group of elements grouped by comparator.
   *
   * @param elems Elements of the group.
   * @param comparator SortComparator for the elems, i.e., it should hold that {@code elems.each
   *     it-> (Comparable.from it) == comparator}.
   * @param compareFunc `SortComparator.compare` function extracted from the comparator.
   */
  private record Group(List<Object> elems, Type comparator, Function compareFunc) {}

  /**
   * Convenient base class that implements java.util.Comparator, and gathers warnings about
   * incomparable values. The warnings are gathered as pure Strings in a hash set, so that they are
   * not duplicated.
   */
  private abstract static class SortComparator implements java.util.Comparator<Object> {

    private final Set<String> warnings = new HashSet<>();
    final AnyToTextNode toTextNode;
    final ProblemBehavior problemBehavior;
    final InteropLibrary interop;

    protected SortComparator(
        AnyToTextNode toTextNode, ProblemBehavior problemBehavior, InteropLibrary interop) {
      this.toTextNode = toTextNode;
      this.problemBehavior = problemBehavior;
      this.interop = interop;
    }

    @TruffleBoundary
    protected void attachIncomparableValuesWarning(Object x, Object y) {
      var xStr = toTextNode.execute(x).toString();
      var yStr = toTextNode.execute(y).toString();
      String warnText = "Values " + xStr + " and " + yStr + " are incomparable";
      warnings.add(warnText);
    }

    public Set<String> getEncounteredWarnings() {
      return warnings;
    }

    @TruffleBoundary
    public boolean hasWarnings() {
      return !warnings.isEmpty();
    }
  }

  /**
   * SortComparator for comparing all values that have Default_Comparator. These are either
   * primitive types, or the types that do not provide their own comparator.
   *
   * <p>Note that it is essential for this class that the {@code by} method parameter to {@code
   * Vector.sort} is set to the default value, which is {@code Ordering.compare}, because then, we
   * know that the partial ordering for primitive types was not redefined by the user (we handle
   * partial ordering for primitive types specifically, partial ordering for other types is not
   * implemented yet - that requires topological sorting).
   */
  final class DefaultSortComparator extends SortComparator {

    private final LessThanNode lessThanNode;
    private final EqualsNode equalsNode;
    private final TypeOfNode typeOfNode;
    private final boolean ascending;

    private DefaultSortComparator(
        LessThanNode lessThanNode,
        EqualsNode equalsNode,
        TypeOfNode typeOfNode,
        AnyToTextNode toTextNode,
        boolean ascending,
        ProblemBehavior problemBehavior,
        InteropLibrary interop) {
      super(toTextNode, problemBehavior, interop);
      this.lessThanNode = lessThanNode;
      this.equalsNode = equalsNode;
      this.typeOfNode = typeOfNode;
      this.ascending = ascending;
    }

    @Override
    public int compare(Object x, Object y) {
      return compareValuesWithDefaultComparator(x, y);
    }

    int compareValuesWithDefaultComparator(Object x, Object y) {
      if (equalsNode.execute(x, y)) {
        return 0;
      } else {
        // Check if x < y
        Object xLessThanYRes = lessThanNode.execute(x, y);
        if (interop.isNull(xLessThanYRes)) {
          // x and y are incomparable - this can happen if x and y are different types
          return handleIncomparableValues(x, y);
        } else if (isTrue(xLessThanYRes)) {
          return ascending ? -1 : 1;
        } else {
          // Check if x > y
          Object yLessThanXRes = lessThanNode.execute(y, x);
          if (isTrue(yLessThanXRes)) {
            return ascending ? 1 : -1;
          } else {
            // yLessThanXRes is either Nothing or False
            return handleIncomparableValues(y, x);
          }
        }
      }
    }

    private int handleIncomparableValues(Object x, Object y) {
      switch (problemBehavior) {
        case REPORT_ERROR -> throw new CompareException(x, y);
        case REPORT_WARNING -> attachIncomparableValuesWarning(x, y);
      }
      if (isPrimitiveValue(x) || isPrimitiveValue(y)) {
        if (isPrimitiveValue(x) && isPrimitiveValue(y)) {
          return handleIncomparablePrimitives(x, y);
        } else if (isPrimitiveValue(x)) {
          // Primitive values are always before non-primitive values - Default_Comparator
          // group should be the first one.
          return ascending ? -1 : 1;
        } else if (isPrimitiveValue(y)) {
          return ascending ? 1 : -1;
        } else {
          throw CompilerDirectives.shouldNotReachHere();
        }
      } else {
        // Values other than primitives are compared just by their type's FQN.
        var xTypeName = getQualifiedTypeName(x);
        var yTypeName = getQualifiedTypeName(y);
        return xTypeName.compareTo(yTypeName);
      }
    }

    /**
     * Incomparable primitive values have either different builtin types, or are Nothing or NaN. All
     * these cases are handled specifically - we hardcode the order of these incomparable values.
     */
    private int handleIncomparablePrimitives(Object x, Object y) {
      int xCost = getPrimitiveValueCost(x);
      int yCost = getPrimitiveValueCost(y);
      int res = Integer.compare(xCost, yCost);
      return ascending ? res : -res;
    }

    private boolean isPrimitiveValue(Object object) {
      return isBuiltinType(typeOfNode.execute(object));
    }

    private String getQualifiedTypeName(Object object) {
      var typeObj = typeOfNode.execute(object);
      return toTextNode.execute(typeObj).toString();
    }

    private int getPrimitiveValueCost(Object object) {
      if (interop.isNull(object)) {
        return 200;
      } else if (isNan(object)) {
        return 100;
      } else {
        var type = typeOfNode.execute(object);
        return getBuiltinTypeCost(type);
      }
    }
  }

  /**
   * Helper class that returns the comparator function.
   *
   * <p>The class is introduced to handle the presence of {@code UnresolvedSymbol}, as the
   * comparator function, which has to be first resolved before it can be used to compare values.
   */
  private abstract class Compare {

    /**
     * Test if the comparator function has self argument.
     *
     * @param definedOn the value on which the function is defined on.
     * @return true if self argument is present, false otherwise.
     */
    abstract boolean hasFunctionSelfArgument(Object definedOn);

    /**
     * Return a comparator function.
     *
     * @param arg the value on which the function is defined on.
     * @return a non-null comparator function.
     */
    abstract Function get(Object arg);
  }

  private final class CompareFromFunction extends Compare {

    private final Function function;

    private CompareFromFunction(Function function) {
      this.function = function;
    }

    @Override
    boolean hasFunctionSelfArgument(Object definedOn) {
      if (function.getSchema().getArgumentsCount() > 0) {
        return function.getSchema().getArgumentInfos()[0].getName().equals("self");
      } else {
        return false;
      }
    }

    @Override
    Function get(Object arg) {
      return function;
    }
  }

  private class CompareFromUnresolvedSymbol extends Compare {

    private final UnresolvedSymbol unresolvedSymbol;
    private final MethodResolverNode methodResolverNode;
    private final TypesLibrary typesLibrary;

    private CompareFromUnresolvedSymbol(
        UnresolvedSymbol unresolvedSymbol,
        MethodResolverNode methodResolvedNode,
        TypesLibrary typesLibrary) {
      this.unresolvedSymbol = unresolvedSymbol;
      this.methodResolverNode = methodResolvedNode;
      this.typesLibrary = typesLibrary;
    }

    @Override
    boolean hasFunctionSelfArgument(Object definedOn) {
      var resolvedFunction =
          methodResolverNode.expectNonNull(
              definedOn, typesLibrary.getType(definedOn), unresolvedSymbol);
      return resolvedFunction.getSchema().getArgumentsCount() > 0
          && resolvedFunction.getSchema().getArgumentInfos()[0].getName().equals("self");
    }

    @Override
    Function get(Object arg) {
      return methodResolverNode.expectNonNull(arg, typesLibrary.getType(arg), unresolvedSymbol);
    }
  }

  /**
   * Comparator for any values. This comparator compares the values by calling back to Enso (by
   * {@link #compareFunc}), rather than using compare nodes (i.e. {@link LessThanNode}). directly,
   * as opposed to {@link DefaultSortComparator}.
   */
  private final class GenericSortComparator extends SortComparator {

    private final boolean ascending;

    /**
     * Either function from `by` parameter to the `Vector.sort` method, or the `compare` function
     * extracted from the comparator for the appropriate group.
     */
    private final Compare compareFunc;

    private final Compare onFunc;
    private final boolean hasCustomOnFunc;
    private final Type comparator;
    private final CallOptimiserNode callNode;
    private final State state;
    private final Atom less;
    private final Atom equal;
    private final Atom greater;

    private GenericSortComparator(
        boolean ascending,
        Object compareFunc,
        Object onFunc,
        ProblemBehavior problemBehavior,
        Type comparator,
        CallOptimiserNode callNode,
        AnyToTextNode toTextNode,
        State state,
        Atom less,
        Atom equal,
        Atom greater,
        InteropLibrary interop,
        TypesLibrary typesLibrary,
        MethodResolverNode methodResolverNode) {
      super(toTextNode, problemBehavior, interop);
      assert compareFunc != null;
      assert comparator != null;
      this.comparator = comparator;
      this.state = state;
      this.ascending = ascending;
      this.compareFunc = checkAndConvertByFunc(compareFunc, typesLibrary, methodResolverNode);
      if (interop.isNull(onFunc)) {
        this.hasCustomOnFunc = false;
        this.onFunc = null;
      } else {
        this.hasCustomOnFunc = true;
        this.onFunc = checkAndConvertOnFunc(onFunc, typesLibrary, methodResolverNode);
      }
      this.callNode = callNode;
      this.less = less;
      this.equal = equal;
      this.greater = greater;
    }

    @Override
    public int compare(Object x, Object y) {
      Object xConverted;
      Object yConverted;
      if (hasCustomOnFunc) {
        // onFunc cannot have `self` argument, we assume it has just one argument.
        xConverted =
            callNode.executeDispatch(null, onFunc.get(x), null, state, new Object[] {x}, null);
        yConverted =
            callNode.executeDispatch(null, onFunc.get(y), null, state, new Object[] {y}, null);
      } else {
        xConverted = x;
        yConverted = y;
      }
      Object[] args;
      if (compareFunc.hasFunctionSelfArgument(xConverted)) {
        args = new Object[] {comparator, xConverted, yConverted};
      } else {
        args = new Object[] {xConverted, yConverted};
      }
      Object res =
          callNode.executeDispatch(null, compareFunc.get(xConverted), null, state, args, null);
      if (res == less) {
        return ascending ? -1 : 1;
      } else if (res == equal) {
        return 0;
      } else if (res == greater) {
        return ascending ? 1 : -1;
      } else {
        // res is either Nothing, or Incomparable_Values. Either way, it means that x and y are
        // incomparable.
        // This case is not supported yet, as it requires topological sorting.
        // We cannot detect if the result was actually returned from the default comparator (it
        // could have been transitively called), so we just bailout.
        throw new CompareException(x, y);
      }
    }

    /**
     * Checks value given for {@code by} parameter and converts it to {@link Function}. Throw a
     * dataflow error otherwise.
     */
    private Compare checkAndConvertByFunc(
        Object byFuncObj, TypesLibrary typesLibrary, MethodResolverNode methodResolverNode) {
      return checkAndConvertFunction(
          byFuncObj,
          "Unsupported argument for `by`, expected a method with two arguments",
          2,
          3,
          typesLibrary,
          methodResolverNode);
    }

    /**
     * Checks the value given for {@code on} parameter and converts it to {@link Function}. Throws a
     * dataflow error otherwise.
     */
    private Compare checkAndConvertOnFunc(
        Object onFuncObj, TypesLibrary typesLibrary, MethodResolverNode methodResolverNode) {
      return checkAndConvertFunction(
          onFuncObj,
          "Unsupported argument for `on`, expected a method with one argument",
          1,
          1,
          typesLibrary,
          methodResolverNode);
    }

    /**
     * @param minArgCount Minimal count of arguments without a default value.
     * @param maxArgCount Maximal count of argument without a default value.
     * @param methodResolverNode node for resolving unresolved symbols.
     * @param typesLibrary types library for resolving the dispatch type for unresolved symbols.
     */
    private Compare checkAndConvertFunction(
        Object funcObj,
        String errMsg,
        int minArgCount,
        int maxArgCount,
        TypesLibrary typesLibrary,
        MethodResolverNode methodResolverNode) {
      if (funcObj instanceof UnresolvedSymbol unresolved) {
        return new CompareFromUnresolvedSymbol(unresolved, methodResolverNode, typesLibrary);
      }
      var err = new IllegalArgumentException(errMsg + ", got " + funcObj);
      if (funcObj instanceof Function func) {
        var argCount = getNumberOfNonDefaultArguments(func);
        if (minArgCount <= argCount && argCount <= maxArgCount) {
          return new CompareFromFunction(func);
        } else {
          throw err;
        }
      } else {
        throw err;
      }
    }
  }

  private static int getNumberOfNonDefaultArguments(Function function) {
    return (int)
        Arrays.stream(function.getSchema().getArgumentInfos())
            .filter(argInfo -> !argInfo.hasDefaultValue())
            .count();
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
