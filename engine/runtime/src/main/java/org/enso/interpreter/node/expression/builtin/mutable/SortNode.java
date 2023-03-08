package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.LoopNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;

import java.util.Arrays;
import java.util.Comparator;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.node.callable.dispatch.SimpleCallOptimiserNode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(type = "Array", name = "sort_builtin", description = "Returns a sorted array.")
public abstract class SortNode extends Node {
  private @Child CallOptimiserNode callOptimiserNode = SimpleCallOptimiserNode.build();
  private @Child InvalidComparisonNode invalidComparisonNode = InvalidComparisonNode.build();
  private final BranchProfile invalidCompareResultProfile = BranchProfile.create();

  abstract Object execute(State state, Object self, Object comparator);

  static SortNode build() {
    return SortNodeGen.create();
  }

  @Specialization
  Object doArray(State state, Array self, Function comparator) {
    EnsoContext context = EnsoContext.get(this);
    int size = self.getItems().length;
    Object[] newArr = new Object[size];
    System.arraycopy(self.getItems(), 0, newArr, 0, size);

    try {
      return getComparatorAndSort(state, newArr, comparator, context);
    } catch (CompareException e) {
      return DataflowError.withoutTrace(
          incomparableValuesError(e.leftOperand, e.rightOperand), this);
    }
  }

  @Specialization(guards = "arrays.hasArrayElements(self)")
  Object doPolyglotArray(
      State state,
      Object self,
      Function comparator,
      @CachedLibrary(limit = "3") InteropLibrary arrays,
      @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    long size;
    Object[] newArray;
    try {
      size = arrays.getArraySize(self);
      newArray = new Object[(int) size];
      for (int i = 0; i < size; i++) {
        newArray[i] = hostValueToEnsoNode.execute(arrays.readArrayElement(self, i));
      }
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }

    try {
      return getComparatorAndSort(state, newArray, comparator, EnsoContext.get(this));
    } catch (CompareException e) {
      return DataflowError.withoutTrace(
          incomparableValuesError(e.leftOperand, e.rightOperand), this);
    }
  }

  @Fallback
  Object doOther(State state, Object self, Object comparator) {
    CompilerDirectives.transferToInterpreter();
    var fun = EnsoContext.get(this).getBuiltins().function();
    throw new PanicException(
        EnsoContext.get(this).getBuiltins().error().makeTypeError(fun, comparator, "comparator"),
        this);
  }

  private Object getComparatorAndSort(
      State state, Object[] rawItems, Function comparator, EnsoContext context) {
    Comparator<Object> compare = new SortComparator(comparator, context, this, state);
    runSort(compare, rawItems);
    return new Array(rawItems);
  }

  Object[] runSort(Comparator<Object> compare, Object[] self) {
    doSort(self, compare);
    LoopNode.reportLoopCount(this, self.length);
    return self;
  }

  @TruffleBoundary
  void doSort(Object[] items, Comparator<Object> compare) {
    Arrays.sort(items, compare);
  }

  private Object incomparableValuesError(Object left, Object right) {
    return EnsoContext.get(this).getBuiltins().error().makeIncomparableValues(left, right);
  }

  private class SortComparator implements Comparator<Object> {
    private final Function compFn;
    private final EnsoContext context;
    private final Atom less;
    private final Atom equal;
    private final Atom greater;
    private final SortNode outerThis;
    private final State state;

    SortComparator(Function compFn, EnsoContext context, SortNode outerThis, State state) {
      this.compFn = compFn;
      this.context = context;
      this.less = context.getBuiltins().ordering().newLess();
      this.equal = context.getBuiltins().ordering().newEqual();
      this.greater = context.getBuiltins().ordering().newGreater();
      this.outerThis = outerThis;
      this.state = state;
    }

    @Override
    public int compare(Object o1, Object o2) {
      Object res = callOptimiserNode.executeDispatch(compFn, null, state, new Object[] {o1, o2});
      if (res == less) {
        return -1;
      } else if (res == equal) {
        return 0;
      } else if (res == greater) {
        return 1;
      } else {
        invalidCompareResultProfile.enter();
        throw new CompareException(o1, o2);
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
