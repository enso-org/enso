package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.LoopNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.util.Arrays;
import java.util.Comparator;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.node.callable.dispatch.SimpleCallOptimiserNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.state.data.EmptyMap;

@BuiltinMethod(type = "Array", name = "sort", description = "Sorts a mutable array in place.")
public abstract class SortNode extends Node {
  private @Child ComparatorNode comparatorNode = ComparatorNode.build();
  private @Child CallOptimiserNode callOptimiserNode = SimpleCallOptimiserNode.build();
  private final BranchProfile resultProfile = BranchProfile.create();

  abstract Object execute(VirtualFrame frame, Object _this, Object comparator);

  static SortNode build() {
    return SortNodeGen.create();
  }

  @Specialization
  Object doSortFunction(
      VirtualFrame frame,
      Array _this,
      Function comparator,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    Comparator<Object> compare = getComparator(comparator, ctxRef);
    return runSort(compare, _this, ctxRef);
  }

  @Specialization
  Object doSortCallable(
      VirtualFrame frame,
      Array _this,
      Object comparator,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    Comparator<Object> compare = (l, r) -> comparatorNode.execute(frame, comparator, l, r);
    return runSort(compare, _this, ctxRef);
  }

  @Specialization
  Object doAtomThis(
      VirtualFrame frame,
      Atom _this,
      Object that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef,
      @Cached("ctxRef.get().getBuiltins().mutable().array()") AtomConstructor array) {
    return ctxRef.get().getBuiltins().nothing().newInstance();
  }

  Object runSort(Comparator<Object> compare, Array _this, ContextReference<Context> ctxRef) {
    doSort(_this.getItems(), compare);
    LoopNode.reportLoopCount(this, _this.length());
    return ctxRef.get().getBuiltins().nothing().newInstance();
  }

  @TruffleBoundary
  void doSort(Object[] items, Comparator<Object> compare) {
    Arrays.sort(items, compare);
  }

  private SortComparator getComparator(Function comp, ContextReference<Context> ctxRef) {
    return new SortComparator(comp, ctxRef, this);
  }

  private class SortComparator implements Comparator<Object> {
    private final Function compFn;
    private final ContextReference<Context> ctxRef;
    private final Atom less;
    private final Atom equal;
    private final Atom greater;
    private final SortNode outerThis;

    SortComparator(Function compFn, ContextReference<Context> ctxRef, SortNode outerThis) {
      this.compFn = compFn;
      this.ctxRef = ctxRef;
      this.less = ctxRef.get().getBuiltins().ordering().newLess();
      this.equal = ctxRef.get().getBuiltins().ordering().newEqual();
      this.greater = ctxRef.get().getBuiltins().ordering().newGreater();
      this.outerThis = outerThis;
    }

    @Override
    public int compare(Object o1, Object o2) {
      Stateful result =
          callOptimiserNode.executeDispatch(compFn, null, EmptyMap.create(), new Object[] {o1, o2});
      Object value = result.getValue();
      return convertResult(value);
    }

    private int convertResult(Object res) {
      if (res == less) {
        return -1;
      } else if (res == equal) {
        return 0;
      } else if (res == greater) {
        return 1;
      } else {
        resultProfile.enter();
        var ordering = ctxRef.get().getBuiltins().ordering().ordering();
        throw new PanicException(
            ctxRef.get().getBuiltins().error().makeTypeError(ordering, res, "result"), outerThis);
      }
    }
  }
}
