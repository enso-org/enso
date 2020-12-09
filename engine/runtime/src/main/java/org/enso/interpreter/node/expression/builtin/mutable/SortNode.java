package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.util.Arrays;
import java.util.Comparator;
import org.enso.compiler.exception.CompilerError;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.state.data.EmptyMap;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(type = "Array", name = "sort", description = "Sorts a mutable array in place.")
public abstract class SortNode extends Node {
  private @Child InvokeCallableNode invokeNode;

  private final ConditionProfile consProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile lessProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile equalsProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile greaterProfile = ConditionProfile.createCountingProfile();

  private volatile int ctr = 0;

  abstract Object execute(VirtualFrame frame, Object _this, Object comparator);

  SortNode() {
    invokeNode = buildInvokeNode();
  }

  static SortNode build() {
    return SortNodeGen.create();
  }

  @Specialization
  Object doSort(
      VirtualFrame frame,
      Array _this,
      Object comparator,
      @Cached("buildInvokeNode()") InvokeCallableNode invokeNode,
      @CachedContext(Language.class) ContextReference<Context> ctxRef,
      @Cached("ctxRef.get().getBuiltins().ordering().newLess()") Atom less,
      @Cached("ctxRef.get().getBuiltins().ordering().newEqual()") Atom equal,
      @Cached("ctxRef.get().getBuiltins().ordering().newGreater()") Atom greater) {
    Object[] items = _this.getItems();

    Comparator<Object> comp = (Object l, Object r) -> { return ((Long) l).compareTo((Long) r);};

    for (int i = 0; i < items.length - 1; ++i) {
      ctr += comp.compare(items[i], items[i+1]);
//      ctr += compare(frame, comparator, items[i], items[i+1], less, equal, greater);
    }

//    if (items.length >= 1) {
//      sort(items, frame, comparator, less, equal, greater);
//    }

    //    Comparator<Object> compare =
    //        (l, r) -> {
    //          Stateful result =
    //              invokeNode.execute(comparator, frame, EmptyMap.create(), new Object[] {l, r});
    //          if (TypesGen.isAtom(result.getValue())) {
    //            Atom atom = TypesGen.asAtom(result.getValue());
    //            if (lessProfile.profile(atom == less)) {
    //              return -1;
    //            } else if (equalsProfile.profile(atom == equal)) {
    //              return 0;
    //            } else if (greaterProfile.profile(atom == greater)) {
    //              return 1;
    //            }
    //          }
    //
    //          CompilerDirectives.transferToInterpreter();
    //          var ordering = ctxRef.get().getBuiltins().ordering().ordering();
    //          throw new PanicException(
    //              ctxRef.get().getBuiltins().error().makeTypeError(ordering, result.getValue()),
    // this);
    //        };
    //    doSort(_this.getItems(), compare);
    return ctxRef.get().getBuiltins().nothing().newInstance();
  }

  @Specialization
  Object doAtomThis(
      VirtualFrame frame,
      Atom _this,
      Object that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef,
      @Cached("ctxRef.get().getBuiltins().mutable().array()") AtomConstructor array) {
    if (consProfile.profile(array == _this.getConstructor())) {
      return ctxRef.get().getBuiltins().nothing().newInstance();
    } else {
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(
          ctxRef.get().getBuiltins().error().makeTypeError(array, _this), this);
    }
  }

  void doSort(Object[] items, Comparator<Object> compare) {
    Arrays.sort(items, compare);
  }

  void sort(
      Object[] items, VirtualFrame frame, Object comparator, Atom less, Atom equal, Atom greater) {
    Comparator<Object> comp = (l, r) -> { return ((Long) l).compareTo((Long) r);};
    Arrays.sort(items, comp);
//    quicksort(items, frame, comparator, 0, items.length - 1, less, equal, greater);
  }

  void quicksort(
      Object[] items,
      VirtualFrame frame,
      Object comparator,
      int low,
      int high,
      Atom less,
      Atom equal,
      Atom greater) {
    if (low < high) {
      int partitionIx = partition(items, frame, comparator, low, high, less, equal, greater);
      quicksort(items, frame, comparator, low, partitionIx, less, equal, greater);
      quicksort(items, frame, comparator, partitionIx + 1, high, less, equal, greater);
    }
  }

  int partition(
      Object[] items,
      VirtualFrame frame,
      Object comparator,
      int low,
      int high,
      Atom less,
      Atom equal,
      Atom greater) {
    Object pivot = items[(high + low) / 2];
    int i = low - 1;
    int j = high + 1;

    while (true) {
      do {
        i += 1;
      } while ((Long) items[i] < (Long) pivot);
      //      } while (compare(frame, comparator, items[i], pivot, less, equal, greater) < 0);

      do {
        j -= 1;
      } while ((Long) items[j] > (Long) pivot);
      //      } while (compare(frame, comparator, items[j], pivot, less, equal, greater) > 0);

      if (i >= j) {
        return j;
      }

      Object temp = items[i];
      items[i] = items[j];
      items[j] = temp;
    }
  }

  int compare(
      VirtualFrame frame,
      Object comparator,
      Object left,
      Object right,
      Atom less,
      Atom equal,
      Atom greater) {
    Stateful result =
        invokeNode.execute(comparator, frame, EmptyMap.create(), new Object[] {left, right});

    if (TypesGen.isAtom(result.getValue())) {
      Atom atom = TypesGen.asAtom(result.getValue());
      if (lessProfile.profile(atom == less)) {
        return -1;
      } else if (equalsProfile.profile(atom == equal)) {
        return 0;
      } else if (greaterProfile.profile(atom == greater)) {
        return 1;
      }
    }

    CompilerDirectives.transferToInterpreter();
    throw new CompilerError("AAAAAAAAAAAAAAAAAA");
  }

  InvokeCallableNode buildInvokeNode() {
    CallArgumentInfo[] callArguments = {new CallArgumentInfo(), new CallArgumentInfo()};
    return InvokeCallableNode.build(
        callArguments, DefaultsExecutionMode.IGNORE, ArgumentsExecutionMode.EXECUTE);
  }
}
