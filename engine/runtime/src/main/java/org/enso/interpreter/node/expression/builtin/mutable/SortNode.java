package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.locks.Condition;
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
  private final ConditionProfile consProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile lessProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile equalsProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile greaterProfile = ConditionProfile.createCountingProfile();

  abstract Object execute(VirtualFrame frame, Object _this, Object comparator);

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
    Comparator<Object> compare =
        (l, r) -> {
          Stateful result =
              invokeNode.execute(comparator, frame, EmptyMap.create(), new Object[] {l, r});
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
          var ordering = ctxRef.get().getBuiltins().ordering().ordering();
          throw new PanicException(
              ctxRef.get().getBuiltins().error().makeTypeError(ordering, result.getValue()), this);
        };
    doSort(_this.getItems(), compare);
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

  InvokeCallableNode buildInvokeNode() {
    CallArgumentInfo[] callArguments = {new CallArgumentInfo(), new CallArgumentInfo()};
    return InvokeCallableNode.build(
        callArguments, DefaultsExecutionMode.IGNORE, ArgumentsExecutionMode.EXECUTE);
  }
}
