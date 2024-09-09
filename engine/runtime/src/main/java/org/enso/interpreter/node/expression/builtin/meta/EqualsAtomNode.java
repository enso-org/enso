package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.ordering.CustomComparatorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.StructsLibrary;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

@GenerateUncached
public abstract class EqualsAtomNode extends Node {

  public static EqualsAtomNode build() {
    return EqualsAtomNodeGen.create();
  }

  public abstract EqualsAndInfo execute(VirtualFrame frame, Atom left, Atom right);

  static EqualsSimpleNode[] createEqualsNodes(int size) {
    var nodes = new EqualsSimpleNode[size];
    for (var i = 0; i < size; i++) {
      nodes[i] = EqualsSimpleNode.build();
    }
    return nodes;
  }

  @Specialization(
      guards = {
        "selfCtorCached == self.getConstructor()",
        "customComparatorNode.execute(self) == null"
      },
      limit = "10")
  @ExplodeLoop
  EqualsAndInfo equalsAtomsWithDefaultComparator(
      VirtualFrame frame,
      Atom self,
      Atom other,
      @Cached("self.getConstructor()") AtomConstructor selfCtorCached,
      @Cached(value = "selfCtorCached.getFields().length", allowUncached = true)
          int fieldsLenCached,
      @Cached(value = "createEqualsNodes(fieldsLenCached)", allowUncached = true)
          EqualsSimpleNode[] fieldEqualsNodes,
      @Shared("customComparatorNode") @Cached CustomComparatorNode customComparatorNode,
      @Cached ConditionProfile constructorsNotEqualProfile,
      @CachedLibrary(limit = "5") StructsLibrary structsLib) {
    if (constructorsNotEqualProfile.profile(self.getConstructor() != other.getConstructor())) {
      return EqualsAndInfo.FALSE;
    }
    CompilerAsserts.partialEvaluationConstant(fieldsLenCached);
    for (int i = 0; i < fieldsLenCached; i++) {
      var selfValue = structsLib.getField(self, i);
      var otherValue = structsLib.getField(other, i);
      var fieldsAreEqual = fieldEqualsNodes[i].execute(frame, selfValue, otherValue);
      if (!fieldsAreEqual.equals()) {
        return fieldsAreEqual;
      }
    }
    return EqualsAndInfo.TRUE;
  }

  @Specialization(
      guards = {
        "selfCtorCached == self.getConstructor()",
        "cachedComparator != null",
      },
      limit = "10")
  EqualsAndInfo equalsAtomsWithCustomComparator(
      Atom self,
      Atom other,
      @Cached("self.getConstructor()") AtomConstructor selfCtorCached,
      @Shared("customComparatorNode") @Cached CustomComparatorNode customComparatorNode,
      @Cached(value = "customComparatorNode.execute(self)") Type cachedComparator,
      @Cached(value = "findCompareMethod(cachedComparator)", allowUncached = true)
          Function compareFn,
      @Cached(value = "invokeCompareNode(compareFn)") InvokeFunctionNode invokeNode,
      @Shared @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Cached BranchProfile warningsPresent) {
    try {
      var otherComparator = customComparatorNode.execute(other);
      if (cachedComparator != otherComparator) {
        return EqualsAndInfo.FALSE;
      }
      var ctx = EnsoContext.get(this);
      var args = new Object[] {cachedComparator, self, other};
      var result = invokeNode.execute(compareFn, null, State.create(ctx), args);
      assert orderingOrNullOrError(this, ctx, result, compareFn);
      if (warnings.hasWarnings(result)) {
        warningsPresent.enter();
        var map = warnings.getWarnings(result, false);
        result = warnings.removeWarnings(result);
        var eq = ctx.getBuiltins().ordering().newEqual() == result;
        return new EqualsAndInfo(eq, map);
      } else {
        return EqualsAndInfo.valueOf(ctx.getBuiltins().ordering().newEqual() == result);
      }
    } catch (UnsupportedMessageException e) {
      throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);
    }
  }

  @TruffleBoundary
  private static boolean orderingOrNullOrError(
      Node where, EnsoContext ctx, Object obj, Function fn) {
    var type = TypeOfNode.getUncached().execute(obj);
    if (type == ctx.getBuiltins().ordering().getType()) {
      return true;
    }
    if (type == ctx.getBuiltins().nothing()) {
      return true;
    }
    if (type == ctx.getBuiltins().dataflowError()) {
      return true;
    }
    var msg =
        "Expecting Ordering or Nothing, but got: " + obj + " with type " + type + " calling " + fn;
    throw ctx.raiseAssertionPanic(where, msg, null);
  }

  @Specialization(
      replaces = {"equalsAtomsWithDefaultComparator", "equalsAtomsWithCustomComparator"})
  EqualsAndInfo equalsAtomsUncached(
      VirtualFrame frame,
      Atom self,
      Atom other,
      @Shared @CachedLibrary(limit = "10") WarningsLibrary warnings) {
    if (self.getConstructor() != other.getConstructor()) {
      return EqualsAndInfo.FALSE;
    } else {
      return equalsAtomsUncached(frame == null ? null : frame.materialize(), self, other, warnings);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private EqualsAndInfo equalsAtomsUncached(
      MaterializedFrame frame, Atom self, Atom other, WarningsLibrary warnings) {
    Type customComparator = CustomComparatorNode.getUncached().execute(self);
    if (customComparator != null) {
      Function compareFunc = findCompareMethod(customComparator);
      var invokeFuncNode = invokeCompareNode(compareFunc);
      return equalsAtomsWithCustomComparator(
          self,
          other,
          self.getConstructor(),
          CustomComparatorNode.getUncached(),
          customComparator,
          compareFunc,
          invokeFuncNode,
          warnings,
          BranchProfile.getUncached());
    }
    for (int i = 0; i < self.getConstructor().getArity(); i++) {
      var selfField = StructsLibrary.getUncached().getField(self, i);
      var otherField = StructsLibrary.getUncached().getField(other, i);
      var areFieldsSame = EqualsSimpleNode.getUncached().execute(frame, selfField, otherField);
      if (!areFieldsSame.equals()) {
        return areFieldsSame;
      }
    }
    return EqualsAndInfo.TRUE;
  }

  @TruffleBoundary
  static Function findCompareMethod(Type comparator) {
    var fn = comparator.getDefinitionScope().getMethodForType(comparator, "compare");
    if (fn == null) {
      throw new AssertionError("No compare function for " + comparator);
    }
    return fn;
  }

  static InvokeFunctionNode invokeCompareNode(Function compareFn) {
    CallArgumentInfo[] argsInfo = new CallArgumentInfo[compareFn.getSchema().getArgumentsCount()];
    for (int i = 0; i < argsInfo.length; i++) {
      var argDef = compareFn.getSchema().getArgumentInfos()[i];
      argsInfo[i] = new CallArgumentInfo(argDef.getName());
    }
    return InvokeFunctionNode.build(
        argsInfo, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.EXECUTE);
  }
}
