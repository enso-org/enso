package org.enso.interpreter.node.typecheck;

import java.util.Arrays;

import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.meta.IsValueOfTypeNode;
import org.enso.interpreter.node.expression.literal.LiteralNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.graalvm.collections.Pair;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.InvalidAssumptionException;
import com.oracle.truffle.api.nodes.Node;

non-sealed abstract class SingleTypeCheckNode extends AbstractTypeCheckNode {
  private final Type expectedType;
  @Node.Child IsValueOfTypeNode checkType;
  @CompilerDirectives.CompilationFinal private String expectedTypeMessage;
  @CompilerDirectives.CompilationFinal private LazyCheckRootNode lazyCheck;
  @Node.Child private EnsoMultiValue.CastToNode castTo;

  SingleTypeCheckNode(String name, Type expectedType) {
    super(name);
    this.checkType = IsValueOfTypeNode.build();
    this.expectedType = expectedType;
  }

  abstract Object executeCheckOrConversion(
      VirtualFrame frame, Object value, ExpressionNode valueSource);

  @Specialization
  Object doPanicSentinel(VirtualFrame frame, PanicSentinel panicSentinel, ExpressionNode ignore) {
    throw panicSentinel;
  }

  @Specialization
  Object doUnresolvedConstructor(
      VirtualFrame frame,
      UnresolvedConstructor unresolved,
      ExpressionNode ignore,
      @Cached UnresolvedConstructor.ConstructNode construct) {
    var state = Function.ArgumentsHelper.getState(frame.getArguments());
    return construct.execute(frame, state, expectedType, unresolved);
  }

  @Specialization(rewriteOn = InvalidAssumptionException.class)
  Object doCheckNoConversionNeeded(VirtualFrame frame, Object v, ExpressionNode ignore)
      throws InvalidAssumptionException {
    var ret = findDirectMatch(frame, v);
    if (ret != null) {
      return ret;
    } else {
      throw new InvalidAssumptionException();
    }
  }

  @Specialization(
      limit = "10",
      guards = {"cachedType != null", "findType(typeOfNode, v, cachedType) == cachedType"})
  Object doWithConversionCached(
      VirtualFrame frame,
      Object v,
      ExpressionNode valueSource,
      @Cached.Shared("typeOfNode") @Cached TypeOfNode typeOfNode,
      @Cached(value = "findType(typeOfNode, v)", dimensions = 1) Type[] cachedType,
      @Cached("findConversionNode(valueSource, cachedType)") ApplicationNode convertNode) {
    return handleWithConversion(frame, v, convertNode);
  }

  @Specialization(replaces = "doWithConversionCached")
  Object doWithConversionUncached(
      VirtualFrame frame,
      Object v,
      ExpressionNode expr,
      @Cached.Shared("typeOfNode") @Cached TypeOfNode typeOfNode) {
    var type = findType(typeOfNode, v);
    return doWithConversionUncachedBoundary(
        frame == null ? null : frame.materialize(), v, expr, type);
  }

  @ExplodeLoop
  final Object findDirectMatch(VirtualFrame frame, Object v) {
    if (isAllFitValue(v)) {
      return v;
    }
    if (v instanceof Function fn && fn.isThunk()) {
      if (lazyCheck == null) {
        CompilerDirectives.transferToInterpreter();
        var enso = EnsoLanguage.get(this);
        var node = (AbstractTypeCheckNode) copy();
        lazyCheck = new LazyCheckRootNode(enso, new TypeCheckValueNode(node, isAllTypes()));
      }
      var lazyCheckFn = lazyCheck.wrapThunk(fn);
      return lazyCheckFn;
    }
    assert EnsoContext.get(this).getBuiltins().any() != expectedType : "Don't check for Any: " + expectedType;
    if (v instanceof EnsoMultiValue mv) {
      if (castTo == null) {
        CompilerDirectives.transferToInterpreter();
        castTo = insert(EnsoMultiValue.CastToNode.create());
      }
      var result = castTo.findTypeOrNull(expectedType, mv, true, isAllTypes());
      if (result != null) {
        return result;
      }
    }
    if (checkType.execute(expectedType, v, isAllTypes())) {
      return v;
    }
    return null;
  }

  private Pair<Function, Type> findConversion(Type from) {
    if (expectedType == from) {
      return null;
    }
    var ctx = EnsoContext.get(this);

    if (getRootNode() instanceof EnsoRootNode root) {
      var convert = UnresolvedConversion.build(root.getModuleScope());
      var conv = convert.resolveFor(ctx, expectedType, from);
      if (conv != null) {
        return Pair.create(conv, expectedType);
      }
    }
    return null;
  }

  ApplicationNode findConversionNode(ExpressionNode valueNode, Type[] allTypes) {
    if (valueNode == null) {
      return null;
    }
    if (allTypes == null) {
      allTypes = new Type[] {null};
    }
    for (var from : allTypes) {
      var convAndType = findConversion(from);

      if (convAndType != null) {
        CompilerAsserts.neverPartOfCompilation();
        var convNode = LiteralNode.build(convAndType.getLeft());
        var intoNode = LiteralNode.build(convAndType.getRight());
        var args =
            new CallArgument[] {
              new CallArgument(null, intoNode), new CallArgument(null, valueNode)
            };
        return ApplicationNode.build(
            convNode, args, InvokeCallableNode.DefaultsExecutionMode.EXECUTE);
      }
    }
    return null;
  }

  final Type[] findType(TypeOfNode typeOfNode, Object v) {
    return findType(typeOfNode, v, null);
  }

  final Type[] findType(TypeOfNode typeOfNode, Object v, Type[] previous) {;
    if (v instanceof EnsoMultiValue multi) {
      var all = typeOfNode.findAllTypesOrNull(multi, false);
      return all;
    }
    if (v instanceof UnresolvedConstructor) {
      return null;
    }
    if (typeOfNode.findTypeOrError(v) instanceof Type from) {
      if (previous != null && previous.length == 1 && previous[0] == from) {
        return previous;
      } else {
        return new Type[] {from};
      }
    }
    return null;
  }

  private Object handleWithConversion(VirtualFrame frame, Object v, ApplicationNode convertNode)
      throws PanicException {
    if (convertNode == null) {
      var ret = findDirectMatch(frame, v);
      if (ret != null) {
        return ret;
      }
      return null;
    } else {
      var converted = convertNode.executeGeneric(frame);
      return converted;
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object doWithConversionUncachedBoundary(
      MaterializedFrame frame, Object v, ExpressionNode expr, Type[] type) {
    var convertNode = findConversionNode(expr, type);
    return handleWithConversion(frame, v, convertNode);
  }

  @Override
  String expectedTypeMessage() {
    if (expectedTypeMessage != null) {
      return expectedTypeMessage;
    }
    CompilerDirectives.transferToInterpreterAndInvalidate();
    expectedTypeMessage = expectedType.toString();
    return expectedTypeMessage;
  }
}
