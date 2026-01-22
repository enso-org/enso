package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.meta.IsValueOfTypeNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.FunctionAndType;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;

abstract non-sealed class SingleTypeCheckNode extends AbstractTypeCheckNode {
  private final Type expectedType;
  @Node.Child IsValueOfTypeNode checkType;
  @CompilerDirectives.CompilationFinal private String expectedTypeMessage;
  @Node.Child private EnsoMultiValue.CastToNode castTo;

  SingleTypeCheckNode(String name, Type expectedType) {
    super(name);
    this.checkType = IsValueOfTypeNode.build();
    this.expectedType = expectedType;
  }

  abstract Object executeConversion(VirtualFrame frame, Object value);

  @Specialization
  Object doPanicSentinel(VirtualFrame frame, PanicSentinel panicSentinel) {
    throw panicSentinel;
  }

  @Specialization
  Object doUnresolvedConstructor(
      VirtualFrame frame,
      UnresolvedConstructor unresolved,
      @Cached UnresolvedConstructor.ConstructNode construct) {
    var state = EnsoContext.get(this).currentState();
    return construct.execute(frame, state, expectedType, unresolved);
  }

  @Specialization(
      limit = "10",
      guards = {"cachedType != null", "findType(typeOfNode, v, cachedType) == cachedType"})
  Object doWithConversionCached(
      VirtualFrame frame,
      Object v,
      @Cached.Shared("typeOfNode") @Cached TypeOfNode typeOfNode,
      @Cached(value = "findType(typeOfNode, v)", dimensions = 1) Type[] cachedType,
      @Cached("findConversionNode(cachedType)") TypeToConvertNode node) {
    return handleWithConversion(frame, v, node);
  }

  @Specialization(replaces = "doWithConversionCached")
  Object doWithConversionUncached(
      VirtualFrame frame, Object v, @Cached.Shared("typeOfNode") @Cached TypeOfNode typeOfNode) {
    var type = findType(typeOfNode, v);
    return doWithConversionUncachedBoundary(frame == null ? null : frame.materialize(), v, type);
  }

  @Override
  final Object findDirectMatch(VirtualFrame frame, Object v) {
    return directMatchImpl(v);
  }

  @ExplodeLoop
  private final Object directMatchImpl(Object v) {
    if (v instanceof Function fn && fn.isFullyApplied()) {
      return fn;
    }
    assert EnsoContext.get(this).getBuiltins().any() != expectedType
        : "Don't check for Any: " + expectedType;
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

  private FunctionAndType findConversion(Type from) {
    if (expectedType == from) {
      return null;
    }
    var ctx = EnsoContext.get(this);

    if (getRootNode() instanceof EnsoRootNode root) {
      var convert = UnresolvedConversion.build(root.getModuleScope());
      var conv = convert.resolveFor(ctx, expectedType, from);
      if (conv != null) {
        return new FunctionAndType(conv, expectedType);
      }
    }
    return null;
  }

  static final class TypeToConvertNode extends Node {
    final Function conv;
    final Type intoType;
    @Child InvokeFunctionNode invokeNode;

    private TypeToConvertNode(Function conv, Type intoType) {
      this.conv = conv;
      this.intoType = intoType;
      this.invokeNode = InvokeFunctionNode.buildWithArity(2);
    }

    final Object executeConvert(VirtualFrame frame, Object value) {
      var ctx = EnsoContext.get(this);
      var state = ctx.currentState();
      return invokeNode.execute(conv, frame, state, new Object[] {intoType, value});
    }
  }

  final TypeToConvertNode findConversionNode(Type[] allTypes) {
    if (allTypes == null) {
      allTypes = new Type[] {null};
    }
    for (var from : allTypes) {
      var convAndType = findConversion(from);

      if (convAndType != null) {
        CompilerAsserts.neverPartOfCompilation();
        var confFn = convAndType.function();
        var intoType = convAndType.type();
        return new TypeToConvertNode(confFn, intoType);
      }
    }
    return null;
  }

  final Type[] findType(TypeOfNode typeOfNode, Object v) {
    return findType(typeOfNode, v, null);
  }

  final Type[] findType(TypeOfNode typeOfNode, Object v, Type[] previous) {
    ;
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

  private Object handleWithConversion(VirtualFrame frame, Object v, TypeToConvertNode convertPair)
      throws PanicException {
    if (convertPair == null) {
      return directMatchImpl(v);
    } else {
      var converted = convertPair.executeConvert(frame, v);
      return converted;
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object doWithConversionUncachedBoundary(MaterializedFrame frame, Object v, Type[] type) {
    var c = findConversionNode(type);
    return handleWithConversion(frame, v, c);
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
