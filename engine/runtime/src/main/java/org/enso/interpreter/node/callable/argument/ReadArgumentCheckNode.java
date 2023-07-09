package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.expression.builtin.meta.IsValueOfTypeNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.literal.LiteralNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.graalvm.collections.Pair;

public abstract class ReadArgumentCheckNode extends Node {
  private final String name;
  @Child IsValueOfTypeNode checkType;
  @CompilerDirectives.CompilationFinal(dimensions = 1)
  private final Type[] expectedTypes;
  @CompilerDirectives.CompilationFinal
  private String expectedTypeMessage;

  ReadArgumentCheckNode(String name, Type[] expectedTypes) {
    this.name = name;
    this.checkType = IsValueOfTypeNode.build();
    this.expectedTypes = expectedTypes;
  }


  static ReadArgumentCheckNode build(String argumentName, Type[] expectedTypes) {
    if (expectedTypes == null || expectedTypes.length == 0) {
      return null;
    } else {
      return ReadArgumentCheckNodeGen.create(argumentName, expectedTypes);
    }
  }

  abstract Object executeCheckOrConversion(VirtualFrame frame, Object value);

  private static boolean isAllFitValue(Object v) {
    return v instanceof DataflowError ||
      (v instanceof Function fn && fn.isThunk());
  }

  @ExplodeLoop
  private boolean findAmongTypes(Object v) {
    for (Type t : expectedTypes) {
      if (checkType.execute(t, v)) {
        return true;
      }
    }
    if (isAllFitValue(v)) {
      return true;
    }
    return false;
  }

  @Specialization(rewriteOn=IllegalArgumentException.class)
  Object executeCheckNoConversion(VirtualFrame frame, Object v) {
    if (findAmongTypes(v)) {
      return v;
    }
    CompilerDirectives.transferToInterpreter();
    throw new IllegalArgumentException();
  }

  @ExplodeLoop
  Pair<Function,Type> findConversion(Type from) {
    var ctx = EnsoContext.get(this);

    if (getRootNode() instanceof EnsoRootNode root) {
      var convert = UnresolvedConversion.build(root.getModuleScope());
      for (Type into : expectedTypes) {
        var conv = convert.resolveFor(ctx, into, from);
        if (conv != null) {
          return Pair.create(conv, into);
        }
      }
    }
    return null;
  }

  ApplicationNode findConversionNode(Type from) {
    var convAndType = findConversion(from);

    if (convAndType != null && getParent() instanceof ReadArgumentNode ran) {
      CompilerAsserts.neverPartOfCompilation();
      var convNode = LiteralNode.build(convAndType.getLeft());
      var intoNode = LiteralNode.build(convAndType.getRight());
      var valueNode = ran.plainRead();
      var args = new CallArgument[]{
        new CallArgument(null, intoNode),
        new CallArgument(null, valueNode)
      };
      return ApplicationNode.build(convNode, args, DefaultsExecutionMode.EXECUTE);
    }
    return null;
  }

  Type findType(TypeOfNode typeOfNode, Object v) {
    if (typeOfNode.execute(v) instanceof Type from) {
      return from;
    }
    return null;
  }

  @Specialization(
    limit="10",
    guards={
      "cachedType != null",
      "findType(typeOfNode, v) == cachedType",
      "convert != null"
    }
  )
  Object executeWithConversion(
    VirtualFrame frame, Object v,
    @Cached TypeOfNode typeOfNode,
    @Cached("findType(typeOfNode, v)") Type cachedType,
    @Cached("findConversionNode(cachedType)") ApplicationNode convert
  ) {
    var converted = convert.executeGeneric(frame);
    return converted;
  }

  boolean noConversionNode(TypeOfNode typeOfNode, Object value) {
    var type = findType(typeOfNode, value);
    return type == null || findConversion(type) == null;
  }

  @Specialization(
    guards={
      "noConversionNode(typeOfNode, v)"
    }
  )
  Object fallbackOrPanicAtTheEnd(
    Object v,
    @Cached TypeOfNode typeOfNode
  ) {
    if (findAmongTypes(v)) {
      return v;
    } else {
      var ctx = EnsoContext.get(this);
      var err = ctx.getBuiltins().error().makeTypeError(expectedTypeMessage(), v, name);
      throw new PanicException(err, this);
    }
  }

  private String expectedTypeMessage() {
    if (expectedTypeMessage != null) {
        return expectedTypeMessage;
    }
    CompilerDirectives.transferToInterpreterAndInvalidate();
    expectedTypeMessage = expectedTypes.length == 1 ?
            expectedTypes[0].toString() :
            Arrays.stream(expectedTypes).map(Type::toString).collect(Collectors.joining(" | "));
    return expectedTypeMessage;
  }
}
