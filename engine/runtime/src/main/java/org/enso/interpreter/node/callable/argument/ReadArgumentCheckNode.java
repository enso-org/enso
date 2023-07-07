package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.CompilerDirectives;
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

public abstract class ReadArgumentCheckNode extends Node {

  static ReadArgumentCheckNode build(String argumentName, Type[] expectedTypes) {
    if (expectedTypes == null || expectedTypes.length == 0) {
      return null;
    } else {
      return new Types(argumentName, expectedTypes);
    }
  }

  abstract Object executeCheckOrConversion(VirtualFrame frame, Object value);

  private static boolean isAllFitValue(Object v) {
    return v instanceof DataflowError ||
      (v instanceof Function fn && fn.isThunk());
  }

  static final class Types extends ReadArgumentCheckNode {
    private final String name;
    @Child IsValueOfTypeNode checkType;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final Type[] expectedTypes;

    Types(String name, Type[] expectedTypes) {
      this.name = name;
      this.checkType = IsValueOfTypeNode.build();
      this.expectedTypes = expectedTypes;
    }

    @Override
    @ExplodeLoop
    Object executeCheckOrConversion(VirtualFrame frame, Object v) {
      for (Type t : expectedTypes) {
        if (checkType.execute(t, v)) {
          return v;
        }
      }
      if (isAllFitValue(v)) {
        return v;
      }
      CompilerDirectives.transferToInterpreter();
      var ctx = EnsoContext.get(this);

      if (getRootNode() instanceof EnsoRootNode root) {
        var convert = UnresolvedConversion.build(root.getModuleScope());
        if (TypeOfNode.getUncached().execute(v) instanceof Type from) {
          for (Type into : expectedTypes) {
            var conv = convert.resolveFor(ctx, into, from);
            if (conv != null) {
              // XXX: absolutely ineffective:
              var convNode = LiteralNode.build(conv);
              var intoNode = LiteralNode.build(into);
              var valueNode = LiteralNode.build(v);
              var args = new CallArgument[]{
                new CallArgument(null, intoNode),
                new CallArgument(null, valueNode),};
              var app = ApplicationNode.build(convNode, args, DefaultsExecutionMode.EXECUTE);
              var r = app.executeGeneric(frame);
              return r;
            }
          }
        }
      }

      var expecting
              = expectedTypes.length == 1 ? expectedTypes[0] : Arrays.stream(expectedTypes).map(Type::toString).collect(Collectors.joining(" | "));
      var err = ctx.getBuiltins().error().makeTypeError(expecting, v, name);
      throw new PanicException(err, this);
    }
  }
}
