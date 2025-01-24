package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

final class AllOfTypesCheckNode extends AbstractTypeCheckNode {

  @Children private AbstractTypeCheckNode[] checks;
  @Child private TypesLibrary types;
  @Child private EnsoMultiValue.NewNode newNode;

  AllOfTypesCheckNode(String name, AbstractTypeCheckNode[] checks) {
    super(name);
    this.checks = checks;
    this.types = TypesLibrary.getFactory().createDispatched(checks.length);
    this.newNode = EnsoMultiValue.NewNode.create();
  }

  AbstractTypeCheckNode[] getChecks() {
    return checks;
  }

  @Override
  Object findDirectMatch(VirtualFrame frame, Object value) {
    return null;
  }

  @Override
  @ExplodeLoop
  Object executeCheckOrConversion(VirtualFrame frame, Object value, ExpressionNode expr) {
    var values = new Object[checks.length];
    var valueTypes = new Type[checks.length];
    var at = 0;
    var integers = 0;
    var floats = 0;
    for (var n : checks) {
      var result = n.executeCheckOrConversion(frame, value, expr);
      if (result == null) {
        return null;
      }
      var t = types.getType(result);
      var ctx = EnsoContext.get(this);
      if (ctx.getBuiltins().number().getInteger() == t) {
        if (++integers > 1) {
          continue;
        }
      }
      if (ctx.getBuiltins().number().getFloat() == t) {
        if (++floats > 1) {
          continue;
        }
      }
      valueTypes[at] = t;
      if (result instanceof EnsoMultiValue emv) {
        result =
            EnsoMultiValue.CastToNode.getUncached()
                .findTypeOrNull(valueTypes[at], emv, false, true);
      }
      if (result == null) {
        return null;
      }
      values[at] = result;
      at++;
    }
    if (at != checks.length) {
      // request for Number & Integer may yield two integers collision
      // request for Number & Float may yield two floats collision
      // request for Number & Integer & Float must yield one collision
      //
      // people shouldn't be doing such things but the code must be
      // ready for that - switching to interpreter without optimization
      CompilerDirectives.transferToInterpreter();
      values = Arrays.copyOf(values, at);
      valueTypes = Arrays.copyOf(valueTypes, at);
    }
    return newNode.newValue(valueTypes, valueTypes.length, 0, values);
  }

  @Override
  String expectedTypeMessage() {
    var parts =
        Arrays.stream(checks)
            .map(AbstractTypeCheckNode::expectedTypeMessage)
            .collect(Collectors.toList());
    return joinTypeParts(parts, "&");
  }
}
