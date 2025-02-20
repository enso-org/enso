package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.interpreter.node.ExpressionNode;

final class OneOfTypesCheckNode extends AbstractTypeCheckNode {

  @Children private AbstractTypeCheckNode[] checks;

  OneOfTypesCheckNode(String name, AbstractTypeCheckNode[] checks) {
    super(name);
    this.checks = checks;
  }

  @Override
  @ExplodeLoop
  final Object findDirectMatch(VirtualFrame frame, Object value) {
    for (var n : checks) {
      java.lang.Object result = n.findDirectMatch(frame, value);
      if (result != null) {
        return result;
      }
    }
    return null;
  }

  @Override
  @ExplodeLoop
  Object executeCheckOrConversion(VirtualFrame frame, Object value, ExpressionNode expr) {
    java.lang.Object direct = findDirectMatch(frame, value);
    if (direct != null) {
      return direct;
    }
    for (var n : checks) {
      java.lang.Object result = n.executeCheckOrConversion(frame, value, expr);
      if (result != null) {
        return result;
      }
    }
    return null;
  }

  @Override
  String expectedTypeMessage() {
    java.util.List<java.lang.String> parts =
        Arrays.stream(checks)
            .map(AbstractTypeCheckNode::expectedTypeMessage)
            .collect(Collectors.toList());
    return joinTypeParts(parts, "|");
  }
}
