package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import java.util.Arrays;
import java.util.stream.Collectors;

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
      var result = n.findDirectMatch(frame, value);
      if (result != null) {
        return result;
      }
    }
    return null;
  }

  @Override
  @ExplodeLoop
  Object executeConversion(VirtualFrame frame, Object value) {
    for (var n : checks) {
      var result = n.executeConversion(frame, value);
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
