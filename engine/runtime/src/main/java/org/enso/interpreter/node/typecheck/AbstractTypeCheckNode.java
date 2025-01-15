package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import java.util.List;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.expression.builtin.meta.AtomWithAHoleNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;

/**
 * Root of hierarchy of nodes checking types. This class (and its subclasses) are an implementation
 * detail. The API to perform the check or conversion is in {@link TypeCheckValueNode}.
 */
abstract sealed class AbstractTypeCheckNode extends Node
    permits OneOfTypesCheckNode, AllOfTypesCheckNode, SingleTypeCheckNode, MetaTypeCheckNode {
  private final String comment;
  @CompilerDirectives.CompilationFinal private String expectedTypeMessage;

  AbstractTypeCheckNode(String comment) {
    this.comment = comment;
  }

  abstract Object findDirectMatch(VirtualFrame frame, Object value);

  abstract Object executeCheckOrConversion(
      VirtualFrame frame, Object value, ExpressionNode valueNode);

  abstract String expectedTypeMessage();

  @ExplodeLoop
  final boolean isAllTypes() {
    Node p = this;
    CompilerAsserts.partialEvaluationConstant(p);
    for (; ; ) {
      if (p instanceof TypeCheckValueNode vn) {
        CompilerAsserts.partialEvaluationConstant(vn);
        var allTypes = vn.isAllTypes();
        CompilerAsserts.partialEvaluationConstant(allTypes);
        return allTypes;
      }
      p = p.getParent();
    }
  }

  /**
   * The error message for this node's check. Ready for being used at "fast path".
   *
   * @return
   */
  final String getExpectedTypeMessage() {
    if (expectedTypeMessage == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      expectedTypeMessage = expectedTypeMessage();
    }
    return expectedTypeMessage;
  }

  /**
   * Composes the comment message describing the kind of check this node performs. Ready for being
   * used at "fast path".
   *
   * @return description of this node's "expectations"
   */
  final Text getComment() {
    var where = Text.create(comment == null ? "expression" : comment);
    var exp = Text.create("expected ");
    var got = Text.create(" to be {exp}, but got {got}");
    return Text.create(exp, Text.create(where, got));
  }

  final String joinTypeParts(List<String> parts, String separator) {
    assert !parts.isEmpty();
    if (parts.size() == 1) {
      return parts.get(0);
    }

    var separatorWithSpace = " " + separator + " ";
    var builder = new StringBuilder();
    boolean isFirst = true;
    for (String part : parts) {
      if (isFirst) {
        isFirst = false;
      } else {
        builder.append(separatorWithSpace);
      }

      // If the part contains a space, it means it is not a single type but already a more complex
      // expression with a separator.
      // So to ensure we don't mess up the expression layers, we need to add parentheses around it.
      boolean needsParentheses = part.contains(" ");
      if (needsParentheses) {
        builder.append("(");
      }
      builder.append(part);
      if (needsParentheses) {
        builder.append(")");
      }
    }

    return builder.toString();
  }

  static boolean isAllFitValue(Object v) {
    return v instanceof DataflowError || AtomWithAHoleNode.isHole(v);
  }
}
