package org.enso.interpreter.node.callable;

import static org.junit.Assert.fail;

import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.expression.literal.LiteralNode;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

public class SequenceLiteralNodeTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  public SequenceLiteralNodeTest() {}

  @Test
  public void propagatePanicSentinelImpl() {
    var sentinel = new PanicSentinel(new PanicException(0L, null), null);

    var one = LiteralNode.build(1);
    var two = LiteralNode.build(2);
    var three = LiteralNode.build(sentinel);
    var four = LiteralNode.build(4);
    var seq = SequenceLiteralNode.build(new ExpressionNode[] {one, two, three, four});

    try {
      var res = seq.executeGeneric(null);
      fail("Unexpected result. PanicSentinel should have been thrown: " + res);
    } catch (PanicSentinel ex) {
      if (sentinel != ex) {
        fail("The right exception should have been propagated!");
      }
    }
  }
}
