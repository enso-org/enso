package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.function.Supplier;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.util.CachingSupplier;

@NodeInfo(
    shortName = "lazy",
    description = "Represents an arbitrary compile-time constant computed lazily.")
public final class LazyObjectNode extends ExpressionNode {

  private final String error;
  private final CachingSupplier<? extends Object> supply;

  private LazyObjectNode(String error, Supplier<? extends Object> supply) {
    this.error = error;
    this.supply = CachingSupplier.wrap(supply);
  }

  /**
   * Creates a node that returns lazily computed value.
   *
   * @param errorMessage the error message to show when the value is {@code null}
   * @param supplier computes the value lazily. Can return {@code null} and then the {@code
   *     errorMessage} error is created
   */
  public static ExpressionNode build(String errorMessage, Supplier<TruffleObject> supplier) {
    return new LazyObjectNode(errorMessage, supplier);
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    var result = supply.get();
    if (result instanceof TruffleObject) {
      return result;
    }
    return DataflowError.withDefaultTrace(Text.create(error), this);
  }
}
