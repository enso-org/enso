package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.runtime.data.text.Text;

@NodeInfo(description = "Converts Enso Text to a Java String.")
public final class ToJavaStringNode extends Node {
  private static final ToJavaStringNode UNCACHED = new ToJavaStringNode();

  private ToJavaStringNode() {}

  /**
   * Returns the uncached version of this node.
   *
   * @return the uncached version of this node.
   */
  public static ToJavaStringNode getUncached() {
    return UNCACHED;
  }

  /**
   * Creates a new instance of this node.
   *
   * @return a new instance of this node.
   */
  @NeverDefault
  public static ToJavaStringNode build() {
    return new ToJavaStringNode();
  }

  /**
   * Performs the conversion of Enso Text to a Java String.
   *
   * @param text the text to convert.
   * @return the result of conversion.
   */
  public String execute(Text text) {
    return text.toString();
  }
}
