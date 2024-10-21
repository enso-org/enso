package org.enso.compiler.core.ir;

import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

public interface JExpression extends IR {
  @IRNode
  interface JBlock extends JExpression {
    @IRChild
    List<JExpression> expressions();

    @IRChild
    JExpression returnValue();

    boolean suspended();
  }

  /**
   * A binding expression of the form `name = expr`
   *
   * <p>To create a binding that binds no available name, set the name of the binding to an
   * [[Name.Blank]] (e.g. _ = foo a b).
   */
  @IRNode
  interface JBinding extends JExpression {
    @IRChild
    JName name();

    @IRChild
    JExpression expression();
  }
}
