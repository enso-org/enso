package org.enso.compiler.core.ir;

import org.enso.compiler.core.IR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

public interface JDefinitionArgument extends IR {
  @IRChild
  JName name();

  @IRChild(required = false)
  JExpression ascribedType();

  @IRChild(required = false)
  JExpression defaultValue();

  boolean suspended();

  @IRNode
  interface JSpecified extends JDefinitionArgument {}
}
