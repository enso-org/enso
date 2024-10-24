package org.enso.compiler.core.ir;

import java.util.List;
import java.util.stream.Collectors;
import org.enso.compiler.core.ir.module.scope.JDefinition;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

public interface JName extends JExpression {
  String name();

  boolean isMethod();

  @IRNode
  interface JBlank extends JName {}

  @IRNode
  interface JLiteral extends JName {
    @IRChild(required = false)
    JName originalName();
  }

  @IRNode
  interface JQualified extends JName {
    @IRChild
    List<JName> parts();

    @Override
    default String name() {
      return parts().stream().map(JName::name).collect(Collectors.joining("."));
    }
  }

  @IRNode
  interface JSelf extends JName {
    boolean synthetic();
  }

  interface JAnnotation extends JName, JDefinition {}

  @IRNode
  interface JGenericAnnotation extends JAnnotation {
    @IRChild
    JExpression expression();
  }
}
