package org.enso.runtime.parser.processor.test.gen.ir.core;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Name;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.collection.immutable.List;

public interface JExpression extends IR {
  @Override
  JExpression mapExpressions(Function<Expression, Expression> fn);

  @Override
  JExpression duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {JExpression.class})
  final class JBlock extends JBlockGen {
    @GenerateFields
    public JBlock(
        @IRChild List<JExpression> expressions,
        @IRChild JExpression returnValue,
        @IRField boolean suspended) {
      super(expressions, returnValue, suspended);
    }
  }

  @GenerateIR(interfaces = {JExpression.class})
  final class JBinding extends JBindingGen {
    @GenerateFields
    public JBinding(@IRChild Name name, @IRChild JExpression expression) {
      super(name, expression);
    }
  }
}
