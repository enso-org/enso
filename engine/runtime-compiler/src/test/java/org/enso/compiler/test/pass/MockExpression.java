package org.enso.compiler.test.pass;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import scala.Option;
import scala.PartialFunction;

public final class MockExpression extends MockIR implements Expression {
  private final Set<MockMiniPass> transformedBy = new HashSet<>();

  public MockExpression(MockIR parent) {
    super(parent);
  }

  boolean isTransformedBy(MockMiniPass pass) {
    return transformedBy.contains(pass);
  }

  boolean isTransformedByAny() {
    return !transformedBy.isEmpty();
  }

  void setTransformedByPass(MockMiniPass pass) {
    transformedBy.add(pass);
  }

  @Override
  public Expression mapExpressions(Function<Expression, Expression> fn) {
    for (var child : children) {
      if (child instanceof MockExpression expr) {
        fn.apply(expr);
      } else {
        child.mapExpressions(fn);
      }
    }
    return this;
  }

  @Override
  public Expression transformExpressions(PartialFunction<Expression, Expression> fn) {
    return this;
  }

  @Override
  public Expression setLocation(Option<IdentifiedLocation> location) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public Expression duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers) {
    throw new UnsupportedOperationException("unimplemented");
  }
}
