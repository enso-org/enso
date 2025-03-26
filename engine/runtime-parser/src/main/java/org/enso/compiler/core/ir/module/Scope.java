package org.enso.compiler.core.ir.module;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import scala.Option;

/** A representation of constructs that can only occur in the top-level module scope */
public interface Scope extends IR {

  @Override
  Scope mapExpressions(Function<Expression, Expression> fn);

  @Override
  Scope setLocation(Option<IdentifiedLocation> location);

  @Override
  Scope duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);
}
