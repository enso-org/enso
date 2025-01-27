package org.enso.compiler.test.ircompare;

import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.Identifier;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import scala.Option;
import scala.collection.immutable.List;

/**
 * An IR node, whose subtree is skipped by {@link IRComparator}.
 */
public final class SkipIR implements Expression {
  public static final SkipIR INSTANCE = new SkipIR();

  private SkipIR() {}

  @Override
  public MetadataStorage passData() {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public Expression setLocation(Option<IdentifiedLocation> location) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public Expression mapExpressions(Function<Expression, Expression> fn) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public List<IR> children() {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public @Identifier UUID getId() {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public DiagnosticStorage diagnostics() {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public DiagnosticStorage getDiagnostics() {
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

  @Override
  public String showCode(int indent) {
    throw new UnsupportedOperationException("unimplemented");
  }
}
