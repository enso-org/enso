package org.enso.compiler.test.pass;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.Identifier;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import scala.Option;
import scala.PartialFunction;
import scala.jdk.javaapi.CollectionConverters;

final class MockExpression implements Expression {
  private final Set<MockMiniPass> transformedBy = new HashSet<>();
  private final Set<MockMiniPass> preparedBy = new HashSet<>();
  private final boolean hasParent;
  private List<MockExpression> exprChildren = new ArrayList<>();

  MockExpression(boolean hasParent) {
    this.hasParent = hasParent;
  }

  boolean isTransformedBy(MockMiniPass pass) {
    return transformedBy.contains(pass);
  }

  boolean isTransformedByAny() {
    return !transformedBy.isEmpty();
  }

  boolean isPreparedBy(MockMiniPass pass) {
    return preparedBy.contains(pass);
  }

  boolean isPreparedByAny() {
    return !preparedBy.isEmpty();
  }

  void setTransformedByPass(MockMiniPass pass) {
    transformedBy.add(pass);
  }

  boolean hasParent() {
    return hasParent;
  }

  void addChild(MockExpression child) {
    exprChildren.add(child);
  }

  void setPreparedBy(MockMiniPass pass) {
    preparedBy.add(pass);
  }

  @Override
  public Expression transformExpressions(PartialFunction<Expression, Expression> fn) {
    return this;
  }

  @Override
  public Expression mapExpressions(Function<Expression, Expression> fn) {
    for (var child : exprChildren) {
      fn.apply(child);
    }
    return this;
  }

  @Override
  public scala.collection.immutable.List<IR> children() {
    var lst = CollectionConverters.asScala(exprChildren).toList();
    var ret = lst.map(item -> (IR) item);
    return ret;
  }

  @Override
  public @Identifier UUID getId() {
    return null;
  }

  @Override
  public DiagnosticStorage diagnostics() {
    return null;
  }

  @Override
  public DiagnosticStorage getDiagnostics() {
    return null;
  }

  @Override
  public MetadataStorage passData() {
    return null;
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    return null;
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

  @Override
  public String showCode(int indent) {
    throw new UnsupportedOperationException("unimplemented");
  }
}
