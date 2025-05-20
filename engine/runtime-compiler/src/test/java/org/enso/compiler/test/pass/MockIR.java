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
import scala.jdk.javaapi.CollectionConverters;

public class MockIR implements IR {
  private final Set<MockMiniPass> preparedBy = new HashSet<>();
  final List<MockIR> children = new ArrayList<>();
  private final MockIR parent;

  /**
   * @param parent null if this is the root element
   */
  public MockIR(MockIR parent) {
    this.parent = parent;
    if (parent != null) {
      assert !parent.children.contains(this);
      parent.children.add(this);
    }
  }

  MockIR getParent() {
    return parent;
  }

  boolean hasParent() {
    return parent != null;
  }

  boolean isPreparedBy(MockMiniPass pass) {
    return preparedBy.contains(pass);
  }

  boolean isPreparedByAny() {
    return !preparedBy.isEmpty();
  }

  void setPreparedBy(MockMiniPass pass) {
    preparedBy.add(pass);
  }

  @Override
  public IR mapExpressions(Function<Expression, Expression> fn) {
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
  public MetadataStorage passData() {
    return null;
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    return null;
  }

  @Override
  public IR setLocation(Option<IdentifiedLocation> location) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Override
  public scala.collection.immutable.List<IR> children() {
    var lst = CollectionConverters.asScala(children).toList();
    var ret = lst.map(item -> (IR) item);
    return ret;
  }

  @Override
  public @Identifier UUID getId() {
    return null;
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
  public IR duplicate(
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
