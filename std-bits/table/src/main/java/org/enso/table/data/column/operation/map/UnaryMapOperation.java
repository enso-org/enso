package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

/**
 * A unary map-like operation.
 *
 * @param <I> the supported storage type
 */
public abstract class UnaryMapOperation<T, I extends Storage<? super T>> {
  private final String name;

  public UnaryMapOperation(String name) {
    this.name = name;
  }

  protected abstract Storage<?> run(I storage, MapOperationProblemBuilder problemBuilder);

  /** @return the name of this operation */
  public String getName() {
    return name;
  }
}
