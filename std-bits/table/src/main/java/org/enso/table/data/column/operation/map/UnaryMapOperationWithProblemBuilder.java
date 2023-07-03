package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

/**
 * A map-like operation that ignores its second argument
 *
 * @param <I> the supported storage type
 */
public abstract class UnaryMapOperationWithProblemBuilder<T, I extends Storage<T>>
    extends MapOperation<T, I> {
  public UnaryMapOperationWithProblemBuilder(String name) {
    super(name);
  }

  protected abstract Storage<?> run(
      I storage, Object arg, MapOperationProblemBuilder problemBuilder);

  @Override
  public Storage<?> runMap(I storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    return run(storage, arg, problemBuilder);
  }

  @Override
  public Storage<?> runZip(I storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    return run(storage, arg, problemBuilder);
  }
}
