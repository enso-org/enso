package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.storage.Storage;

public abstract class GenericUnaryIntegerOp<U, T extends U, I extends Storage<T>>
    extends UnaryIntegerOp<T, I> {
  public GenericUnaryIntegerOp(String name) {
    super(name);
  }

  protected abstract long doGenericOperation(U value);

  @Override
  protected long doOperation(T value) {
    return doGenericOperation(value);
  }
}
