package org.enso.table.data.column.operation.map.datetime;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.operation.map.bool.GenericBinaryOpReturningBoolean;
import org.enso.table.data.column.storage.SpecializedStorage;

public abstract class TimeLikeBinaryOpReturningBoolean<T>
    extends GenericBinaryOpReturningBoolean<T, SpecializedStorage<T>> {
  Class<T> clazz;

  public TimeLikeBinaryOpReturningBoolean(String name, Class<T> objectType) {
    super(name);
    this.clazz = objectType;
  }

  @Override
  protected T tryCast(Object object) {
    // We need to adapt date/time values to ensure correct handling of polyglot values.
    Object adapted = Polyglot_Utils.convertPolyglotValue(object);
    if (clazz.isInstance(adapted)) {
      return clazz.cast(adapted);
    } else {
      return null;
    }
  }
}
