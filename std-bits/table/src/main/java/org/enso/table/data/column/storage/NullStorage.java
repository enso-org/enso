package org.enso.table.data.column.storage;

import java.util.Iterator;
import java.util.stream.LongStream;
import org.enso.table.data.column.storage.type.NullType;

/** A specialized storage that can be used by columns that contain only null values. */
public class NullStorage extends Storage<Void> {
  private final long size;

  public NullStorage(long size) {
    super(NullType.INSTANCE);
    this.size = size;
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public Void getItemBoxed(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return null;
  }

  @Override
  public Iterator<Void> iterator() {
    return LongStream.range(0, size).mapToObj(i -> (Void) null).iterator();
  }
}
