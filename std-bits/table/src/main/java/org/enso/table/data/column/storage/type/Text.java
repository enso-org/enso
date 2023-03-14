package org.enso.table.data.column.storage.type;

public record Text(long maxLength, boolean fixedLength) implements StorageType {
}
