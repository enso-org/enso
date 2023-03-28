package org.enso.table.data.column.storage.type;

/**
 * Represents an underlying internal storage type that can be mapped to the Value Type that is exposed to users.
 */
public sealed interface StorageType permits AnyObject, Boolean, Date, DateTime, Float, Integer, Text, TimeOfDay {
}
