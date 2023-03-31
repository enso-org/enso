package org.enso.table.data.column.storage.type;

/**
 * Represents an underlying internal storage type that can be mapped to the Value Type that is exposed to users.
 */
public sealed interface StorageType permits AnyObjectType, BooleanType, DateType, DateTimeType, FloatType, IntegerType, TextType, TimeOfDayType {
}
