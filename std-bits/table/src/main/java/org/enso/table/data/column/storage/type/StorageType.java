package org.enso.table.data.column.storage.type;

public sealed interface StorageType {
  record Boolean() implements StorageType {}
  record Integer(Bits bits) implements StorageType {}
  record Float(Bits bits) implements StorageType {
    public Float {
      if (bits != Bits.BITS_64) {
        throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
      }
    }
  }

  record FixedLengthString(int length) implements StorageType {}
  record VariableLengthString() implements StorageType {}
  record Date() implements StorageType {}
  record TimeOfDay() implements StorageType {}
  record DateTime() implements StorageType {}
  record AnyObject() implements StorageType {}

  Boolean BOOLEAN = new Boolean();
  Integer INTEGER_64 = new Integer(Bits.BITS_8);
  Float FLOAT_64 = new Float(Bits.BITS_64);
  VariableLengthString VARIABLE_LENGTH_STRING = new VariableLengthString();
  Date DATE = new Date();
  TimeOfDay TIME_OF_DAY = new TimeOfDay();
  DateTime DATE_TIME = new DateTime();
  AnyObject ANY_OBJECT = new AnyObject();

  enum Bits {
    BITS_8,
    BITS_16,
    BITS_32,
    BITS_64;

    public int toBits() {
      return switch (this) {
        case BITS_8 -> 8;
        case BITS_16 -> 16;
        case BITS_32 -> 32;
        case BITS_64 -> 64;
      };
    }
  }
}
