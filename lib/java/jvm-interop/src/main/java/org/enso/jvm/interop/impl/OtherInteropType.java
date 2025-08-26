package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;

final class OtherInteropType {
  private static final int IS_BOOLEAN = 0x01;
  private static final int IS_STRING = 0x02;
  private static final int IS_META_OBJECT = 0x03;
  private static final int IS_EXCEPTION = 0x04;
  private static final int IS_ITERATOR = 0x05;
  private static final int IS_DURATION = 0x08;
  private static final int IS_NULL = 0x10;

  private static final int MASK_TEMPORAL = 0x0100;
  private static final int IS_DATE = MASK_TEMPORAL + 0x01;
  private static final int IS_TIME = MASK_TEMPORAL + 0x02;
  private static final int IS_ZONE = MASK_TEMPORAL + 0x04;

  private static final int MASK_NUMBER = 0x0200;
  private static final int FITS_BYTE = MASK_NUMBER + 0x01;
  private static final int FITS_SHORT = MASK_NUMBER + 0x02;
  private static final int FITS_INT = MASK_NUMBER + 0x04;
  private static final int FITS_LONG = MASK_NUMBER + 0x08;
  private static final int FITS_FLOAT = MASK_NUMBER + 0x10;
  private static final int FITS_DOUBLE = MASK_NUMBER + 0x20;
  private static final int FITS_BIG_INTEGER = MASK_NUMBER + 0x40;

  private static final int MASK_ARRAY = 0x0400;
  private static final int MASK_HASH = 0x0800;

  private OtherInteropType() {}

  static short findType(TruffleObject obj) {
    var iop = InteropLibrary.getUncached();
    var one = oneOf(obj, iop);
    if (iop.hasArrayElements(obj)) {
      one |= MASK_ARRAY;
    }
    if (iop.hasHashEntries(obj)) {
      one |= MASK_HASH;
    }
    return one;
  }

  private static short oneOf(TruffleObject obj, InteropLibrary iop) {
    if (iop.isNull(obj)) {
      return IS_NULL;
    }
    if (iop.isBoolean(obj)) {
      return IS_BOOLEAN;
    }
    if (iop.isString(obj)) {
      return IS_STRING;
    }
    if (iop.isNumber(obj)) {
      var m = MASK_NUMBER;
      if (iop.fitsInByte(obj)) {
        m |= FITS_BYTE;
      }
      if (iop.fitsInShort(obj)) {
        m |= FITS_SHORT;
      }
      if (iop.fitsInInt(obj)) {
        m |= FITS_INT;
      }
      if (iop.fitsInLong(obj)) {
        m |= FITS_LONG;
      }
      if (iop.fitsInFloat(obj)) {
        m |= FITS_FLOAT;
      }
      if (iop.fitsInDouble(obj)) {
        m |= FITS_DOUBLE;
      }
      if (iop.fitsInBigInteger(obj)) {
        m |= FITS_BIG_INTEGER;
      }
      return (short) m;
    }
    if (iop.isDuration(obj)) {
      return IS_DURATION;
    }
    if (iop.isException(obj)) {
      return IS_EXCEPTION;
    }
    if (iop.isMetaObject(obj)) {
      return IS_META_OBJECT;
    }
    if (iop.isIterator(iop)) {
      return IS_ITERATOR;
    }
    var m = 0x00;
    if (iop.isDate(obj)) {
      m |= IS_DATE;
    }
    if (iop.isTime(obj)) {
      m |= IS_TIME;
    }
    if (iop.isTimeZone(obj)) {
      m |= IS_ZONE;
    }
    return (short) m;
  }

  static boolean isNull(int v) {
    return v == IS_NULL;
  }

  static boolean isMetaObject(int v) {
    return v == IS_META_OBJECT;
  }

  static boolean isDate(int v) {
    return (v & IS_DATE) == IS_DATE;
  }

  static boolean isTime(int v) {
    return (v & IS_TIME) == IS_TIME;
  }

  static boolean isZone(int v) {
    return (v & IS_ZONE) == IS_ZONE;
  }

  static boolean isDuration(int v) {
    return v == IS_DURATION;
  }

  static boolean isNumber(int v) {
    return (v & MASK_NUMBER) == MASK_NUMBER;
  }

  static boolean fitsByte(int v) {
    return (v & FITS_BYTE) == FITS_BYTE;
  }

  static boolean fitsShort(int v) {
    return (v & FITS_SHORT) == FITS_SHORT;
  }

  static boolean fitsInt(int v) {
    return (v & FITS_INT) == FITS_INT;
  }

  static boolean fitsLong(int v) {
    return (v & FITS_LONG) == FITS_LONG;
  }

  static boolean fitsFloat(int v) {
    return (v & FITS_FLOAT) == FITS_FLOAT;
  }

  static boolean fitsDouble(int v) {
    return (v & FITS_DOUBLE) == FITS_DOUBLE;
  }

  static boolean fitsBigInteger(int v) {
    return (v & FITS_BIG_INTEGER) == FITS_BIG_INTEGER;
  }

  static boolean hasArrayElements(int v) {
    return (v & MASK_ARRAY) == MASK_ARRAY;
  }

  static boolean hasHashEntries(int v) {
    return (v & MASK_HASH) == MASK_HASH;
  }
}
