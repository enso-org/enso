package org.enso.table.data.index;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.Objects;

import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;

/**
 * A multi-value key for unordered operations like group-by or distinct.
 *
 * <p>It relies on folding logic that coerces values to their representatives in such a way that
 * their equality is consistent with how Enso would handle equality.
 *
 * <p>As it relies on hashing, it currently is not prepared to work correctly for custom
 * Enso-defined objects, as hashing of such objects is not yet implemented properly.
 */
public class UnorderedMultiValueKey extends MultiValueKeyBase {
  private final int hashCodeValue;
  private final TextFoldingStrategy textFoldingStrategy;

  public UnorderedMultiValueKey(Storage[] storages, int rowIndex) {
    this(storages, rowIndex, TextFoldingStrategy.unicodeNormalizedFold);
  }

  public UnorderedMultiValueKey(
      Storage[] storages, int rowIndex, TextFoldingStrategy textFoldingStrategy) {
    super(storages, rowIndex);
    this.textFoldingStrategy = textFoldingStrategy;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 1;
    for (int i = 0; i < storages.length; i++) {
      h = 31 * h;

      Object value = this.get(i);
      if (value != null) {
        hasFloatValues = hasFloatValues || isFloatingPoint(value);
        Object folded = foldObject(value);
        h += folded.hashCode();
      }
    }

    this.hashCodeValue = h;
    floatsComputed = true;
  }

  /**
   * Folds the value to ensure consistency with Enso's equality.
   *
   * <p>Case-sensitivity of text folding is controlled by {@code textFoldingStrategy}.
   */
  protected Object foldObject(Object value) {
    if (value == null) {
      return null;
    }

    if (value instanceof String s) {
      return textFoldingStrategy.fold(s);
    }

    Object numeric = foldNumeric(value);
    if (numeric != null) {
      return numeric;
    }

    if (value instanceof Boolean) {
      return value;
    }

    if (value instanceof LocalDate || value instanceof LocalTime || value instanceof ZonedDateTime) {
      return value;
    }

    throw new IllegalArgumentException("Custom objects in UnorderedMultiValueKey are currently not supported due to lack of hashing support.");
  }

  /**
   * If the value is a numeric type, this method coerces it in such a way to ensure consistency with
   * Enso.
   *
   * <p>Integer types are coerced to {@code Long} and floating point values are coerced to {@code
   * Double} unless they represent a whole integer in which case they are also coerced to {@code
   * Long}, to ensure the Enso property that {@code 2 == 2.0}.
   *
   * Returns {@code null} if the value was not a numeric value.
   */
  protected Object foldNumeric(Object value) {
    if (value instanceof Long) {
      return value;
    } else if (value instanceof Integer i) {
      return i.longValue();
    } else if (value instanceof Byte b) {
      return b.longValue();
    } else if (value instanceof Float f && f % 1 == 0) {
      return f.longValue();
    } else if (value instanceof Double d && d % 1 == 0) {
      return d.longValue();
    } else if (value instanceof Float f) {
      return f.doubleValue();
    } else if (value instanceof Double d) {
      return d;
    }

    return null;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof UnorderedMultiValueKey that)) return false;
    if (storages.length != that.storages.length) return false;
    if (hashCodeValue != that.hashCodeValue) return false;
    for (int i = 0; i < storages.length; i++) {
      Object thisFolded = foldObject(this.get(i));
      Object thatFolded = foldObject(that.get(i));
      if (!Objects.equals(thisFolded, thatFolded)) {
        return false;
      }
    }

    return true;
  }

  @Override
  public int hashCode() {
    return this.hashCodeValue;
  }
}
