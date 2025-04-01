package org.enso.table.data.column.storage;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.CountNonTrivialWhitespace;
import org.enso.table.data.column.operation.CountUntrimmed;
import org.enso.table.data.column.operation.SampleOperation;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.text.CoalescingStringStringOp;
import org.enso.table.data.column.operation.map.text.StringIsInOp;
import org.enso.table.data.column.operation.map.text.StringStringOp;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;

/** A column storing strings. */
public final class StringStorage extends SpecializedStorage<String> {

  record DataQualityMetrics(Long untrimmedCount, Long whitespaceCount) {}

  private CachedPropertyCheck<DataQualityMetrics> dataQualityMetricsValues;

  /**
   * @param data the underlying data
   * @param type the type of the column
   */
  public StringStorage(String[] data, TextType type) {
    super(type, data, buildOps());

    dataQualityMetricsValues =
        new CachedPropertyCheck<>(() -> createDataQualityMetricsWitDefaultSize(), null);
  }

  public static StringStorage makeEmpty(TextType type, long size) {
    int intSize = Builder.checkSize(size);
    return new StringStorage(new String[intSize], type);
  }

  @Override
  public TextType getType() {
    // As the type is fixed, we can safely cast it.
    return (TextType) super.getType();
  }

  @Override
  protected SpecializedStorage<String> newInstance(String[] data) {
    return new StringStorage(data, getType());
  }

  @Override
  protected String[] newUnderlyingArray(int size) {
    return new String[size];
  }

  DataQualityMetrics createDataQualityMetricsWitDefaultSize() {
    return new DataQualityMetrics(
        CountUntrimmed.compute(this, SampleOperation.DEFAULT_SAMPLE_SIZE, null),
        CountNonTrivialWhitespace.compute(this, SampleOperation.DEFAULT_SAMPLE_SIZE, null));
  }

  /**
   * Counts the number of cells in the columns with untrimmed whitespace. If the calculation fails
   * then it returns null.
   *
   * @return the number of cells with untrimmed whitespace
   */
  public Long cachedUntrimmedCount() throws InterruptedException {
    return dataQualityMetricsValues.get().untrimmedCount.longValue();
  }

  /**
   * Counts the number of cells in the columns with non trivial whitespace. If the calculation fails
   * then it returns null.
   *
   * @return the number of cells with non trivial whitespace
   */
  public Long cachedWhitespaceCount() throws InterruptedException {
    return dataQualityMetricsValues.get().whitespaceCount.longValue();
  }

  private static MapOperationStorage<String, SpecializedStorage<String>> buildOps() {
    MapOperationStorage<String, SpecializedStorage<String>> t = new MapOperationStorage<>();
    t.add(new StringIsInOp<>());
    t.add(
        new StringStringOp(Maps.ADD) {
          @Override
          protected String doString(String a, String b) {
            return a + b;
          }

          @Override
          protected TextType computeResultType(TextType a, TextType b) {
            return TextType.concatTypes(a, b);
          }
        });
    t.add(
        new CoalescingStringStringOp(Maps.MIN) {
          @Override
          protected String doString(String a, String b) {
            if (Text_Utils.compare_normalized(a, b) < 0) {
              return a;
            } else {
              return b;
            }
          }

          @Override
          protected TextType computeResultType(TextType a, TextType b) {
            return TextType.maxType(a, b);
          }
        });
    t.add(
        new CoalescingStringStringOp(Maps.MAX) {
          @Override
          protected String doString(String a, String b) {
            if (Text_Utils.compare_normalized(a, b) > 0) {
              return a;
            } else {
              return b;
            }
          }

          @Override
          protected TextType computeResultType(TextType a, TextType b) {
            return TextType.maxType(a, b);
          }
        });
    return t;
  }

  @Override
  public StorageType<?> inferPreciseTypeShrunk() {
    var type = getType();
    if (type.fixedLength()) {
      return type;
    }

    long minLength = Long.MAX_VALUE;
    long maxLength = Long.MIN_VALUE;
    for (long i = 0; i < getSize(); i++) {
      String s = getItemBoxed(i);
      if (s != null) {
        long length = Text_Utils.grapheme_length(s);
        minLength = Math.min(minLength, length);
        maxLength = Math.max(maxLength, length);
      }
    }

    // maxLength will be <0 if all values were null and will be ==0 if all values were empty
    // strings.
    // In both of these cases, we avoid shrinking the type and return the original type instead.
    if (maxLength <= 0) {
      return getType();
    }

    final long SHORT_LENGTH_THRESHOLD = 255;
    if (minLength == maxLength) {
      return TextType.fixedLength(minLength);
    } else if (maxLength <= SHORT_LENGTH_THRESHOLD
        && (type.maxLength() < 0 || SHORT_LENGTH_THRESHOLD < type.maxLength())) {
      // If the string was unbounded or the bound was larger than 255, we shrink it to 255.
      return TextType.variableLengthWithLimit(SHORT_LENGTH_THRESHOLD);
    } else {
      // Otherwise, we return the original type (because it was either smaller than the proposed 255
      // bound, or the
      // existing elements to do not fit into the 255 bound).
      return getType();
    }
  }
}
