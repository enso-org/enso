package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.CountNonTrivialWhitespace;
import org.enso.table.data.column.operation.CountUntrimmed;
import org.enso.table.data.column.operation.DistinctValuesCheck;
import org.enso.table.data.column.operation.SampleOperation;
import org.enso.table.data.column.storage.type.TextType;

/** A column storing strings. */
public final class StringStorage extends SpecializedStorage<String> {
  record DataQualityMetrics(Long untrimmedCount, Long whitespaceCount) {}

  private final CachedPropertyCheck<DataQualityMetrics> dataQualityMetricsValues;
  private final CachedPropertyCheck<Boolean> distinctValuesCheck;

  /**
   * @param data the underlying data
   * @param type the type of the column
   */
  public StringStorage(String[] data, TextType type) {
    super(type, data);

    dataQualityMetricsValues =
        new CachedPropertyCheck<>(this::createDataQualityMetricsWitDefaultSize, null);

    distinctValuesCheck =
        new CachedPropertyCheck<>(() -> DistinctValuesCheck.compute(this, null), null);
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
    return dataQualityMetricsValues.get().untrimmedCount;
  }

  /**
   * Counts the number of cells in the columns with non trivial whitespace. If the calculation fails
   * then it returns null.
   *
   * @return the number of cells with non trivial whitespace
   */
  public Long cachedWhitespaceCount() throws InterruptedException {
    return dataQualityMetricsValues.get().whitespaceCount;
  }

  /**
   * Checks the number of distinct values
   *
   * @return true if there are less than 100 distinct values
   */
  public Boolean cachedDistinctValueCheck() throws InterruptedException {
    return distinctValuesCheck.get();
  }
}
