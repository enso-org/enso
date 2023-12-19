package org.enso.table.data.column.operation.cast;

import java.util.ArrayList;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

public class CastProblemAggregator extends ProblemAggregator {
  private final String columnName;
  private final StorageType targetType;
  private int failedConversionsCount = 0;
  private int numberOutOfRangeCount = 0;
  private int textTooLongCount = 0;

  private static final int MAX_EXAMPLES_COUNT = 3;
  private final ArrayList<Object> failedConversionExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);
  private final ArrayList<Object> numberOutOfRangeExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);
  private final ArrayList<String> textTooLongExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);

  public CastProblemAggregator(
      ProblemAggregator parent, String columnName, StorageType targetType) {
    super(parent);
    this.columnName = columnName;
    this.targetType = targetType;
  }

  public void reportConversionFailure(Object sourceValue) {
    failedConversionsCount++;

    if (failedConversionExamples.size() < MAX_EXAMPLES_COUNT) {
      failedConversionExamples.add(sourceValue);
    }
  }

  public void reportTextTooLong(String text) {
    textTooLongCount++;

    if (textTooLongExamples.size() < MAX_EXAMPLES_COUNT) {
      textTooLongExamples.add(text);
    }
  }

  public void reportNumberOutOfRange(Number number) {
    numberOutOfRangeCount++;

    if (numberOutOfRangeExamples.size() < MAX_EXAMPLES_COUNT) {
      numberOutOfRangeExamples.add(number);
    }
  }

  @Override
  public ProblemSummary summarize() {
    var problems = super.summarize();
    if (failedConversionsCount > 0) {
      problems.add(
          new ConversionFailure(
              ConversionFailureType.FAILED_CONVERSION,
              targetType,
              columnName,
              failedConversionsCount,
              failedConversionExamples));
    }

    if (numberOutOfRangeCount > 0) {
      problems.add(
          new ConversionFailure(
              ConversionFailureType.NUMBER_OUT_OF_RANGE,
              targetType,
              columnName,
              numberOutOfRangeCount,
              numberOutOfRangeExamples));
    }

    if (textTooLongCount > 0) {
      problems.add(
          new ConversionFailure(
              ConversionFailureType.TEXT_TOO_LONG,
              targetType,
              columnName,
              textTooLongCount,
              textTooLongExamples));
    }

    return problems;
  }
}
