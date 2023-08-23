package org.enso.table.data.column.operation.cast;

import java.util.ArrayList;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.AggregatedProblems;

public class CastProblemBuilder {
  private final String columnName;
  private final StorageType targetType;
  private int failedConversionsCount = 0;
  private int numberOutOfRangeCount = 0;
  private int textTooLongCount = 0;

  private static final int MAX_EXAMPLES_COUNT = 3;
  private final ArrayList<Object> failedConversionExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);
  private final ArrayList<Object> numberOutOfRangeExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);
  private final ArrayList<String> textTooLongExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);
  private AggregatedProblems otherProblems = new AggregatedProblems();

  public CastProblemBuilder(String columnName, StorageType targetType) {
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

  public void aggregateOtherProblems(AggregatedProblems problems) {
    otherProblems = AggregatedProblems.merge(otherProblems, problems);
  }

  private AggregatedProblems summarizeMyProblems() {
    AggregatedProblems problems = new AggregatedProblems();

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

  public AggregatedProblems getAggregatedProblems() {
    return AggregatedProblems.merge(summarizeMyProblems(), otherProblems);
  }
}
