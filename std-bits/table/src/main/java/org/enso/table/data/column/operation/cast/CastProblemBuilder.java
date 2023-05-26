package org.enso.table.data.column.operation.cast;

import java.util.ArrayList;
import java.util.List;

public class CastProblemBuilder {
  private int failedConversionsCount = 0;
  private int textTooLongCount = 0;

  private static final int MAX_EXAMPLES_COUNT = 3;
  private final ArrayList<Object> failedConversionExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);
  private final ArrayList<String> textTooLongExamples = new ArrayList<>(MAX_EXAMPLES_COUNT);

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

  public int getFailedConversionsCount() {
    return failedConversionsCount;
  }

  public List<Object> getFailedConversionExamples() {
    return failedConversionExamples;
  }

  public int getTextTooLongCount() {
    return textTooLongCount;
  }

  public List<String> getTextTooLongExamples() {
    return textTooLongExamples;
  }
}
