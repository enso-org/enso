package org.enso.base.encoding;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class DecodingProblemAggregator {
  private final List<DecodingProblem> baseProblems = new ArrayList<>();
  private int invalidUnitCount = 0;
  private String invalidCharacterErrorPrefix = "";
  private final List<Integer> invalidUnitExamplePositions = new ArrayList<>();
  private static final int MAX_ENCODING_ISSUE_EXAMPLES = 3;

  public void reportOtherProblem(String message) {
    baseProblems.add(new DecodingProblem(message));
  }

  /**
   * Sets message prefix for the problem indicating characters that could not be decoded.
   *
   * <p>This prefix can be used to provide additional context for the problem.
   */
  public void setInvalidCharacterErrorPrefix(String prefix) {
    invalidCharacterErrorPrefix = prefix;
  }

  public void reportInvalidCharacterProblem(int position) {
    invalidUnitCount++;
    if (invalidUnitExamplePositions.size() < MAX_ENCODING_ISSUE_EXAMPLES) {
      invalidUnitExamplePositions.add(position);
    }
  }

  private DecodingProblem summarizeInvalidCharacterProblems() {
    if (invalidUnitCount == 0) {
      return null;
    }

    String positions =
        invalidUnitExamplePositions.stream()
            .map(Object::toString)
            .collect(Collectors.joining(", "));
    String suffix = invalidUnitCount > invalidUnitExamplePositions.size() ? ", ..." : "";
    return new DecodingProblem(
        invalidCharacterErrorPrefix
            + "Failed to decode "
            + invalidUnitCount
            + " code units (at positions: "
            + positions
            + suffix
            + ").");
  }

  public List<DecodingProblem> summarize() {
    var problems = new ArrayList<>(baseProblems);
    var invalidCharacterProblem = summarizeInvalidCharacterProblems();
    if (invalidCharacterProblem != null) {
      problems.add(invalidCharacterProblem);
    }
    return problems;
  }
}
