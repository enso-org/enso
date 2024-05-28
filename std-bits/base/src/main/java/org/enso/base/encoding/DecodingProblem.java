package org.enso.base.encoding;

import java.util.List;
import java.util.stream.Collectors;

public record DecodingProblem(int count, List<Integer> examplePositions) {
  public String getMessage() {
    String positions = examplePositions.stream().map(Object::toString).collect(Collectors.joining(", "));
    String suffix = count > examplePositions.size() ? ", ..." : "";
    return "Failed to decode " + count + " code units (at positions " + positions + suffix + ").";
  }
}
