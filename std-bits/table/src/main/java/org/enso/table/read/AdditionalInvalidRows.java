package org.enso.table.read;

public class AdditionalInvalidRows implements ParsingProblem {
public final long count;

  public AdditionalInvalidRows(long count) {
    this.count = count;
  }
}
