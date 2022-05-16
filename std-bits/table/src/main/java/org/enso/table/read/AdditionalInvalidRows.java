package org.enso.table.read;

/** A problem which indicates how many additional invalid rows were encountered. */
public class AdditionalInvalidRows implements ParsingProblem {
  public final long count;

  public AdditionalInvalidRows(long count) {
    this.count = count;
  }
}
