package org.enso.base.encoding;

public class SimpleDecodingProblemAggregator extends DecodingProblemAggregator<SimpleDecodingProblemAggregator.Location> {
  @Override
  protected Location toLocation(int position) {
    return new Location(position);
  }

  public record Location(int position) {
      @Override
      public String toString() {
          return Integer.toString(position);
      }
  }
}
