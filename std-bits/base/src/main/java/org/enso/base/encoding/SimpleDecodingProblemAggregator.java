package org.enso.base.encoding;

public class SimpleDecodingProblemAggregator extends DecodingProblemAggregator<Integer> {
  @Override
  protected Integer toLocation(int position) {
    return position;
  }
}
