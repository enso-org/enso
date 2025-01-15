package org.enso.base.cache;

public class ResponseTooLargeException extends Exception {
  private final Long actualSize;
  private final long limit;

  public ResponseTooLargeException(Long actualSize, long limit) {
    super(
        "Response too large: response size"
            + (actualSize == null ? "" : " " + actualSize)
            + " is over the limit "
            + limit);

    this.actualSize = actualSize;
    this.limit = limit;
  }

  public long getLimit() {
    return limit;
  }

  public Long getActualSize() {
    return actualSize;
  }
}
