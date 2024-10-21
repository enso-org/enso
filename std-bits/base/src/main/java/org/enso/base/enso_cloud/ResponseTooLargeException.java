package org.enso.base.enso_cloud;

public class ResponseTooLargeException extends Exception {
  private final long size;
  private final long limit;

  public ResponseTooLargeException(long size, long limit) {
    super(
        "Response too large: repsonse size ("
            + size
            + ") is over the limit ("
            + limit
            + ")"

    this.size = size;
    this.limit = limit;
  }

  public long getSize() {
    return size;
  }

  public long getLimit() {
    return limit;
  }
}
