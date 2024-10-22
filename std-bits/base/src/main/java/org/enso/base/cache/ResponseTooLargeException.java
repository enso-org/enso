package org.enso.base.cache;

public class ResponseTooLargeException extends Exception {
  private final long limit;

  public ResponseTooLargeException(long limit) {
    super("Response too large: repsonse size is over the limit (" + limit + ")");

    this.limit = limit;
  }

  public long getLimit() {
    return limit;
  }
}
