package org.enso.base.enso_cloud;

public class ResponseTooLargeException extends Exception {
    private final long size;
    private final long limit;
    private final String url;

  public ResponseTooLargeException (long size, long limit, String url) {
    super(
        "Response too large: repsonse size (" + size + ") is over the limit (" + limit + ")" +
            (url == null ? "" : "(URL " + url + ")"));

    this.size = size;
    this.limit = limit;
    this.url = url;
  }

  public long getSize() {
    return size;
  }

  public long getLimit() {
    return limit;
  }

  public String getURL() {
    return url;
  }
}
