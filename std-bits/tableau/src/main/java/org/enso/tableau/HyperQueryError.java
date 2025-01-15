package org.enso.tableau;

public class HyperQueryError extends RuntimeException {
  private final String query;

  public HyperQueryError(String message, String query, Throwable cause) {
    super(message, cause);
    this.query = query;
  }

  public String getQuery() {
    return query;
  }
}
