package org.enso.tableau;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

class HyperQueryError extends RuntimeException {
  private final String query;

  public HyperQueryError(String message, String query, Throwable cause) {
    super(message, cause);
    this.query = query;
  }

  public String getQuery() {
    return query;
  }

  public Value asEnsoAtom() {
    return EnsoMeta.makeInstance(
        "Standard.Tableau.Hyper_Errors", "Query_Failed", "Error", getMessage(), getQuery());
  }
}
