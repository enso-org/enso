package org.enso.ydoc.polyfill.nodejs;

import io.helidon.common.uri.UriQuery;
import io.helidon.http.HttpPrologue;
import org.graalvm.polyglot.HostAccess;

/**
 * The {@link URL} class implements the <a
 * href="https://nodejs.org/docs/latest-v20.x/api/url.html">URL</a> Node.js interface and can be
 * used directly for interop with JS.
 */
public final class URL {

  private final HttpPrologue prologue;

  @HostAccess.Export public final String pathname;

  @HostAccess.Export public final URLSearchParams searchParams;

  URL(HttpPrologue prologue) {
    this.prologue = prologue;

    this.pathname = prologue.uriPath().path();
    this.searchParams = new URLSearchParams(prologue.query());
  }

  @Override
  @HostAccess.Export
  public String toString() {
    return prologue.toString();
  }

  /** Implements Node.js URLSearchParams interface. */
  public static final class URLSearchParams {

    private final UriQuery uriQuery;

    URLSearchParams(UriQuery uriQuery) {
      this.uriQuery = uriQuery;
    }

    @HostAccess.Export
    public String get(String name) {
      return uriQuery.contains(name) ? uriQuery.get(name) : null;
    }

    @Override
    @HostAccess.Export
    public String toString() {
      return uriQuery.rawValue();
    }
  }
}
