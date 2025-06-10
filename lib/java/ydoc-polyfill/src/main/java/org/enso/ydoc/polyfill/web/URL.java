package org.enso.ydoc.polyfill.web;

import io.helidon.common.uri.UriQuery;
import io.helidon.http.HttpPrologue;
import org.graalvm.polyglot.HostAccess;

/**
 * Implements the <a href="https://nodejs.org/api/url.html">URL</a> Node.js interface and can be
 * used directly for interop with JS.
 *
 * <p>Note that {@code HostAccess.Export} entries are not detected by the Native Image Agent {@code
 * -agentlib:native-image-agent} and should be manually listed in the reflect-config.json.
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

  /**
   * Implements the <a
   * href="https://nodejs.org/api/url.html#class-urlsearchparams">URLSearchParams</a> Node.js
   * interface.
   */
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
