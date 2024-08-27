package org.enso.ydoc.polyfill.web;

import io.helidon.common.http.Parameters;
import io.helidon.common.http.UriComponent;
import org.graalvm.polyglot.HostAccess;

import java.net.URI;

/**
 * Implements the <a href="https://nodejs.org/api/url.html">URL</a> Node.js interface and can be
 * used directly for interop with JS.
 */
public final class URL {

  private final URI uri;

  @HostAccess.Export public final String pathname;

  @HostAccess.Export public final URLSearchParams searchParams;

  URL(URI uri) {
    this.uri = uri;
    this.pathname = uri.getRawPath();
    this.searchParams = new URLSearchParams(UriComponent.decodeQuery(uri.getRawQuery(), true));
  }

  @Override
  @HostAccess.Export
  public String toString() {
    return uri.toString();
  }

  /**
   * Implements the <a
   * href="https://nodejs.org/api/url.html#class-urlsearchparams">URLSearchParams</a> Node.js
   * interface.
   */
  public static final class URLSearchParams {

    private final Parameters parameters;

    URLSearchParams(Parameters parameters) {
      this.parameters = parameters;
    }

    @HostAccess.Export
    public String get(String name) {
      return parameters.first(name).orElse(null);
    }

    @Override
    @HostAccess.Export
    public String toString() {
      return parameters.toString();
    }
  }
}
