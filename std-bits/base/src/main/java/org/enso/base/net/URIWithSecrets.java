package org.enso.base.net;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import org.enso.base.enso_cloud.HideableValue;
import org.graalvm.collections.Pair;

/**
 * A structure representing a URI that contains parts which may need to be updated once data from
 * secrets is resolved.
 *
 * <p>The query parameters are stored separately, because they may contain secrets and will only be
 * resolved to plain values within {@link org.enso.base.enso_cloud.EnsoSecretHelper}.
 */
public record URIWithSecrets(URI baseUri, List<Pair<String, HideableValue>> queryParameters) {

  /** Creates a schematic that does not disclose secret values and can be returned to the user. */
  public URISchematic makeSchematicForRender() {
    List<Pair<String, String>> renderedParameters =
        queryParameters.stream().map(p -> Pair.create(p.getLeft(), p.getRight().render())).toList();
    return new URISchematic(baseUri, renderedParameters);
  }

  public URI render() {
    try {
      return makeSchematicForRender().build();
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * Resolves to a proper URI if it does not contain any secrets. If there was a secret, it throws
   * an exception.
   */
  public URI safeResolve() {
    try {
      return makeSchematicForSafeResolve().build();
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
  }

  public boolean containsSecrets() {
    return queryParameters.stream().anyMatch(p -> p.getRight().containsSecrets());
  }

  private URISchematic makeSchematicForSafeResolve() {
    List<Pair<String, String>> resolvedParameters =
        queryParameters.stream()
            .map(p -> Pair.create(p.getLeft(), p.getRight().safeResolve()))
            .toList();
    return new URISchematic(baseUri, resolvedParameters);
  }

  public String getScheme() {
    return baseUri.getScheme();
  }

  private URI forAuthorityPart() {
    // We can ignore secrets in the query part, because they are not used for resolving the
    // authority.
    return new URIWithSecrets(baseUri, List.of()).safeResolve();
  }

  public String getUserInfo() {
    return this.forAuthorityPart().getUserInfo();
  }

  public String getRawUserInfo() {
    return this.forAuthorityPart().getRawUserInfo();
  }

  public String getHost() {
    // This is not affected by secrets at all, so we can rely on the baseUri.
    return baseUri.getHost();
  }

  public int getPort() {
    return baseUri.getPort();
  }

  public String getAuthority() {
    return this.forAuthorityPart().getAuthority();
  }

  public String getRawAuthority() {
    return this.forAuthorityPart().getRawAuthority();
  }

  public String getPath() {
    return baseUri.getPath();
  }

  public String getRawPath() {
    return baseUri.getRawPath();
  }

  private URI forQueryPart() {
    return safeResolve();
  }

  public String getQuery() {
    return this.forQueryPart().getQuery();
  }

  public String getRawQuery() {
    return this.forQueryPart().getRawQuery();
  }

  public String getFragment() {
    return baseUri.getFragment();
  }

  public String getRawFragment() {
    return baseUri.getRawFragment();
  }
}
