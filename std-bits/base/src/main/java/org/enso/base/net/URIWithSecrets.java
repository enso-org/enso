package org.enso.base.net;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.AbstractMap;
import java.util.List;
import org.enso.base.enso_cloud.EnsoHeader;
import org.enso.base.enso_cloud.HideableValue;

/**
 * A structure representing a URI that contains parts which may need to be updated once data from
 * secrets is resolved.
 *
 * <p>The query parameters are stored separately, because they may contain secrets and will only be
 * resolved to plain values within {@link org.enso.base.enso_cloud.EnsoSecretHelper}.
 */
public class URIWithSecrets {
  private final URI baseUri;
  private final List<EnsoHeader> queryParameters;

  public URIWithSecrets(String baseUri, List<EnsoHeader> queryParameters) {
    this.baseUri = URI.create(baseUri);
    this.queryParameters = queryParameters;
  }

  /** Creates a schematic that does not disclose secret values and can be returned to the user. */
  public URISchematic makeSchematicForRender() {
    var renderedParameters =
        queryParameters.stream()
            .map(
                p ->
                    new AbstractMap.SimpleEntry<>(
                        p.name(), HideableValue.from(p.hideable_value()).render()))
            .toList();
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
    return queryParameters.stream()
        .anyMatch(p -> HideableValue.from(p.hideable_value()).containsSecrets());
  }

  private URISchematic makeSchematicForSafeResolve() {
    var resolvedParameters =
        queryParameters.stream()
            .map(
                p ->
                    new AbstractMap.SimpleEntry<>(
                        p.name(), HideableValue.from(p.hideable_value()).safeResolve()))
            .toList();
    return new URISchematic(baseUri, resolvedParameters);
  }

  public String getScheme() {
    return baseUri.getScheme();
  }

  private URI forAuthorityPart() {
    // We can ignore secrets in the query part, because they are not used for resolving the
    // authority.
    return new URIWithSecrets(baseUri.toString(), List.of()).safeResolve();
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
