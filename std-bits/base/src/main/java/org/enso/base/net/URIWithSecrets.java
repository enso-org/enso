package org.enso.base.net;

import org.enso.base.enso_cloud.HideableValue;
import org.graalvm.collections.Pair;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public record URIWithSecrets(
    URI baseUri,
    List<Pair<String, HideableValue>> queryParameters,
    UserInfoWithSecrets userInfo
) {

  /**
   * Creates a schematic that does not disclose secret values and can be returned to the user.
   */
  public URISchematic makeSchematicForRender() {
    List<Pair<String, String>> renderedParameters = queryParameters.stream()
        .map(p -> Pair.create(p.getLeft(), p.getRight().render()))
        .toList();
    Pair<String, String> renderedUserInfo = userInfo == null ? null : Pair.create(userInfo.username().render(), userInfo.password().render());
    return new URISchematic(baseUri, renderedParameters, renderedUserInfo);
  }

  public URI render() {
    try {
      return makeSchematicForRender().build();
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
  }

  /** Resolves to a proper URI if it does not contain any secrets. If there was a secret, it throws an exception. */
  public URI safeResolve() {
    try {
      return makeSchematicForSafeResolve().build();
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
  }

  private URISchematic makeSchematicForSafeResolve() {
    List<Pair<String, String>> resolvedParameters = queryParameters.stream()
        .map(p -> Pair.create(p.getLeft(), p.getRight().safeResolve()))
        .toList();
    Pair<String, String> resolvedUserInfo = userInfo == null ? null : Pair.create(userInfo.username().safeResolve(), userInfo.password().safeResolve());
    return new URISchematic(baseUri, resolvedParameters, resolvedUserInfo);
  }

  public String getScheme() {
    return baseUri.getScheme();
  }

  private URIWithSecrets forAuthorityPart() {
    // We can ignore secrets in the query part, because they are not used for resolving the authority.
    return new URIWithSecrets(baseUri, List.of(), userInfo);
  }

  public String getUserInfo() {
    return this.forAuthorityPart().safeResolve().getUserInfo();
  }

  public String getRawUserInfo() {
    return this.forAuthorityPart().safeResolve().getRawUserInfo();
  }

  public String getHost() {
    // This is not affected by secrets at all, so we can rely on the baseUri.
    return this.baseUri.getHost();
  }

  public int getPort() {
    return this.baseUri.getPort();
  }

  public String getAuthority() {
    return this.forAuthorityPart().safeResolve().getAuthority();
  }

  public String getRawAuthority() {
    return this.forAuthorityPart().safeResolve().getRawAuthority();
  }

  public String getPath() {
    return baseUri.getPath();
  }

  public String getRawPath() {
    return baseUri.getRawPath();
  }

  private URIWithSecrets forQueryPart() {
    // We can ignore secrets in the authority part, because they are not used for resolving the query.
    return new URIWithSecrets(baseUri, queryParameters, null);
  }

  public String getQuery() {
    return this.forQueryPart().safeResolve().getQuery();
  }

  public String getRawQuery() {
    return this.forQueryPart().safeResolve().getRawQuery();
  }

  public String getFragment() {
    return baseUri.getFragment();
  }

  public String getRawFragment() {
    return baseUri.getRawFragment();
  }
}
