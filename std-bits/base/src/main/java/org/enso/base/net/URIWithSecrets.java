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

  public URI render() throws URISyntaxException {
    return makeSchematicForRender().build();
  }
}
