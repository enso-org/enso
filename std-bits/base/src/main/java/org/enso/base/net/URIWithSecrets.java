package org.enso.base.net;

import org.enso.base.enso_cloud.EnsoKeyValuePair;

import java.net.URI;
import java.util.List;

public record URIWithSecrets(URI baseUri, List<EnsoKeyValuePair> queryParameters, UserInfoWithSecrets userInfo) {
}
