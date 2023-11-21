package org.enso.base.enso_cloud;

record EnsoKeySecretPair(String key, String secretId) implements EnsoKeyValuePair {}

record EnsoKeyStringPair(String key, String value) implements EnsoKeyValuePair {}

public sealed interface EnsoKeyValuePair permits EnsoKeySecretPair, EnsoKeyStringPair {
  String key();

  static EnsoKeyValuePair ofNothing(String key) {
    return new EnsoKeyStringPair(key, null);
  }

  static EnsoKeyValuePair ofText(String key, String value) {
    return new EnsoKeyStringPair(key, value);
  }

  static EnsoKeyValuePair ofSecret(String key, String secretId) {
    return new EnsoKeySecretPair(key, secretId);
  }
}
