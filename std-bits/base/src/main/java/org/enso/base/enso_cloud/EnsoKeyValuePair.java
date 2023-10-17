package org.enso.base.enso_cloud;

public record EnsoKeyValuePair(String key, String value, String secretId) {
  public static EnsoKeyValuePair ofNothing(String key) {
    return new EnsoKeyValuePair(key, null, null);
  }

  public static EnsoKeyValuePair ofText(String key, String value) {
    return new EnsoKeyValuePair(key, value, null);
  }

  public static EnsoKeyValuePair ofSecret(String key, String secretId) {
    return new EnsoKeyValuePair(key, null, secretId);
  }
}
