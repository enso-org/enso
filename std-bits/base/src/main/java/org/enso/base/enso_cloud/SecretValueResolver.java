package org.enso.base.enso_cloud;

sealed class SecretValueResolver permits EnsoSecretHelper, ExternalLibrarySecretHelper {
  /**
   * Gets the value of an HideableValue resolving secrets.
   *
   * @param value The value to resolve.
   * @return The pair's value. Should not be returned to Enso.
   */
  protected static String resolveValue(HideableValue value) {
    return switch (value) {
      case HideableValue.PlainValue plainValue -> plainValue.value();
      case HideableValue.SecretValue secretValue -> {
        yield EnsoSecretReader.readSecret(secretValue.secretId());
      }
      case HideableValue.ConcatValues concatValues -> {
        String left = resolveValue(concatValues.left());
        String right = resolveValue(concatValues.right());
        yield left + right;
      }
      case HideableValue.Base64EncodeValue base64EncodeValue -> HideableValue.Base64EncodeValue
          .encode(resolveValue(base64EncodeValue.value()));
    };
  }
}
