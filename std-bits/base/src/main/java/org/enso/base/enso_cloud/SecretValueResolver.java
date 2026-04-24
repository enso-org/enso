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
      case HideableImpl.PlainValue plainValue -> plainValue.value();
      case HideableImpl.SecretValue secretValue ->
          EnsoSecretReader.INSTANCE.readSecret(secretValue.secretId());
      case HideableImpl.ConcatValues concatValues -> {
        String left = resolveValue(concatValues.left());
        String right = resolveValue(concatValues.right());
        yield left + right;
      }
      case HideableImpl.Base64EncodeValue base64EncodeValue ->
          HideableImpl.Base64EncodeValue.encode(resolveValue(base64EncodeValue.value()));
      case HideableImpl.InterpretAsPrivateKey pk ->
          throw new IllegalStateException(
              "InterpretAsPrivateKey can only be used in JDBC connections. This state should never"
                  + " be reached.");
    };
  }
}
