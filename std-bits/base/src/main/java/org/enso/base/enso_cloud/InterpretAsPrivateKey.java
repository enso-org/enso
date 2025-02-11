package org.enso.base.enso_cloud;

import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Base64;

public record InterpretAsPrivateKey(HideableValue value) implements HideableValue {
  @Override
  public String render() {
    return "<private key>";
  }

  @Override
  public String safeResolve() throws EnsoSecretAccessDenied {
    throw new IllegalArgumentException(
        "InterpretAsPrivateKey should only be used in context of JDBC.");
  }

  @Override
  public boolean containsSecrets() {
    // We treat the private key as secret even if it is not passed as a secret value.
    return true;
  }

  static PrivateKey decodePrivateKey(String key) {
    try {
      KeyFactory factory = KeyFactory.getInstance("RSA");
      KeySpec spec = new PKCS8EncodedKeySpec(Base64.getMimeDecoder().decode(trimKey(key)));
      return factory.generatePrivate(spec);
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("Unexpected: the JVM lacks support for RSA algorithm.");
    } catch (InvalidKeySpecException e) {
      throw new IllegalStateException("Encountered a private key is in invalid format.");
    }
  }

  private static String trimKey(String key) {
    key = key.trim();
    if (key.startsWith(PRIVATE_KEY_PREFIX)) {
      key = key.substring(PRIVATE_KEY_PREFIX.length());
    }
    if (key.endsWith(PRIVATE_KEY_SUFFIX)) {
      key = key.substring(0, key.length() - PRIVATE_KEY_SUFFIX.length());
    }
    return key.trim();
  }

  private static final String PRIVATE_KEY_PREFIX = "-----BEGIN PRIVATE-----";
  private static final String PRIVATE_KEY_SUFFIX = "-----END PRIVATE-----";
}
