package org.enso.base.enso_cloud;

import java.nio.charset.StandardCharsets;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Base64;

/**
 * Various implementations of {@link HideableValue}. They are intended to be hidden from public and
 * only used inside of this package. Public access shall be performed either via {@link
 * HideableValue#plain(java.lang.String)} and other factory methods or via public instance methods
 * of {@link HideableValue}.
 */
final class HideableImpl {

  record SecretValue(String secretId) implements HideableValue {

    @Override
    public String render() {
      return "__SECRET__";
    }

    @Override
    public String safeResolve() throws EnsoSecretAccessDenied {
      throw new EnsoSecretAccessDenied();
    }

    @Override
    public boolean containsSecrets() {
      return true;
    }

    @Override
    public String toString() {
      return "{{Secret:" + secretId() + "}}";
    }
  }

  record PlainValue(String value) implements HideableValue {

    @Override
    public String render() {
      return value;
    }

    @Override
    public String safeResolve() throws EnsoSecretAccessDenied {
      return value;
    }

    @Override
    public boolean containsSecrets() {
      return false;
    }

    @Override
    public String toString() {
      return value();
    }
  }

  record ConcatValues(HideableValue left, HideableValue right) implements HideableValue {
    @Override
    public String render() {
      return left.render() + right.render();
    }

    @Override
    public String safeResolve() throws EnsoSecretAccessDenied {
      return left.safeResolve() + right.safeResolve();
    }

    @Override
    public boolean containsSecrets() {
      return left.containsSecrets() || right.containsSecrets();
    }

    @Override
    public String toString() {
      return left() + "_" + right();
    }
  }

  record Base64EncodeValue(HideableValue value) implements HideableValue {

    @Override
    public String render() {
      if (value.containsSecrets()) {
        // If the value contains secrets, we cannot encode it so we render as 'pseudocode'
        return "base64(" + value.render() + ")";
      } else {
        // But if there are no secrets inside, there is no harm in encoding for preview.
        return encode(value.render());
      }
    }

    @Override
    public String safeResolve() throws EnsoSecretAccessDenied {
      return encode(value.safeResolve());
    }

    @Override
    public boolean containsSecrets() {
      return value.containsSecrets();
    }

    public static String encode(String value) {
      return Base64.getEncoder().encodeToString(value.getBytes(StandardCharsets.UTF_8));
    }

    @Override
    public String toString() {
      return "base64_" + value();
    }
  }

  record InterpretAsPrivateKey(HideableValue value) implements HideableValue {

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

    @Override
    public String toString() {
      // originally that would throw
      // new IllegalArgumentException("Unexpected value: " + value);
      return "private_key_" + value();
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
}
