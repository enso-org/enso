package org.enso.base.enso_cloud;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

/** Represents a value that is input of various operation that may contain a Secret. */
public sealed interface HideableValue
    permits HideableValue.Base64EncodeValue,
        HideableValue.ConcatValues,
        HideableValue.PlainValue,
        HideableValue.SecretValue {

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
  }

  /**
   * Returns a display-oriented representation of the value, replacing secrets with a placeholder.
   */
  String render();

  /**
   * Returns the plain text representation of the value, throwing an exception if it contained a
   * secret.
   */
  String safeResolve() throws EnsoSecretAccessDenied;

  boolean containsSecrets();
}
