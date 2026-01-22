package org.enso.base.enso_cloud;

/**
 * Represents a hidden value. Such a value can be an input of various operation that may contain a
 * Secret.
 */
public sealed interface HideableValue
    permits HideableImpl.Base64EncodeValue,
        HideableImpl.ConcatValues,
        HideableImpl.InterpretAsPrivateKey,
        HideableImpl.PlainValue,
        HideableImpl.SecretValue {

  record KeyValuePair(String key, HideableValue value) {
    String first() {
      return key;
    }

    HideableValue second() {
      return value;
    }
  }

  class Factory {
    public HideableValue plain(String value) {
      return HideableValue.plain(value);
    }

    public HideableValue secret(String secretId) {
      return HideableValue.secret(secretId);
    }

    public HideableValue concat(HideableValue left, HideableValue right) {
      return HideableValue.concat(left, right);
    }

    public HideableValue base64(HideableValue inner) {
      return HideableValue.base64(inner);
    }

    public HideableValue privateKey(HideableValue k) {
      return HideableValue.privateKey(k);
    }

    public boolean isAnInstance(Object obj) {
      return obj instanceof HideableValue;
    }

    public HideableValue.KeyValuePair createPair(String key, HideableValue value) {
      return new HideableValue.KeyValuePair(key, value);
    }
  }

  /**
   * Creates new instance of plain value. It doesn't {@link #containsSecrets()} and {@link
   * #render()} exactly as the provided value.
   *
   * @param value the provided value
   * @return new instance of hideable value with provided value
   */
  static HideableValue plain(String value) {
    return new HideableImpl.PlainValue(value);
  }

  /**
   * Creates new instance of a secret value. The value {@link #containsSecrets() contains secrets}.
   *
   * @param secretId the ID of the secret
   * @return new instance of hideable value with provided value
   */
  static HideableValue secret(String secretId) {
    return new HideableImpl.SecretValue(secretId);
  }

  /**
   * Concatenates two hideable values into one.
   *
   * @param left first hideable value
   * @param right second hideable value
   * @return new instance of hideable value
   */
  static HideableValue concat(HideableValue left, HideableValue right) {
    return new HideableImpl.ConcatValues(left, right);
  }

  /**
   * Creates new base64 encoded value.
   *
   * @param inner the value to encode
   * @return new instance of hideable value
   */
  static HideableValue base64(HideableValue inner) {
    return new HideableImpl.Base64EncodeValue(inner);
  }

  /**
   * Creates a value interpreted as private key.
   *
   * @param k
   * @return new instance of hideable value
   */
  static HideableValue privateKey(HideableValue k) {
    return new HideableImpl.InterpretAsPrivateKey(k);
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

  /**
   * Does this value contain some secrets.
   *
   * @return
   */
  boolean containsSecrets();

  /** Creates a unique string representation of the value. */
  String toString();
}
