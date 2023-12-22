package org.enso.base.enso_cloud;

/** Represents a value that is input of various operation that may contain a Secret. */
public sealed interface HideableValue permits HideableValue.SecretValue, HideableValue.PlainValue {

  record SecretValue(String secretId) implements HideableValue {
    @Override
    public String render() {
      return "__SECRET__";
    }

    @Override
    public String safeResolve() throws EnsoSecretAccessDenied {
      throw new EnsoSecretAccessDenied();
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
}
