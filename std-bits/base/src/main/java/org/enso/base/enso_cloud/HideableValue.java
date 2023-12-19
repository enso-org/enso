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

  String render();

  String safeResolve() throws EnsoSecretAccessDenied;
}
