package org.enso.base.enso_cloud;

public sealed interface HideableValue {
}

record SecretValue(String secretId) implements HideableValue {}
record PlainValue(String value) implements HideableValue {}
record NothingValue() implements HideableValue {}