package org.enso.aws;

import org.enso.base.enso_cloud.HideableValue;

public sealed interface AwsCredential {
  record Key(HideableValue accessKeyId, HideableValue secretAccessKey) implements AwsCredential {}

  record Profile(String name) implements AwsCredential {}

  record Default() implements AwsCredential {}
}
