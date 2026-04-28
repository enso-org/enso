package org.enso.aws;

import java.util.List;
import org.enso.base.enso_cloud.EnsoHideableValue;

public sealed interface AwsCredential {
  record Key(EnsoHideableValue accessKeyId, EnsoHideableValue secretAccessKey)
      implements AwsCredential {}

  record Profile(String name) implements AwsCredential {}

  record Default() implements AwsCredential {}

  static AwsCredential create(String credentialType, List<EnsoHideableValue> parameters) {
    return switch (credentialType) {
      case DEFAULT_TYPE -> new Default();
      case PROFILE_TYPE -> new Profile(parameters.get(0).text_value());
      case KEY_TYPE -> new Key(parameters.get(0), parameters.get(1));
      default -> throw new IllegalArgumentException("Unknown credential type: " + credentialType);
    };
  }

  String DEFAULT_TYPE = "default";
  String PROFILE_TYPE = "profile";
  String KEY_TYPE = "key";
}
