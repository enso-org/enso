package org.enso.base.enso_cloud;

import java.util.List;

public interface EnsoHideableValue {
  String value_type();

  String text_value();

  List<EnsoHideableValue> children();

  default HideableValue toHideableValue() {
    return switch (value_type()) {
      case PLAIN_TYPE -> HideableValue.plain(text_value());
      case SECRET_TYPE -> HideableValue.secret(text_value());
      case BASE64_TYPE -> HideableValue.base64(children().get(0).toHideableValue());
      case CONCAT_TYPE ->
          HideableValue.concat(
              children().get(0).toHideableValue(), children().get(1).toHideableValue());
      case PRIVATE_KEY_TYPE -> HideableValue.privateKey(children().get(0).toHideableValue());
      default -> throw new IllegalArgumentException("Unknown HideableValue type: " + value_type());
    };
  }

  String PLAIN_TYPE = "plain";
  String SECRET_TYPE = "secret";
  String BASE64_TYPE = "base64";
  String CONCAT_TYPE = "concat";
  String PRIVATE_KEY_TYPE = "privateKey";
}
