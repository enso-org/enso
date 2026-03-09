package org.enso.base.enso_cloud;

import java.util.List;

public interface EnsoHideableValue {
  String value_type();

  String text_value();

  List<EnsoHideableValue> children();

  String PLAIN_TYPE = "plain";
  String SECRET_TYPE = "secret";
  String BASE64_TYPE = "base64";
  String CONCAT_TYPE = "concat";
  String PRIVATE_KEY_TYPE = "privateKey";
}
