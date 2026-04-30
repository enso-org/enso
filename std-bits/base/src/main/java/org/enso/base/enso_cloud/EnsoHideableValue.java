package org.enso.base.enso_cloud;

import java.util.List;

/**
 * This is a Java view of ` Standard.Base.Enso_Cloud.Enso_Secret.Derived_Secret_Value`. It allows
 * one to pass instances of `Derived_Secret_Value` into `HideableValue.from` method getting the core
 * values from the Enso object.
 */
public interface EnsoHideableValue {
  String value_type();

  String text_value();

  List<EnsoHideableValue> children();

  String PLAIN_TYPE = "plain";
  String SECRET_TYPE = "secret";
  String BASE64_TYPE = "base64";
  String CONCAT_TYPE = "concat";
  String PRIVATE_KEY_TYPE = "privateKey";

  static boolean containsSecrets(EnsoHideableValue value) {
    return HideableValue.from(value).containsSecrets();
  }

  static String safeResolve(EnsoHideableValue value) {
    return HideableValue.from(value).safeResolve();
  }

  static String render(EnsoHideableValue value) {
    return HideableValue.from(value).render();
  }

  default String uniqueId() {
    return HideableValue.from(this).toString();
  }
}
