package org.enso.base.enso_cloud;

public class EnsoSecretAccessDenied extends RuntimeException {
  public EnsoSecretAccessDenied() {
    super("Access to secret denied.");
  }
}
