package org.enso.base.enso_cloud;

/** An interface for Standard.Base.Network.HTTP.Header.Header can match */
public interface EnsoHeader {
  String name();

  EnsoHideableValue as_derived_secret();
}
