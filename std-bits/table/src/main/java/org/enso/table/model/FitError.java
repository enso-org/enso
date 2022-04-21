package org.enso.table.model;

/*
  Exception class when fitting a model
 */
public class FitError extends Exception {
  public FitError(String message) {
    super(message);
  }
}
