package org.enso.base.statistics;

/*
 A class for exceptions thrown when fitting a model.
*/
public class FitError extends Exception {
  public FitError(String message) {
    super(message);
  }
}
