package org.enso.interpreter.runtime.state;

public record State(Object stateContainer, IOConfig ioConfig) {
  public State duplicate() { return this; }
}
