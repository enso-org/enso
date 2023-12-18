package org.enso.interpreter.epb;

final class ExceptionForeignNode extends ForeignFunctionCallNode {
  private final ForeignParsingException ex;

  ExceptionForeignNode(ForeignParsingException ex) {
    this.ex = ex;
  }

  @Override
  public Object execute(Object[] arguments) {
    throw ex;
  }
}
