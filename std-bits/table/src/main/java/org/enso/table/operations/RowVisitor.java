package org.enso.table.operations;

public interface RowVisitor {

  void visit(int row);

  default void finalise() {}
  ;
}
