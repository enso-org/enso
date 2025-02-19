package org.enso.table.operations;

public interface GroupRowVisitor {

  void visit(int row);

  default void finalise() {}
  ;
}
