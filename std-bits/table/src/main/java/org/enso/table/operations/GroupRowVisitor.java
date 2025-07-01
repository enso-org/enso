package org.enso.table.operations;

public interface GroupRowVisitor {

  void visit(long row);

  default void finalise() {}
  ;
}
