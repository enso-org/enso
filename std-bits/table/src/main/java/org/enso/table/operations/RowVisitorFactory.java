package org.enso.table.operations;

public interface RowVisitorFactory {

  GroupRowVisitor getNewRowVisitor();
}
