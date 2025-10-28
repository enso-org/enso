package org.enso.table.operations;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForLong;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public class AddRowNumber {
  public static ColumnStorage<?> createNumbering(
      long start,
      long step,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    if (groupingColumns.length == 0 && orderingColumns.length == 0) {
      throw new IllegalArgumentException("At least one grouping or ordering column is required.");
    }
    var sourceColumn = groupingColumns.length > 0 ? groupingColumns[0] : orderingColumns[0];
    var visitorFactory =
        new RowNumberRowVisitorFactory(start, step, sourceColumn.getSize(), problemAggregator);
    return GroupingOrderingVisitor.visit(
        groupingColumns,
        orderingColumns,
        directions,
        problemAggregator,
        visitorFactory,
        sourceColumn.getSize());
  }

  private static class RowNumberRowVisitorFactory implements RowVisitorFactory {
    private final long start;
    private final long step;
    private final BuilderForLong builder;

    RowNumberRowVisitorFactory(
        long start, long step, int size, ProblemAggregator problemAggregator) {
      this.start = start;
      this.step = step;
      this.builder = Builder.getForLong(IntegerType.INT_64, size, problemAggregator);
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      return new RowNumberRowVisitor(this);
    }

    @Override
    public ColumnStorage<?> seal() {
      return builder.seal();
    }

    private static class RowNumberRowVisitor implements GroupRowVisitor {
      private final RowNumberRowVisitorFactory parent;
      private long current;

      RowNumberRowVisitor(RowNumberRowVisitorFactory parent) {
        this.parent = parent;
        this.current = parent.start;
      }

      @Override
      public void visit(long row) {
        parent.builder.appendLong(next());
      }

      public Long next() throws ArithmeticException {
        long result = current;
        current = Math.addExact(current, parent.step);
        return result;
      }
    }
  }
}
