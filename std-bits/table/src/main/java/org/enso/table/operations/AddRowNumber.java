package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public class AddRowNumber {

  public static Storage<?> create_numbering(
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
    var rowNumberFactory =
        new RowNumberRowVisitorFactory(start, step, sourceColumn.getSize(), problemAggregator);
    GroupingOrderingVisitor.visit(
        groupingColumns,
        orderingColumns,
        directions,
        problemAggregator,
        rowNumberFactory,
        sourceColumn.getSize());
    return new LongStorage(rowNumberFactory.numbers, IntegerType.INT_64);
  }

  private static class RowNumberRowVisitorFactory implements RowVisitorFactory {

    private final long start;
    private final long step;
    long[] numbers;

    RowNumberRowVisitorFactory(
        long start, long step, int size, ProblemAggregator problemAggregator) {
      this.start = start;
      this.step = step;
      numbers = new long[size];
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      return new RowNumberRowVisitor(start, step, numbers);
    }

    private static class RowNumberRowVisitor implements GroupRowVisitor {

      private final long start;
      private final long step;
      private long current;
      private boolean isFirst = true;
      private final long[] numbers;

      RowNumberRowVisitor(long start, long step, long[] numbers) {
        this.start = start;
        this.step = step;
        this.numbers = numbers;
      }

      @Override
      public void visit(int row) {
        numbers[row] = next();
      }

      public Long next() throws ArithmeticException {
        if (isFirst) {
          isFirst = false;
          current = start;
        } else {
          current = Math.addExact(current, step);
        }

        return current;
      }
    }
  }
}
