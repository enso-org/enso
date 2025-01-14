package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public class AddGroupNumber {
  public static Storage<?> numberGroupsUnique(
      long numRows,
      long start,
      long step,
      Column[] groupingColumns,
      ProblemAggregator problemAggregator) {
    if (groupingColumns.length == 0) {
      throw new IllegalArgumentException("At least one grouping column is required.");
    }
    var groupNumberRowVisitorFactory = new GroupNumberRowVisitorFactory(start, step, Math.toIntExact(numRows));
    GroupingOrderingVisitor.visit(groupingColumns,
        new Column[0],
        new int[0],
        problemAggregator,
        groupNumberRowVisitorFactory,
        numRows);
    return new LongStorage(groupNumberRowVisitorFactory.numbers, IntegerType.INT_64);
  }

  private static class GroupNumberRowVisitorFactory implements RowVisitorFactory {

    private long current;
    private final long step;
    long[] numbers;
  
    GroupNumberRowVisitorFactory(
      long start, long step, int size) {
      this.current = start;
      this.step = step;
      numbers = new long[size];
    }

    @Override
    public RowVisitor getNewRowVisitor() {
      var nextGroupNumber = current;
      current = Math.addExact(current, step);
      return new GroupNumberRowVisitor(nextGroupNumber, numbers);
    }
    private static class GroupNumberRowVisitor implements RowVisitor {
      private final long groupNumber;
      private final long[] numbers;

      GroupNumberRowVisitor(long groupNumber, long[] numbers) {
        this.groupNumber = groupNumber;
        this.numbers = numbers;
      }

      @Override
      public void visit(int row) {
          numbers[row] = groupNumber;
      }
    }
  }

  public static Storage<?> numberGroupsEqualCount(
      long numRows,
      int groupCount,
      long start,
      long step,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    var equalCountRowVisitorFactory = new EqualCountRowVisitorFactory(start, step, numRows, groupCount);
    GroupingOrderingVisitor.visit(
        new Column[0],
        orderingColumns, 
        directions,
        problemAggregator,
        equalCountRowVisitorFactory,
        numRows);
    return new LongStorage(equalCountRowVisitorFactory.numbers, IntegerType.INT_64);
  }
 
  private static class EqualCountRowVisitorFactory implements RowVisitorFactory {

    private final long start;
    private final long step;
    private final long groupSize;
    long[] numbers;
    
    EqualCountRowVisitorFactory(
      long start, long step, long totalCount, long numgroups) {
      this.start = start;
      this.step = step;
      groupSize = (long) Math.ceil((double) totalCount / (double) numgroups);
      numbers = new long[Math.toIntExact(totalCount)];
    }

    @Override
    public RowVisitor getNewRowVisitor() {
      return new EqualCountRowVisitor(start, step, groupSize, numbers);
    }
    private static class EqualCountRowVisitor implements RowVisitor {
      private final long start;
      private final long step;
      private long currentIndex = 0;
      private final long groupSize;
      long[] numbers;

      EqualCountRowVisitor(long start, long step, long groupSize, long[] numbers) {
          this.start = start;
          this.step = step;
          this.groupSize = groupSize;
          this.numbers = numbers;
      }

      @Override
      public void visit(int row) {
        numbers[row] = Math.addExact(start, Math.multiplyExact(step, (currentIndex / groupSize)));
        currentIndex = Math.addExact(currentIndex, 1L);
      }
    }
  }
}
