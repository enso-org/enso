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
    var groupNumberRowVisitorFactory =
        new GroupNumberRowVisitorFactory(start, step, Math.toIntExact(numRows));
    GroupingOrderingVisitor.visit(
        groupingColumns,
        new Column[0],
        new int[0],
        problemAggregator,
        groupNumberRowVisitorFactory,
        numRows);
    return new LongStorage(groupNumberRowVisitorFactory.storageForResult, IntegerType.INT_64);
  }

  private static class GroupNumberRowVisitorFactory implements RowVisitorFactory {

    private long current;
    private final long step;
    long[] storageForResult;

    GroupNumberRowVisitorFactory(long start, long step, int size) {
      this.current = start;
      this.step = step;
      storageForResult = new long[size];
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      var nextGroupNumber = current;
      current = Math.addExact(current, step);
      return new GroupNumberRowVisitor(nextGroupNumber, storageForResult);
    }

    private static class GroupNumberRowVisitor implements GroupRowVisitor {
      private final long groupNumber;
      private final long[] storageForResult;

      GroupNumberRowVisitor(long groupNumber, long[] storageForResult) {
        this.groupNumber = groupNumber;
        this.storageForResult = storageForResult;
      }

      @Override
      public void visit(int row) {
        storageForResult[row] = groupNumber;
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
    var equalCountRowVisitorFactory =
        new EqualCountRowVisitorFactory(start, step, numRows, groupCount);
    GroupingOrderingVisitor.visit(
        new Column[0],
        orderingColumns,
        directions,
        problemAggregator,
        equalCountRowVisitorFactory,
        numRows);
    return new LongStorage(equalCountRowVisitorFactory.storageForResult, IntegerType.INT_64);
  }

  private static class EqualCountRowVisitorFactory implements RowVisitorFactory {

    private final long start;
    private final long step;
    private final long groupSize;
    long[] storageForResult;

    EqualCountRowVisitorFactory(long start, long step, long totalCount, long numgroups) {
      this.start = start;
      this.step = step;
      groupSize = (long) Math.ceil((double) totalCount / (double) numgroups);
      storageForResult = new long[Math.toIntExact(totalCount)];
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      return new EqualCountRowVisitor(this);
    }

    private static class EqualCountRowVisitor implements GroupRowVisitor {
      private final EqualCountRowVisitorFactory parent;
      private long currentIndex = 0;

      EqualCountRowVisitor(EqualCountRowVisitorFactory parent) {
        this.parent = parent;
      }

      @Override
      public void visit(int row) {
        parent.storageForResult[row] =
            Math.addExact(
                parent.start, Math.multiplyExact(parent.step, (currentIndex / parent.groupSize)));
        currentIndex = Math.addExact(currentIndex, 1L);
      }
    }
  }
}
