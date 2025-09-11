package org.enso.table.operations;

import java.util.function.BiFunction;
import org.enso.base.ProgressReporter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForLong;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Row;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.problems.IllegalArgumentError;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public class AddGroupNumber {
  public static ColumnStorage<?> numberGroupsUnique(
      long numRows,
      long start,
      long step,
      Column[] groupingColumns,
      ProblemAggregator problemAggregator) {
    if (groupingColumns.length == 0) {
      throw new IllegalArgumentException("At least one grouping column is required.");
    }

    var visitorFactory = new GroupNumberRowVisitorFactory(start, step, numRows, problemAggregator);
    return GroupingOrderingVisitor.visit(
        groupingColumns, new Column[0], new int[0], problemAggregator, visitorFactory, numRows);
  }

  private static class GroupNumberRowVisitorFactory implements RowVisitorFactory {
    private long current;
    private final long step;
    private final BuilderForLong builder;

    GroupNumberRowVisitorFactory(
        long start, long step, long size, ProblemAggregator problemAggregator) {
      this.current = start;
      this.step = step;
      this.builder = Builder.getForLong(IntegerType.INT_64, size, problemAggregator);
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      var nextGroupNumber = current;
      current = Math.addExact(current, step);
      return new GroupNumberRowVisitor(nextGroupNumber, builder);
    }

    @Override
    public ColumnStorage<?> seal() {
      return builder.seal();
    }

    private record GroupNumberRowVisitor(long groupNumber, BuilderForLong builder)
        implements GroupRowVisitor {
      @Override
      public void visit(long row) {
        builder.appendLong(groupNumber);
      }
    }
  }

  public static ColumnStorage<?> numberGroupsEqualCount(
      long numRows,
      int groupCount,
      long start,
      long step,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    var visitorFactory =
        new EqualCountRowVisitorFactory(start, step, numRows, groupCount, problemAggregator);
    return GroupingOrderingVisitor.visit(
        new Column[0], orderingColumns, directions, problemAggregator, visitorFactory, numRows);
  }

  private static class EqualCountRowVisitorFactory implements RowVisitorFactory {
    private final long start;
    private final long step;
    private final long groupSize;
    private final BuilderForLong builder;
    private final GroupRowVisitor visitor;

    EqualCountRowVisitorFactory(
        long start,
        long step,
        long totalCount,
        long numgroups,
        ProblemAggregator problemAggregator) {
      this.start = start;
      this.step = step;
      this.groupSize = (long) Math.ceil((double) totalCount / (double) numgroups);
      this.builder = Builder.getForLong(IntegerType.INT_64, totalCount, problemAggregator);
      this.visitor = new EqualCountRowVisitor(this);
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      return visitor;
    }

    @Override
    public ColumnStorage<?> seal() {
      return builder.seal();
    }

    private record EqualCountRowVisitor(EqualCountRowVisitorFactory parent)
        implements GroupRowVisitor {
      @Override
      public void visit(long row) {
        long group =
            Math.addExact(
                parent().start,
                Math.multiplyExact(
                    parent().step, (parent.builder.getCurrentSize() / parent().groupSize)));
        parent.builder.appendLong(group);
      }
    }
  }

  public static record EqualSumResult(ColumnStorage<?> storage, long actualGroupCount) {}

  public static EqualSumResult numberGroupsEqualSum(
      long numRows,
      Column sumColumn,
      int groupCount,
      long start,
      long step,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    var visitorFactory =
        new EqualSumRowVisitorFactory(
            start, step, numRows, sumColumn, groupCount, problemAggregator);
    var storage =
        GroupingOrderingVisitor.visit(
            new Column[0], orderingColumns, directions, problemAggregator, visitorFactory, numRows);
    long actualGroupCount = visitorFactory.getHighestGroupIndex() + 1;
    return new EqualSumResult(storage, actualGroupCount);
  }

  /** This 'factory' is also the sole visitor. */
  private static class EqualSumRowVisitorFactory implements RowVisitorFactory, GroupRowVisitor {
    private final long start;
    private final long step;
    private final double targetGroupSum;
    private final Column sumColumn;
    private final BuilderForLong builder;
    private final ColumnAggregatedProblemAggregator innerAggregator;

    private final double columnTotal;
    private double currentGroupSubtotal = 0.0;
    private long currentGroupIndex = 0;
    private long highestGroupIndex = 0;

    EqualSumRowVisitorFactory(
        long start,
        long step,
        long totalCount,
        Column sumColumn,
        long numgroups,
        ProblemAggregator problemAggregator) {
      this.start = start;
      this.step = step;
      this.sumColumn = sumColumn;
      this.builder = Builder.getForLong(IntegerType.INT_64, totalCount, problemAggregator);

      columnTotal = sum();
      targetGroupSum = columnTotal / numgroups;

      innerAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
    }

    @Override
    public GroupRowVisitor getNewRowVisitor() {
      return this;
    }

    @Override
    public ColumnStorage<?> seal() {
      return builder.seal();
    }

    @Override
    public void visit(long row) {
      Object valObj = sumColumn.getItem(row);
      if (valObj instanceof Number n) {
        double d = n.doubleValue();

        long groupIndex = currentGroupIndex;

        currentGroupSubtotal += d;
        if (currentGroupSubtotal >= targetGroupSum) {
          currentGroupSubtotal = 0.0;
          currentGroupIndex++;
        }

        long groupNumber = Math.addExact(start, Math.multiplyExact(step, groupIndex));
        builder.appendLong(groupNumber);

        highestGroupIndex = groupIndex;
      } else {
        innerAggregator.reportColumnAggregatedProblem(
            new IllegalArgumentError(
                "Equal_Sum", "Non-numeric value encountered in sum column", row));
      }
    }

    public long getHighestGroupIndex() {
      return highestGroupIndex;
    }

    // This ignores non-numeric values, which will be reported in visit().
    private double sum() {
      double total = 0.0;
      for (long i = 0; i < sumColumn.getSize(); i++) {
        Object val = sumColumn.getItem(i);
        if (val instanceof Number n) {
          total += n.doubleValue();
        }
      }
      return total;
    }
  }

  public static ColumnStorage<?> flaggedGroups(
      Table table,
      Column column,
      long start,
      long step,
      BiFunction<Object, Object, Boolean> predicate,
      boolean passPrevious,
      ProblemAggregator problemAggregator) {
    var builder = Builder.getForLong(IntegerType.INT_64, table.rowCount(), problemAggregator);
    if (table.rowCount() == 0) {
      return builder.seal();
    }

    try (var progressReporter =
        ProgressReporter.createWithStep(
            "find_group_number", table.rowCount(), StorageIterators.PROGRESS_STEP)) {
      var currentRow = new Row(table, 0);
      var newRow = new Row(table, 0);

      long currentGroup = start;
      builder.appendLong(currentGroup);
      progressReporter.advance();

      for (long i = 1; i < table.rowCount(); i++) {
        newRow.setRowIndex(i);
        if (passPrevious) {
          currentRow.setRowIndex(i - 1);
        }

        boolean predicateResult =
            column == null
                ? predicate.apply(currentRow, newRow)
                : predicate.apply(column.getItem(currentRow.index()), column.getItem(i));

        if (predicateResult) {
          if (!passPrevious) {
            currentRow.setRowIndex(i);
          }
          currentGroup = Math.addExact(currentGroup, step);
        }

        builder.appendLong(currentGroup);
        progressReporter.advance();
      }

      return builder.seal();
    }
  }
}
