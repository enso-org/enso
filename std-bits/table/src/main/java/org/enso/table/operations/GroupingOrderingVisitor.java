package org.enso.table.operations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.LongStream;
import org.enso.base.ProgressReporter;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.masks.IndexMapper;
import org.enso.table.data.column.operation.masks.MaskOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;

/**
 * Abstract class GroupingOrderingVisitor
 *
 * <p>Overview: This class provides a mechanism for visiting rows of data based on grouping and
 * ordering criteria.
 *
 * <p>Usage : GroupingOrderingVisitor.visit( groupingColumns, orderingColumns, directions,
 * problemAggregator, rowVisitorFactory, sourceColumn.getSize());
 */
abstract class GroupingOrderingVisitor {
  /**
   * For each group: will call getNewRowVisitor() Then for each row in that group will call
   * visit(rowNumber) on the visitor for that group in the order specified by the OrderingColumns
   * Then calls finalise() to indicate that group is complete and there will be no more calls to
   * visit. Can be used without any groupingColumns in which case the whole dataset is treated as a
   * single group. Can be used without orderingColumns in which case the orginal record order is
   * used.
   *
   * @param groupingColumns Columns used to group data.
   * @param orderingColumns Columns used to order data within groups.
   * @param directions Array specifying the sort direction for each orderingColumn.
   * @param problemAggregator Collects problems with grouping/ordering.
   * @param visitorFactory The factory which we call getNewRowVisitor on.
   * @param numRows Number of rows in the datset. Must be the same as any columns used for grouping
   *     and ordering.
   * @return A ColumnStorage containing the results of the visit operation.
   * @throws IllegalArgumentException if the length of orderingColumns and directions do not match.
   */
  public static ColumnStorage<?> visit(
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator,
      RowVisitorFactory visitorFactory,
      long numRows) {
    if (orderingColumns.length != directions.length) {
      throw new IllegalArgumentException(
          "The number of ordering columns and directions must be the same.");
    }

    GroupingOrderingVisitor visitMethod;
    if (groupingColumns.length > 0 && orderingColumns.length > 0) {
      visitMethod =
          new GroupingOrderingRunning(
              groupingColumns, orderingColumns, directions, problemAggregator);
    } else if (groupingColumns.length > 0) {
      visitMethod = new GroupingNoOrderingRunning(groupingColumns, problemAggregator);
    } else if (orderingColumns.length > 0) {
      visitMethod = new NoGroupingOrderingRunning(orderingColumns, directions);
    } else {
      visitMethod = new NoGroupingNoOrderingRunning();
    }
    visitMethod.visitImpl(visitorFactory, numRows);

    var rawResult = visitorFactory.seal();
    var mask = visitMethod.getMask();
    return mask == null || rawResult == null
        ? rawResult
        : MaskOperation.getSlicedStorage(rawResult, new IndexMapper.ArrayMapping(mask));
  }

  // interface for the different implementations
  public abstract void visitImpl(RowVisitorFactory runningStatistic, long numRows);

  public long[] getMask() {
    return null; // Default implementation returns null, can be overridden if needed
  }
}

class NoGroupingNoOrderingRunning extends GroupingOrderingVisitor {
  @Override
  public void visitImpl(RowVisitorFactory runningStatistic, long numRows) {
    var it = runningStatistic.getNewRowVisitor();
    try (var progressReporter =
        ProgressReporter.createWithStep("running", numRows, StorageIterators.PROGRESS_STEP)) {
      for (long i = 0; i < numRows; i++) {
        it.visit(i);
        progressReporter.advance();
      }
      it.finalise();
    }
  }
}

class GroupingNoOrderingRunning extends GroupingOrderingVisitor {
  private final Column[] groupingColumns;
  private final ColumnStorage<?>[] groupingStorages;
  private final ColumnAggregatedProblemAggregator groupingProblemAggregator;
  private final List<TextFoldingStrategy> textFoldingStrategy;
  private final Map<UnorderedMultiValueKey, GroupRowVisitor> groups;

  public GroupingNoOrderingRunning(Column[] groupingColumns, ProblemAggregator problemAggregator) {
    this.groupingColumns = groupingColumns;
    groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
    groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
    textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    groups = new HashMap<>();
  }

  @Override
  public void visitImpl(RowVisitorFactory runningStatistic, long numRows) {
    for (long i = 0; i < numRows; i++) {
      var key = new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      key.checkAndReportFloatingEquality(
          groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
      var it = groups.computeIfAbsent(key, k -> runningStatistic.getNewRowVisitor());
      it.visit(i);
    }
    groups.forEach((key, it) -> it.finalise());
  }
}

class NoGroupingOrderingRunning extends GroupingOrderingVisitor {
  private final ColumnStorage<?>[] orderingStorages;
  private final List<OrderedMultiValueKey> keys;
  private final long[] mask;

  public NoGroupingOrderingRunning(Column[] orderingColumns, int[] directions) {
    long n = orderingColumns[0].getSize();
    orderingStorages =
        Arrays.stream(orderingColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
    keys =
        new ArrayList<>(
            LongStream.range(0, n)
                .mapToObj(i -> new OrderedMultiValueKey(orderingStorages, i, directions))
                .toList());
    keys.sort(null);
    mask = new long[Builder.checkSize(n)];
  }

  @Override
  public void visitImpl(RowVisitorFactory runningStatistic, long numRows) {
    long idx = 0;
    var it = runningStatistic.getNewRowVisitor();
    for (var key : keys) {
      var i = key.getRowIndex();
      it.visit(i);
      mask[Math.toIntExact(i)] = idx++; // Store the original index in the mask
    }
    it.finalise();
  }

  @Override
  public long[] getMask() {
    return mask; // Return the mask containing the original indices
  }
}

class GroupingOrderingRunning extends GroupingOrderingVisitor {

  private final Column[] groupingColumns;
  private final int[] directions;
  private final ColumnStorage<?>[] orderingStorages;
  private final ProblemAggregator problemAggregator;
  private final long[] mask;

  public GroupingOrderingRunning(
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    this.groupingColumns = groupingColumns;
    this.directions = directions;

    var groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
    ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);

    orderingStorages =
        Arrays.stream(orderingColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
    this.problemAggregator = problemAggregator;

    mask = new long[Builder.checkSize(orderingColumns[0].getSize())];
  }

  @Override
  public void visitImpl(RowVisitorFactory runningStatistic, long numRows) {
    int idx = 0;
    var groupIndex =
        MultiValueIndex.makeUnorderedIndex(
            groupingColumns, numRows, TextFoldingStrategy.unicodeNormalizedFold, problemAggregator);
    for (var entry : groupIndex.mapping().entrySet()) {
      List<Long> indices = entry.getValue();
      List<OrderedMultiValueKey> orderingKeys =
          new ArrayList<>(
              indices.stream()
                  .map(i -> new OrderedMultiValueKey(orderingStorages, i, directions))
                  .toList());
      orderingKeys.sort(null);
      var it = runningStatistic.getNewRowVisitor();
      for (OrderedMultiValueKey key : orderingKeys) {
        var i = key.getRowIndex();
        it.visit(i);
        mask[Math.toIntExact(i)] = idx++;
      }
      it.finalise();
    }
  }

  @Override
  public long[] getMask() {
    return mask;
  }
}
