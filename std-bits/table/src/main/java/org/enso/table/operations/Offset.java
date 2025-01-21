package org.enso.table.operations;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;
import java.util.stream.IntStream;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public class Offset {
  public static Storage<?>[] offset(
      Column[] sourceColumns,
      int n,
      FillWith fillWith,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    if (n == 0 || sourceColumns.length == 0)
      return Arrays.stream(sourceColumns).map(c -> c.getStorage()).toArray(Storage<?>[]::new);
    var rowOrderMask =
        groupingColumns.length == 0 && orderingColumns.length == 0
            ? calculate_ungrouped_unordered_mask(sourceColumns[0].getSize(), n, fillWith)
            : calculate_grouped_ordered_mask(
                sourceColumns,
                n,
                fillWith,
                groupingColumns,
                orderingColumns,
                directions,
                problemAggregator);
    return Arrays.stream(sourceColumns)
        .map(c -> c.getStorage().applyMask(OrderMask.fromArray(rowOrderMask)))
        .toArray(Storage<?>[]::new);
  }

  public static Storage<?> offset_single_column(Column sourceColumn, int n, FillWith fillWith) {
    if (n == 0) return sourceColumn.getStorage();
    var rowOrderMask = calculate_ungrouped_unordered_mask(sourceColumn.getSize(), n, fillWith);
    return sourceColumn.getStorage().applyMask(OrderMask.fromArray(rowOrderMask));
  }

  private static int[] calculate_ungrouped_unordered_mask(int size, int n, FillWith fillWith) {
    return IntStream.range(0, size).map(i -> calculate_row_offset(i, n, fillWith, size)).toArray();
  }

  private static int calculate_row_offset(int i, int n, FillWith fillWith, int size) {
    int result = i + n;
    if (result < 0) {
      return switch (fillWith) {
        case NOTHING -> Storage.NOT_FOUND_INDEX;
        case CLOSEST_VALUE -> 0;
        case WRAP_AROUND -> (result % size) == 0 ? 0 : (result % size) + size;
      };
    } else if (result >= size) {
      return switch (fillWith) {
        case NOTHING -> Storage.NOT_FOUND_INDEX;
        case CLOSEST_VALUE -> size - 1;
        case WRAP_AROUND -> result % size;
      };
    }
    return result;
  }

  private static int[] calculate_grouped_ordered_mask(
      Column[] sourceColumns,
      int n,
      FillWith fillWith,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    var offsetRowVisitorFactory = new OffsetRowVisitorFactory(sourceColumns[0], n, fillWith);
    GroupingOrderingVisitor.visit(
        groupingColumns,
        orderingColumns,
        directions,
        problemAggregator,
        offsetRowVisitorFactory,
        sourceColumns[0].getSize());
    return offsetRowVisitorFactory.rowOrderMask;
  }

  private static class OffsetRowVisitorFactory implements RowVisitorFactory {

    int[] rowOrderMask;
    int n;
    FillWith fillWith;

    OffsetRowVisitorFactory(Column sourceColumn, int n, FillWith fillWith) {
      rowOrderMask = new int[sourceColumn.getSize()];
      this.n = n;
      this.fillWith = fillWith;
    }

    @Override
    public OffsetRowVisitor getNewRowVisitor() {
      return new OffsetRowVisitor(n, fillWith, rowOrderMask);
    }
  }

  private static class OffsetRowVisitor implements GroupRowVisitor {
    Queue<Integer> rolling_queue;
    Queue<Integer> fill_queue;
    int n;
    int current_n;
    int closestPos;
    FillWith fillWith;
    int[] rowOrderMask;

    public OffsetRowVisitor(int n, FillWith fillWith, int[] rowOrderMask) {
      this.rolling_queue = new LinkedList<>();
      this.fill_queue = new LinkedList<>();
      this.current_n = 0;
      this.closestPos = -1;
      this.n = n;
      this.fillWith = fillWith;
      this.rowOrderMask = rowOrderMask;
    }

    @Override
    public void visit(int i) {
      rolling_queue.add(i);

      if (n < 0 && current_n <= Math.abs(n)) {
        closestPos = rolling_queue.peek();
      } else if (n > 0) {
        closestPos = i;
      }

      if (current_n < Math.abs(n)) {
        fill_queue.add(i);
      } else if (n < 0) {
        rowOrderMask[i] = rolling_queue.poll();
      } else if (n > 0) {
        rowOrderMask[rolling_queue.poll()] = i;
      }

      current_n++;
    }

    @Override
    public void finalise() {
      while (fillWith == FillWith.WRAP_AROUND && current_n < Math.abs(n) && !fill_queue.isEmpty()) {
        var i = fill_queue.poll();
        fill_queue.add(i);
        current_n++;
      }

      while (n < 0 && !fill_queue.isEmpty()) {
        rowOrderMask[fill_queue.poll()] = getFillValue();
      }

      while (n > 0 && !rolling_queue.isEmpty()) {
        rowOrderMask[rolling_queue.poll()] = getFillValue();
      }
    }

    int getFillValue() {
      return switch (fillWith) {
        case NOTHING -> Storage.NOT_FOUND_INDEX;
        case CLOSEST_VALUE -> closestPos;
        case WRAP_AROUND -> n < 0 ? rolling_queue.poll() : fill_queue.poll();
      };
    }
  }
}
