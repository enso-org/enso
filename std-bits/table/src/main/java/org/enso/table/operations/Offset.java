package org.enso.table.operations;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;
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
    var offsetRowVisitorFactory = new OffsetRowVisitorFactory(sourceColumns[0], n, fillWith);
    GroupingOrderingVisitor.visit(
        groupingColumns,
        orderingColumns,
        directions,
        problemAggregator,
        offsetRowVisitorFactory,
        sourceColumns[0].getSize());
    return Arrays.stream(sourceColumns)
        .map(
            c ->
                c.getStorage().applyMask(OrderMask.fromArray(offsetRowVisitorFactory.rowOrderMask)))
        .toArray(Storage<?>[]::new);
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
