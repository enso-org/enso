package org.enso.table.operations;

import java.util.LinkedList;
import java.util.Queue;
import java.util.Arrays;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public class Offset {
    public static Storage<?>[] offset(
      Column[] sourceColumns,
      int n,
      OffFill offFill,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
        if (n==0) return Arrays.stream(sourceColumns).map(c -> c.getStorage()).toArray(Storage<?>[]::new);
        var offsetRunningStatistic = new OffsetRowVisitorFactory(sourceColumns[0], n, offFill);
        GroupingOrderingVisitor.visit(
            groupingColumns,
            orderingColumns,
            directions,
            problemAggregator,
            offsetRunningStatistic,
            sourceColumns[0].getSize());
        return Arrays.stream(sourceColumns).map(c -> c.getStorage().applyMask(OrderMask.fromArray(offsetRunningStatistic.result))).toArray(Storage<?>[]::new);
      }

    private static class OffsetRowVisitorFactory implements RowVisitorFactory {

        int[] result;
        int n;
        OffFill offFill;

        OffsetRowVisitorFactory(Column sourceColumn, int n, OffFill offFill) {
            result = new int[sourceColumn.getSize()];
            this.n = n;
            this.offFill = offFill;
        }

        @Override
        public OffsetRowVisitor getNewRowVisitor() {
            return new OffsetRowVisitor(n, offFill, result);
        }
    }


  private static class OffsetRowVisitor implements RowVisitor {
        Queue<Integer> rolling_queue;
        Queue<Integer> fill_queue;
        int n;
        int current_n;
        int closestPos;
        OffFill offFill;
        int[] result;

        public OffsetRowVisitor(int n, OffFill offFill, int[] result)
        {
            this.rolling_queue = new LinkedList<>();
            this.fill_queue = new LinkedList<>();
            this.current_n = 0;
            this.closestPos = -1;
            this.n = n;
            this.offFill = offFill;
            this.result = result;
        }

        @Override
        public void visit(int i) {
            rolling_queue.add(i);
            
            if (n<0 && current_n <= Math.abs(n)) {
                closestPos = rolling_queue.peek();
            } else if (n>0) {
                closestPos = i;
            }
            
            if (current_n < Math.abs(n)) {
                fill_queue.add(i);
            } else if (n<0) {
                result[i] = rolling_queue.poll();
            } else if (n>0) {
                result[rolling_queue.poll()] = i;
            }

            current_n++;
        }

        @Override
        public void finalise() {
            while (offFill == OffFill.WRAP_AROUND && current_n < Math.abs(n) && !fill_queue.isEmpty()) {
                var i = fill_queue.poll();
                fill_queue.add(i);
                current_n++;
            }

            while (n<0 && !fill_queue.isEmpty()) {
                result[fill_queue.poll()] = getFillValue();
            }

            while (n>0 && !rolling_queue.isEmpty()) {
                result[rolling_queue.poll()] = getFillValue();
            }
        }
    
        int getFillValue()
        {
            return switch (offFill) {
                case NOTHING -> -1;
                case CLOSEST_VALUE -> closestPos;
                case WRAP_AROUND -> n<0 ? rolling_queue.poll() : fill_queue.poll();
                case CONSTANT -> -1;
            };
        }

  }
}