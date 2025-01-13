package org.enso.table.operations;

import java.util.LinkedList;
import java.util.Queue;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public class Offset {
    public static Storage<?> offset(
      Column sourceColumn,
      int n,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
        var offsetRunningStatistic = new OffsetRunningStatistic(sourceColumn, n, problemAggregator);
        RunningLooper.loop(
            groupingColumns,
            orderingColumns,
            directions,
            problemAggregator,
            offsetRunningStatistic,
            sourceColumn.getSize());
        return offsetRunningStatistic.getResultColumn();
      }

    private static class OffsetRunningStatistic implements RunningStatistic<OffsetIterator> {

        int[] result;
        Column sourceColumn;
        int n;

        OffsetRunningStatistic(Column sourceColumn, int n, ProblemAggregator problemAggregator) {
            result = new int[sourceColumn.getSize()];
            this.sourceColumn = sourceColumn;
            this.n = n;
        }

        @Override
        public void calculateNextValue(int i, OffsetIterator it) {
            if (n<0) {
                Integer dNextValue = it.next(i);
                if (dNextValue == null) {
                    result[i] = -1;
                } else {
                    result[i] = dNextValue;
                }
            } else {
                Integer dNextPosition = it.next(i);
                if (dNextPosition != null) {
                    result[dNextPosition] = i;
                }
            }
        }

        @Override
        public void finalise(OffsetIterator it) {
            if (n>0) {
            while (!it.queue.isEmpty()) {
                result[it.queue.poll()] = -1; 
                }
            }
        }

        Storage<?> getResultColumn() {
            return sourceColumn.getStorage().applyMask(OrderMask.fromArray(result));
        }

        @Override
        public OffsetIterator getNewIterator() {
            return new OffsetIterator(n);
        }
    }


  private static class OffsetIterator {
        Queue<Integer> queue;
        int n;
        int current_n;

        public OffsetIterator(int n)
        {
            this.queue = new LinkedList<>();
            this.n = n;
            this.current_n = 0;
        }

        public Integer next(int value) {
            queue.add(value);
            current_n++;
            if (current_n > Math.abs(n)) {
                return queue.poll();
            } else {
                return null;
            }
        }
  }
}