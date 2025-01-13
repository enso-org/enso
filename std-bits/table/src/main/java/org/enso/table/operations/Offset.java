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
      OffFill offFill,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
        var offsetRunningStatistic = new OffsetRunningStatistic(sourceColumn, n, offFill);
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
        OffFill offFill;
        int closestPos;

        OffsetRunningStatistic(Column sourceColumn, int n, OffFill offFill) {
            result = new int[sourceColumn.getSize()];
            this.sourceColumn = sourceColumn;
            this.n = offFill==OffFill.WRAP_AROUND && sourceColumn.getSize() != 0 ? n % sourceColumn.getSize() : n;
            this.offFill = offFill;
            this.closestPos = -1;
        }

        @Override
        public void calculateNextValue(int i, OffsetIterator it) {
            it.rolling_queue.add(i);
            if (it.current_n < Math.abs(n)) {
                it.fill_queue.add(i);
            }
            if (n<0) {
                if (it.current_n <= Math.abs(n)) {
                    closestPos = it.rolling_queue.peek();
                } 
                if (it.current_n >= Math.abs(n)) {
                    result[i] = it.rolling_queue.poll();
                }
            } else {
                closestPos = i;
                if (it.current_n >= Math.abs(n)) {
                    result[it.rolling_queue.poll()] = i;
                }
            }
            it.current_n++;
        }

        @Override
        public void finalise(OffsetIterator it) {
            int fillValue = switch (offFill) {
                case NOTHING -> -1;
                case CLOSEST_VALUE -> closestPos;
                case WRAP_AROUND -> -1;
                case CONSTANT -> -1;
            };

            if (offFill != OffFill.WRAP_AROUND) {
                if (n<0) {
                    while (!it.fill_queue.isEmpty()) {
                        result[it.fill_queue.poll()] = fillValue;
                        }
                } else {
                    while (!it.rolling_queue.isEmpty()) {
                        result[it.rolling_queue.poll()] = fillValue;
                        }
                }
            } else {
                if (n<0) {
                    while (!it.fill_queue.isEmpty()) {
                        result[it.fill_queue.poll()] = it.rolling_queue.poll();
                        }
                } else {
                    while (!it.rolling_queue.isEmpty()) {
                        result[it.rolling_queue.poll()]  = it.fill_queue.poll();
                        }
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
        Queue<Integer> rolling_queue;
        Queue<Integer> fill_queue;
        int current_n;

        public OffsetIterator(int n)
        {
            this.rolling_queue = new LinkedList<>();
            this.fill_queue = new LinkedList<>();
            this.current_n = 0;
        }
  }
}