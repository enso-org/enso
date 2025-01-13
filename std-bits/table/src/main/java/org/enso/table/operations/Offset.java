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
            this.n = n;
            this.offFill = offFill;
            this.closestPos = -1;
        }

        @Override
        public void calculateNextValue(int i, OffsetIterator it) {
            if (n<0) {
                it.rolling_queue.add(i);
                if (it.current_n < Math.abs(n)) {
                    it.fill_queue.add(i);
                }
                if (it.current_n <= Math.abs(n)) {
                    closestPos = it.rolling_queue.peek();
                } 
                if (it.current_n >= Math.abs(n)) {
                    result[i] = it.rolling_queue.poll();
                }
            } else {
                it.rolling_queue.add(i);
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

            if (n<0) {
                while (!it.fill_queue.isEmpty()) {
                    result[it.fill_queue.poll()] = fillValue;
                    }
            } else {
                while (!it.rolling_queue.isEmpty()) {
                    result[it.rolling_queue.poll()] = fillValue;
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
        int n;
        int current_n;
        int fill_pos;

        public OffsetIterator(int n)
        {
            this.rolling_queue = new LinkedList<>();
            this.fill_queue = new LinkedList<>();
            this.n = n;
            this.current_n = 0;
            this.fill_pos = -1;
        }

        public Integer next(int value) {
            rolling_queue.add(value);
            fill_pos = value;
            current_n++;
            if (current_n > Math.abs(n)) {
                return rolling_queue.poll();
            } else {
                fill_queue.add(value);
                return null;
            }
        }
  }
}