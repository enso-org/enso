package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

import java.util.BitSet;
import java.util.LinkedList;
import java.util.Queue;

import org.apache.commons.math3.analysis.function.Abs;
import org.apache.poi.xssf.model.ThemesTable;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;

public class Offset {
    public static Storage<?> offset(
      Column sourceColumn,
      int n,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
        var offsetRunningStatistic = new OffsetRunningStatistic<Long>(sourceColumn, n, problemAggregator);
        RunningLooper.loop(
            groupingColumns,
            orderingColumns,
            directions,
            problemAggregator,
            offsetRunningStatistic,
            sourceColumn.getSize());
        return offsetRunningStatistic.getResult();
      }

    private static class OffsetRunningStatistic<T> implements RunningStatistic<Long, OffsetIterator> {

        long[] result;
        BitSet isNothing;
        ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;
        Column sourceColumn;
        int n;

        OffsetRunningStatistic(Column sourceColumn, int n, ProblemAggregator problemAggregator) {
            result = new long[sourceColumn.getSize()];
            isNothing = new BitSet();
            columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
            this.sourceColumn = sourceColumn;
            this.n = n;
        }

        @Override
        public void calculateNextValue(int i, OffsetIterator it) {
            Object value = sourceColumn.getStorage().getItemBoxed(i);
            Long dValue = NumericConverter.tryConvertingToLong(value);
            if (n<0) {
                Long dNextValue = it.next(dValue);
                if (dNextValue == null) {
                    isNothing.set(i);
                } else {
                    result[i] = dNextValue;
                }
            } else {
                Long dNextPosition = it.next(Long.valueOf(i));
                if (dNextPosition != null) {
                    result[dNextPosition.intValue()] = dValue;
                }
            }
        }

        @Override
        public void finalise(OffsetIterator it) {
            if (n>0) {
            while (!it.queue.isEmpty()) {
                isNothing.set(it.queue.poll().intValue()); 
                }
            }
        }

        @Override
        public Storage<Long> getResult() {
            return new LongStorage(result, sourceColumn.getSize(), isNothing, IntegerType.INT_64);
        }

        @Override
        public OffsetIterator getNewIterator() {
            return new OffsetIterator(n);
        }
    }


  private static class OffsetIterator {
        Queue<Long> queue;
        int n;
        int current_n;

        public OffsetIterator(int n)
        {
            this.queue = new LinkedList<>();
            this.n = n;
            this.current_n = 0;
        }

        public Long next(Long value) {
            queue.add(value);
            current_n++;
            if (current_n > Math.abs(n)) {
                return queue.poll();
            } else {
                return null;
            }
        }

        public Long currentValue() {
            // if nn
            return queue.peek();
        }

  }
}