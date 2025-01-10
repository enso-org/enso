package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

import java.util.BitSet;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;

public class Offset {
    public static Storage<?> offset(
      Column sourceColumn,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
        var offsetRunningStatistic = new OffsetRunningStatistic<Long>(sourceColumn, problemAggregator);
        RunningLooper.loop(
            groupingColumns,
            orderingColumns,
            directions,
            problemAggregator,
            offsetRunningStatistic,
            sourceColumn.getSize());
        return offsetRunningStatistic.getResult();
      }

    private static class OffsetRunningStatistic<T> implements RunningStatistic<Long> {

        long[] result;
        BitSet isNothing;
        ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;
        Column sourceColumn;

        OffsetRunningStatistic(Column sourceColumn, ProblemAggregator problemAggregator) {
            result = new long[sourceColumn.getSize()];
            isNothing = new BitSet();
            columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
            this.sourceColumn = sourceColumn;
        }

        @Override
        public void calculateNextValue(int i, RunningIterator<Long> it) {
            Object value = sourceColumn.getStorage().getItemBoxed(i);
            Long dValue = NumericConverter.tryConvertingToLong(value);
            Long dNextValue = it.next(dValue);
            if (dNextValue == null) {
                isNothing.set(i);
            } else {
                result[i] = dNextValue;
            }
        }

        @Override
        public Storage<Long> getResult() {
            return new LongStorage(result, sourceColumn.getSize(), isNothing, IntegerType.INT_64);
        }

        @Override
        public RunningIterator<Long> getNewIterator() {
            return new OffsetRunning();
        }
    }


  private static class OffsetRunning implements RunningIterator<Long> {
        Long prev;
        @Override
        public Long next(Long value) {
            var ret = prev;
            prev = value;
            return ret;
        }

        @Override
        public Long currentValue() {
            return prev;
        }

  }
}