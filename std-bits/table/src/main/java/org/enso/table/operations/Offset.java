package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

import java.util.BitSet;

import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;

public class Offset {
    public static Storage<?> offset(
      Column sourceColumn,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
        var offsetRunningStatistic = new OffsetRunningStatistic<Double>(sourceColumn, problemAggregator);
        RunningLooper.loop(
            groupingColumns,
            orderingColumns,
            directions,
            problemAggregator,
            offsetRunningStatistic,
            sourceColumn.getSize());
        return offsetRunningStatistic.getResult();
      }

    private static class OffsetRunningStatistic<T> implements RunningStatistic<Double> {

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
        public void calculateNextValue(int i, RunningIterator<Double> it) {
        
        }

        @Override
        public Storage<Double> getResult() {
            return new DoubleStorage(result, sourceColumn.getSize(), isNothing);
        }

        @Override
        public RunningIterator<Double> getNewIterator() {
            return new OffsetRunning();
        }
    }


  private static class OffsetRunning implements RunningIterator<Double> {

        @Override
        public Double next(Double value) {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        @Override
        public Double currentValue() {
            throw new UnsupportedOperationException("Not supported yet.");
        }

  }
}