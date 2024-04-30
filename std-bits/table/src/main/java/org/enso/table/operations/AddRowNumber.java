package org.enso.table.operations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;

public class AddRowNumber {

    public static Storage<?> create_numbering(
            long start,
            long step,
            Column[] groupingColumns,
            Column[] orderingColumns,
            int[] directions,
            ProblemAggregator problemAggregator) {
        if (orderingColumns.length != directions.length) {
            throw new IllegalArgumentException(
                    "The number of ordering columns and directions must be the same.");
        }
        if (groupingColumns.length == 0 && orderingColumns.length == 0) {
            throw new IllegalArgumentException("At least one grouping or ordering column is required.");
        }
        var sourceColumn = groupingColumns.length > 0 ? groupingColumns[0] : orderingColumns[0];
        var numberingStatistic = new NumberingStatistic(start, step, sourceColumn, problemAggregator);
        RunningLooper.loop(
                groupingColumns,
                orderingColumns,
                directions,
                problemAggregator,
                numberingStatistic,
                sourceColumn.getSize());
        return numberingStatistic.getResult();
    }

    private static class NumberingStatistic implements RunningStatistic<Long> {

        private final long start;
        private final long step;
        long[] numbers;

        NumberingStatistic(
                long start, long step, Column sourceColumn, ProblemAggregator problemAggregator) {
            this.start = start;
            this.step = step;
            int n = sourceColumn.getSize();
            numbers = new long[n];
        }

        @Override
        public RunningIterator<Long> getNewIterator() {
            return new RangeIterator(start, step);
        }

        @Override
        public void calculateNextValue(int i, RunningIterator<Long> it) {
            numbers[i] = it.next(0l);
        }

        @Override
        public Storage<Long> getResult() {
            return new LongStorage(numbers, IntegerType.INT_64);
        }

        private static class RangeIterator implements RunningIterator<Long> {

            private final long start;
            private final long step;
            private long current;
            private boolean isFirst = true;

            RangeIterator(long start, long step) {
                this.start = start;
                this.step = step;
            }

            @Override
            public Long next(Long value) throws ArithmeticException {
                if (isFirst) {
                    isFirst = false;
                    current = start;
                } else {
                    current = Math.addExact(current, step);
                }

                return current;
            }

            @Override
            public Long currentValue() {
                return current;
            }
        }
    }

    public static LongStorage create_grouped_numbering(
            long start, long step, Column[] groupingColumns, ProblemAggregator problemAggregator) {
        if (groupingColumns.length == 0) {
            throw new IllegalArgumentException("At least one grouping column is required.");
        }

        int n = groupingColumns[0].getSize();
        long[] numbers = new long[n];
        Storage<?>[] groupingStorages
                = Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
        ColumnAggregatedProblemAggregator groupingProblemAggregator
                = new ColumnAggregatedProblemAggregator(problemAggregator);
        List<TextFoldingStrategy> textFoldingStrategy
                = ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
        Map<UnorderedMultiValueKey, RangeIterator> groups = new HashMap<>();
        for (int i = 0; i < n; i++) {
            UnorderedMultiValueKey key
                    = new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
            key.checkAndReportFloatingEquality(
                    groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
            RangeIterator it = groups.computeIfAbsent(key, k -> new RangeIterator(start, step));
            numbers[i] = it.next();
        }
        return new LongStorage(numbers, IntegerType.INT_64);
    }

    /**
     * A helper for computing consecutive numbers based on a start and step. It
     * will throw an {@link
     * java.lang.ArithmeticException} if the next number overflows.
     */
    private static class RangeIterator {

        private final long start;
        private final long step;
        private long current;
        private boolean isFirst = true;

        RangeIterator(long start, long step) {
            this.start = start;
            this.step = step;
        }

        long next() throws ArithmeticException {
            if (isFirst) {
                isFirst = false;
                current = start;
            } else {
                current = Math.addExact(current, step);
            }

            return current;
        }
    }
}
