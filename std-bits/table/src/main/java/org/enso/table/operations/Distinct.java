package org.enso.table.operations;

import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueKeyBase;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;
import org.graalvm.polyglot.Context;

public class Distinct {

    /**
     * Creates a row mask containing only the first row from sets of rows
     * grouped by key columns.
     */
    public static BitSet buildDistinctRowsMask(
            int tableSize,
            Column[] keyColumns,
            TextFoldingStrategy textFoldingStrategy,
            ProblemAggregator problemAggregator) {
        ColumnAggregatedProblemAggregator groupingProblemAggregator
                = new ColumnAggregatedProblemAggregator(problemAggregator);
        Context context = Context.getCurrent();
        var mask = new BitSet();
        if (keyColumns.length != 0) {
            HashSet<MultiValueKeyBase> visitedRows = new HashSet<>();
            int size = keyColumns[0].getSize();
            Storage<?>[] storage
                    = Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
            List<TextFoldingStrategy> strategies = ConstantList.make(textFoldingStrategy, storage.length);
            for (int i = 0; i < size; i++) {
                UnorderedMultiValueKey key = new UnorderedMultiValueKey(storage, i, strategies);
                key.checkAndReportFloatingEquality(
                        groupingProblemAggregator, columnIx -> keyColumns[columnIx].getName());

                if (!visitedRows.contains(key)) {
                    mask.set(i);
                    visitedRows.add(key);
                }

                context.safepoint();
            }
        } else {
            // If there are no columns to distinct-by we just return the whole table.
            mask.set(0, tableSize);
        }

        return mask;
    }

    public static BitSet buildDuplicatesRowsMask(
            int tableSize,
            Column[] keyColumns,
            TextFoldingStrategy textFoldingStrategy,
            ProblemAggregator problemAggregator) {
        ColumnAggregatedProblemAggregator groupingProblemAggregator
                = new ColumnAggregatedProblemAggregator(problemAggregator);
        Context context = Context.getCurrent();
        var mask = new BitSet();
        if (keyColumns.length != 0) {
            Map<MultiValueKeyBase, Integer> visitedRows = new HashMap<>();
            int size = keyColumns[0].getSize();
            Storage<?>[] storage
                    = Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
            List<TextFoldingStrategy> strategies = ConstantList.make(textFoldingStrategy, storage.length);
            for (int i = 0; i < size; i++) {
                UnorderedMultiValueKey key = new UnorderedMultiValueKey(storage, i, strategies);
                key.checkAndReportFloatingEquality(
                        groupingProblemAggregator, columnIx -> keyColumns[columnIx].getName());

                var keyIndex = visitedRows.get(key);
                if (keyIndex == null) {
                    visitedRows.put(key, i);
                } else {
                    mask.set(i);
                    mask.set(keyIndex);
                }

                context.safepoint();
            }
        } else {
            // If there are no columns to distinct-by we just return the whole table.
            mask.set(0, tableSize);
        }

        return mask;
    }
}
