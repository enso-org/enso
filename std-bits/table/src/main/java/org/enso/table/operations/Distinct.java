package org.enso.table.operations;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import org.enso.base.ProgressReporter;
import org.enso.base.arrays.LongArrayList;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.index.MultiValueKeyBase;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;

public class Distinct {
  /** Creates a row mask containing only the first row from sets of rows grouped by key columns. */
  public static long[] buildDistinctRowsMask(
      long tableSize,
      Column[] keyColumns,
      TextFoldingStrategy textFoldingStrategy,
      ProblemAggregator problemAggregator) {
    try (var progressReporter =
        ProgressReporter.createWithStep(
            "buildDistinctRowsMask", tableSize, StorageIterators.PROGRESS_STEP)) {
      var groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);

      ColumnStorage<?>[] storage =
          Arrays.stream(keyColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
      List<TextFoldingStrategy> strategies =
          ConstantList.make(textFoldingStrategy, keyColumns.length);

      var distinctRows = new LongArrayList((int) Math.min(tableSize, 100000));
      HashSet<MultiValueKeyBase> visitedRows = new HashSet<>();

      for (long i = 0; i < tableSize; i++) {
        var key = new UnorderedMultiValueKey(storage, (int) i, strategies);
        key.checkAndReportFloatingEquality(
            groupingProblemAggregator, columnIx -> keyColumns[columnIx].getName());

        if (!visitedRows.contains(key)) {
          distinctRows.add(i);
          visitedRows.add(key);
        }

        progressReporter.advance();
      }

      return distinctRows.toArray();
    }
  }

  public static long[] buildDuplicatesRowsMask(
      long tableSize,
      Column[] keyColumns,
      TextFoldingStrategy textFoldingStrategy,
      ProblemAggregator problemAggregator) {
    try (var progressReporter =
        ProgressReporter.createWithStep(
            "buildDuplicatesRowsMask", tableSize, StorageIterators.PROGRESS_STEP)) {
      var groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);

      ColumnStorage<?>[] storage =
          Arrays.stream(keyColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
      List<TextFoldingStrategy> strategies =
          ConstantList.make(textFoldingStrategy, keyColumns.length);

      HashSet<Long> duplicateRows = new HashSet<>();
      Map<MultiValueKeyBase, Long> visitedRows = new HashMap<>();

      for (long i = 0; i < tableSize; i++) {
        var key = new UnorderedMultiValueKey(storage, (int) i, strategies);
        key.checkAndReportFloatingEquality(
            groupingProblemAggregator, columnIx -> keyColumns[columnIx].getName());

        var keyIndex = visitedRows.get(key);
        if (keyIndex == null) {
          visitedRows.put(key, i);
        } else {
          // Mark both the current row and the first occurrence of this key as duplicates.
          duplicateRows.add(keyIndex);
          duplicateRows.add(i);
        }

        progressReporter.advance();
      }

      // Sort the duplicate rows to ensure they are in ascending order.
      long[] result = duplicateRows.stream().mapToLong(Long::longValue).toArray();
      Arrays.sort(result);
      return result;
    }
  }
}
