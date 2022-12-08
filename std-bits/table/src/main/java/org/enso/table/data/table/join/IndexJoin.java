package org.enso.table.data.table.join;

import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.graalvm.collections.Pair;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class IndexJoin implements JoinStrategy {
    private final Comparator<Object> comparator;

    public IndexJoin(Comparator<Object> comparator) {
        this.comparator = comparator;
    }

    @Override
    public JoinResult join(Table left, Table right, List<JoinCondition> conditions) {
        var equalConditions = conditions.stream()
                .map(c -> c instanceof Equals e ? e : null)
                .filter(c -> c != null)
                .collect(Collectors.toList());
        if (equalConditions.size() != conditions.size()) {
            return new ScanJoin().join(left, right, conditions);
        }

        try {
            var leftEquals = equalConditions.stream().map(Equals::left).toArray(Column[]::new);
            var leftIndex = new MultiValueIndex(leftEquals, left.rowCount(), comparator);

            var rightEquals = equalConditions.stream().map(Equals::right).toArray(Column[]::new);
            var rightIndex = new MultiValueIndex(rightEquals, right.rowCount(), comparator);

            List<Pair<Integer, Integer>> matches = new ArrayList<>();
            for (var leftKey : leftIndex.keys()) {
                if (rightIndex.contains(leftKey)) {
                    for (var leftRow : leftIndex.get(leftKey)) {
                        for (var rightRow : rightIndex.get(leftKey)) {
                            matches.add(Pair.create(leftRow, rightRow));
                        }
                    }
                }
            }
            return new JoinResult(matches);
        } catch (IllegalStateException e) {
            // Fallback for custom objects
            return new ScanJoin().join(left, right, conditions);
        }
    }
}
