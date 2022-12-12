package org.enso.table.data.table.join;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.join.scan.Matcher;
import org.enso.table.data.table.join.scan.MatcherFactory;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.graalvm.collections.Pair;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

public class IndexJoin implements JoinStrategy {
    private final Comparator<Object> objectComparator;
    private final BiFunction<Object, Object, Boolean> equalityFallback;

    public IndexJoin(Comparator<Object> objectComparator, BiFunction<Object, Object, Boolean> equalityFallback) {
        this.objectComparator = objectComparator;
        this.equalityFallback = equalityFallback;
    }

    @Override
    public JoinResult join(Table left, Table right, List<JoinCondition> conditions) {
        var equalConditions = conditions.stream()
                .filter(IndexJoin::isSupported)
                // TODO equals Ignore case support needed too
                .map(c -> c instanceof Equals e ? e : null)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        var remainingConditions = conditions.stream()
            .filter(c -> !isSupported(c)).collect(Collectors.toList());

        var leftEquals = equalConditions.stream().map(Equals::left).toArray(Column[]::new);
        var leftIndex = new MultiValueIndex(leftEquals, left.rowCount(), objectComparator);

        var rightEquals = equalConditions.stream().map(Equals::right).toArray(Column[]::new);
        var rightIndex = new MultiValueIndex(rightEquals, right.rowCount(), objectComparator);

        MatcherFactory factory = new MatcherFactory(objectComparator, equalityFallback);
        Matcher remainingMatcher = factory.create(remainingConditions);

        List<Pair<Integer, Integer>> matches = new ArrayList<>();
        for (var leftKey : leftIndex.keys()) {
            if (rightIndex.contains(leftKey)) {
                for (var leftRow : leftIndex.get(leftKey)) {
                    for (var rightRow : rightIndex.get(leftKey)) {
                        if (remainingMatcher.matches(leftRow, rightRow)) {
                            matches.add(Pair.create(leftRow, rightRow));
                        }
                    }
                }
            }
        }

        AggregatedProblems problems = AggregatedProblems.merge(new AggregatedProblems[]{
            leftIndex.getProblems(),
            rightIndex.getProblems(),
            remainingMatcher.getProblems()
        });
        return new JoinResult(matches, problems);
    }

    private static boolean isSupported(JoinCondition condition) {
        if (condition instanceof Equals eq) {
            // Currently hashing works only for builtin types.
            return isBuiltinType(eq.left().getStorage()) && isBuiltinType(eq.right().getStorage());
        } else return false;
    }

    private static boolean isBuiltinType(Storage<?> storage) {
        return storage.getType() != Storage.Type.OBJECT;
    }
}
