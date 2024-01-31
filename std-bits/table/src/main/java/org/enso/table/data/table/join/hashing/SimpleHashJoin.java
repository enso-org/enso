package org.enso.table.data.table.join.hashing;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.join.JoinKind;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.conditions.HashableCondition;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/**
 * A strategy that uses a hash-map to perform join on the equality conditions.
 *
 * <p>
 * It then delegates to {@code remainingMatcher} to perform the remaining
 * conditions on the matching pairs of row subsets.
 */
public class SimpleHashJoin implements JoinStrategy {

    public SimpleHashJoin(List<HashableCondition> conditions, JoinKind joinKind) {
        this.hashJoinConfig = new HashJoinConfig(conditions);
        this.joinKind = joinKind;
    }

    private final HashJoinConfig hashJoinConfig;
    private final JoinKind joinKind;

    @Override
    public JoinResult join(ProblemAggregator problemAggregator) {
        Context context = Context.getCurrent();

        var rightEquals = hashJoinConfig.getRightEquals();
        var leftEquals = hashJoinConfig.getLeftEquals();
        var textFoldingStrategies = hashJoinConfig.getTextFoldingStrategies();

        var rightIndex
                = MultiValueIndex.makeUnorderedIndex(
                        rightEquals, rightEquals[0].getSize(), textFoldingStrategies, problemAggregator);

        JoinResult.Builder resultBuilder = new JoinResult.Builder();
        final Storage<?>[] storage
                = Arrays.stream(leftEquals).map(Column::getStorage).toArray(Storage[]::new);

        Set<UnorderedMultiValueKey> matchedRightKeys = new HashSet<>();
        ColumnAggregatedProblemAggregator groupingProblemAggregator
                = new ColumnAggregatedProblemAggregator(problemAggregator);
        for (int leftRow = 0; leftRow < leftEquals[0].getSize(); leftRow++) {
            var leftKey = new UnorderedMultiValueKey(storage, leftRow, textFoldingStrategies);
            leftKey.checkAndReportFloatingEquality(
                    groupingProblemAggregator, columnIx -> leftEquals[columnIx].getName());

            // If any field of the key is null, it cannot match anything.
            List<Integer> rightRows = leftKey.hasAnyNulls() ? null : rightIndex.get(leftKey);
            if (rightRows != null) {
                if (joinKind.wantsCommon) {
                    List<Integer> leftRows = List.of(leftRow);
                    matchAll(leftRows, rightRows, resultBuilder);
                }
                if (joinKind.wantsRightUnmatched) {
                    matchedRightKeys.add(leftKey);
                }
            } else {
                if (joinKind.wantsLeftUnmatched) {
                    resultBuilder.addUnmatchedLeftRow(leftRow);
                    context.safepoint();
                }
            }
            context.safepoint();
        }

        if (joinKind.wantsRightUnmatched) {
            for (var rightEntry : rightIndex.mapping().entrySet()) {
                UnorderedMultiValueKey rightKey = rightEntry.getKey();
                boolean wasCompletelyUnmatched = !matchedRightKeys.contains(rightKey);
                if (wasCompletelyUnmatched) {
                    for (int rightRow : rightEntry.getValue()) {
                        resultBuilder.addUnmatchedRightRow(rightRow);
                        context.safepoint();
                    }
                }
            }
        }

        return resultBuilder.buildAndInvalidate();
    }

    private static void matchAll(
            List<Integer> leftGroup,
            List<Integer> rightGroup,
            JoinResult.Builder resultBuilder) {
        Context context = Context.getCurrent();
        for (var leftRow : leftGroup) {
            for (var rightRow : rightGroup) {
                resultBuilder.addMatchedRowsPair(leftRow, rightRow);
                context.safepoint();
            }

            context.safepoint();
        }
    }
}
