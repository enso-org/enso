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

/** A strategy that uses a hash-map to perform join on the equality conditions. */
public class SimpleHashJoin implements JoinStrategy {

  public SimpleHashJoin(List<HashableCondition> conditions, JoinKind joinKind) {
    var tempHashJoinConfig = new HashJoinConfig(conditions);

    // algorithm assumes that left table is the big table.
    // If not we will flip the left and right tables over to do the join
    if (tempHashJoinConfig.getLeftNumRows() >= tempHashJoinConfig.getRightNumRows()) {
      this.hashJoinConfig = tempHashJoinConfig;
      this.joinKind = joinKind;
      this.resultBuilder = new SimpleHashJoinResultBuilder(false);
    } else {
      // flip left and right inside of HashJoinConfig
      this.hashJoinConfig =
          new HashJoinConfig(
              tempHashJoinConfig.getRightEquals(),
              tempHashJoinConfig.getLeftEquals(),
              tempHashJoinConfig.getTextFoldingStrategies());
      this.joinKind = flipJoinKind(joinKind);
      this.resultBuilder = new SimpleHashJoinResultBuilder(true);
    }
  }

  private final HashJoinConfig hashJoinConfig;
  private final JoinKind joinKind;
  private final SimpleHashJoinResultBuilder resultBuilder;

  @Override
  public JoinResult join(ProblemAggregator problemAggregator) {
    // algorithm assumes that left table is the big table.
    // If not we have flipped the tables round to do the join.
    // If you are debugging your left table might not be your left table here.
    // The result builder flips the indexes back as you add them
    assert (hashJoinConfig.getLeftNumRows() >= hashJoinConfig.getRightNumRows());

    var groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
    var rightIndex =
        MultiValueIndex.makeUnorderedIndex(
            hashJoinConfig.getRightEquals(),
            hashJoinConfig.getRightNumRows(),
            hashJoinConfig.getTextFoldingStrategies(),
            problemAggregator);
    var storage =
        Arrays.stream(hashJoinConfig.getLeftEquals())
            .map(Column::getStorage)
            .toArray(Storage[]::new);
    Set<UnorderedMultiValueKey> matchedRightKeys = new HashSet<>();

    Context context = Context.getCurrent();
    for (int leftRow = 0; leftRow < hashJoinConfig.getLeftNumRows(); leftRow++) {
      var leftKey = makeLeftKey(storage, leftRow, groupingProblemAggregator);
      // If any field of the key is null, it cannot match anything.
      List<Integer> rightRows = leftKey.hasAnyNulls() ? null : rightIndex.get(leftKey);
      if (rightRows != null) {
        if (joinKind.wantsCommon) {
          addAll(leftRow, rightRows, resultBuilder);
        }
        if (joinKind.wantsRightUnmatched) {
          matchedRightKeys.add(leftKey);
        }
      } else if (joinKind.wantsLeftUnmatched) {
        resultBuilder.addUnmatchedLeftRow(leftRow);
        context.safepoint();
      }
      context.safepoint();
    }

    if (joinKind.wantsRightUnmatched) {
      addUnmatchedRightRows(rightIndex, matchedRightKeys);
    }

    return resultBuilder.buildAndInvalidate();
  }

  private void addUnmatchedRightRows(
      MultiValueIndex<UnorderedMultiValueKey> rightIndex,
      Set<UnorderedMultiValueKey> matchedRightKeys) {
    Context context = Context.getCurrent();
    for (var rightEntry : rightIndex.mapping().entrySet()) {
      UnorderedMultiValueKey rightKey = rightEntry.getKey();
      boolean wasCompletelyUnmatched = !matchedRightKeys.contains(rightKey);
      if (wasCompletelyUnmatched) {
        for (int rightRow : rightEntry.getValue()) {
          resultBuilder.addUnmatchedRightRow(rightRow);
          context.safepoint();
        }
      }
      context.safepoint();
    }
  }

  public UnorderedMultiValueKey makeLeftKey(
      Storage[] storage,
      int rowNumber,
      ColumnAggregatedProblemAggregator groupingProblemAggregator) {
    var leftEquals = hashJoinConfig.getLeftEquals();
    var leftKey =
        new UnorderedMultiValueKey(storage, rowNumber, hashJoinConfig.getTextFoldingStrategies());
    leftKey.checkAndReportFloatingEquality(
        groupingProblemAggregator, columnIx -> leftEquals[columnIx].getName());
    return leftKey;
  }

  private static void addAll(
      int leftRow, List<Integer> rightGroup, SimpleHashJoinResultBuilder resultBuilder) {
    Context context = Context.getCurrent();
    for (var rightRow : rightGroup) {
      resultBuilder.addMatchedRowsPair(leftRow, rightRow);
      context.safepoint();
    }
    context.safepoint();
  }

  private static JoinKind flipJoinKind(JoinKind joinKind) {
    return switch (joinKind) {
      case LEFT_OUTER -> JoinKind.RIGHT_OUTER;
      case RIGHT_OUTER -> JoinKind.LEFT_OUTER;
      case LEFT_ANTI -> JoinKind.RIGHT_ANTI;
      case RIGHT_ANTI -> JoinKind.LEFT_ANTI;
      default -> joinKind;
    };
  }

  private class SimpleHashJoinResultBuilder {

    public SimpleHashJoinResultBuilder(boolean flipLeftAndRight) {
      this.flipLeftAndRight = flipLeftAndRight;
      this.resultBuilder = new JoinResult.Builder();
    }

    JoinResult.Builder resultBuilder;
    private final boolean flipLeftAndRight;

    public void addMatchedRowsPair(int leftIndex, int rightIndex) {
      addPair(leftIndex, rightIndex);
    }

    public void addUnmatchedLeftRow(int leftIndex) {
      addPair(leftIndex, -1);
    }

    public void addUnmatchedRightRow(int rightIndex) {
      addPair(-1, rightIndex);
    }

    public JoinResult buildAndInvalidate() {
      return resultBuilder.buildAndInvalidate();
    }

    private void addPair(int leftIndex, int rightIndex) {
      if (flipLeftAndRight) {
        resultBuilder.addMatchedRowsPair(rightIndex, leftIndex);
      } else {
        resultBuilder.addMatchedRowsPair(leftIndex, rightIndex);
      }
    }
  }
}
