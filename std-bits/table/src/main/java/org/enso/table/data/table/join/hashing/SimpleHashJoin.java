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
 * <p>It then delegates to {@code remainingMatcher} to perform the remaining conditions on the
 * matching pairs of row subsets.
 */
public class SimpleHashJoin implements JoinStrategy {

  public SimpleHashJoin(List<HashableCondition> conditions, JoinKind joinKind) {
    var tempHashJoinConfig = new HashJoinConfig(conditions);

    // alogirthm assumes that left table is the big table. If not we will flip the tables round to
    // do the join
    if (tempHashJoinConfig.getLeftEquals()[0].getSize()
        >= tempHashJoinConfig.getRightEquals()[0].getSize()) {
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
    // alogirthm assumes that left table is the big table. If not we have flipped the tables round
    // to do the join
    // so if you are debugging your left table might not be your left table here
    // the result builder flips the indexes back as you add them
    assert (hashJoinConfig.getLeftEquals()[0].getSize()
        >= hashJoinConfig.getRightEquals()[0].getSize());

    var groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);

    var rightIndex = makeRightIndex(problemAggregator);

    var leftEquals = hashJoinConfig.getLeftEquals();
    var storage = Arrays.stream(leftEquals).map(Column::getStorage).toArray(Storage[]::new);

    Set<UnorderedMultiValueKey> matchedRightKeys = new HashSet<>();

    Context context = Context.getCurrent();
    for (int leftRow = 0; leftRow < leftEquals[0].getSize(); leftRow++) {
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

  private MultiValueIndex<UnorderedMultiValueKey> makeRightIndex(
      ProblemAggregator problemAggregator) {
    var rightEquals = hashJoinConfig.getRightEquals();
    return MultiValueIndex.makeUnorderedIndex(
        rightEquals,
        rightEquals[0].getSize(),
        hashJoinConfig.getTextFoldingStrategies(),
        problemAggregator);
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
