package org.enso.table.data.table.join;

import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.join.scan.Matcher;
import org.enso.table.data.table.join.scan.MatcherFactory;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.List;
import java.util.stream.Collectors;

public class IndexJoin implements JoinStrategy {
  private record HashEqualityCondition(
      Column left, Column right, TextFoldingStrategy textFoldingStrategy) {
  }

  @Override
  public JoinResult join(Table left, Table right, List<JoinCondition> conditions, ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    List<HashEqualityCondition> equalConditions =
        conditions.stream()
            .filter(IndexJoin::isSupported)
            .map(IndexJoin::makeHashEqualityCondition)
            .collect(Collectors.toList());

    var remainingConditions =
        conditions.stream().filter(c -> !isSupported(c)).collect(Collectors.toList());

    var leftEquals =
        equalConditions.stream().map(HashEqualityCondition::left).toArray(Column[]::new);
    var rightEquals =
        equalConditions.stream().map(HashEqualityCondition::right).toArray(Column[]::new);
    var textFoldingStrategies =
        equalConditions.stream()
            .map(HashEqualityCondition::textFoldingStrategy)
            .collect(Collectors.toList());

    var leftIndex =
        MultiValueIndex.makeUnorderedIndex(leftEquals, left.rowCount(), textFoldingStrategies, problemAggregator);
    var rightIndex =
        MultiValueIndex.makeUnorderedIndex(rightEquals, right.rowCount(), textFoldingStrategies, problemAggregator);

    MatcherFactory factory = new MatcherFactory();
    Matcher remainingMatcher = factory.create(
        remainingConditions, new ColumnAggregatedProblemAggregator(problemAggregator)
    );

    JoinResult.Builder resultBuilder = new JoinResult.Builder();
    for (var leftEntry : leftIndex.mapping().entrySet()) {
      UnorderedMultiValueKey leftKey = leftEntry.getKey();
      List<Integer> leftRows = leftEntry.getValue();
      List<Integer> rightRows = rightIndex.get(leftKey);

      if (rightRows != null) {
        for (var leftRow : leftRows) {
          for (var rightRow : rightRows) {
            if (remainingMatcher.matches(leftRow, rightRow)) {
              resultBuilder.addRow(leftRow, rightRow);
            }

            context.safepoint();
          }

          context.safepoint();
        }
      }

      context.safepoint();
    }

    return resultBuilder.build();
  }

  private static boolean isSupported(JoinCondition condition) {
    return switch (condition) {
      case Equals ignored -> true;
      case EqualsIgnoreCase ignored -> true;
      default -> false;
    };
  }

  private static HashEqualityCondition makeHashEqualityCondition(JoinCondition eq) {
    switch (eq) {
      case Equals e -> {
        return new HashEqualityCondition(
            e.left(), e.right(), TextFoldingStrategy.unicodeNormalizedFold);
      }
      case EqualsIgnoreCase e -> {
        return new HashEqualityCondition(
            e.left(), e.right(), TextFoldingStrategy.caseInsensitiveFold(e.locale()));
      }
      default -> throw new IllegalStateException(
          "Impossible: trying to convert condition "
              + eq
              + " to a HashEqualityCondition, but it should not be marked as supported. This is a"
              + " bug in the Table library.");
    }
  }
}
