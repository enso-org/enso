package org.enso.table.data.table.join.scan;

import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.enso.base.ObjectComparator;
import org.enso.base.Text_Utils;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.table.join.Between;
import org.enso.table.data.table.join.Equals;
import org.enso.table.data.table.join.EqualsIgnoreCase;
import org.enso.table.data.table.join.JoinCondition;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.data.table.problems.FloatingPointGrouping;

public class MatcherFactory {
  public Matcher create(JoinCondition condition) {
    return switch (condition) {
      case Equals eq -> new EqualsMatcher(eq);
      case EqualsIgnoreCase eq -> new EqualsIgnoreCaseMatcher(eq);
      case Between between -> new BetweenMatcher(between);
      default -> throw new UnsupportedOperationException(
          "Unsupported join condition: " + condition);
    };
  }

  public Matcher create(List<JoinCondition> condition) {
    List<Matcher> matchers = condition.stream().map(this::create).collect(Collectors.toList());
    return new CompoundMatcher(matchers);
  }

  static final class CompoundMatcher implements Matcher {
    private final List<Matcher> matchers;

    CompoundMatcher(List<Matcher> matchers) {
      this.matchers = matchers;
    }

    @Override
    public boolean matches(int left, int right) {
      for (Matcher matcher : matchers) {
        if (!matcher.matches(left, right)) {
          return false;
        }
      }

      return true;
    }

    @Override
    public AggregatedProblems getProblems() {
      return AggregatedProblems.merge(
          matchers.stream().map(Matcher::getProblems).toArray(AggregatedProblems[]::new));
    }
  }

  static final class EqualsMatcher implements Matcher {
    private final Storage<?> leftStorage;
    private final Storage<?> rightStorage;
    private final String leftColumnName;
    private final String rightColumnName;

    private final AggregatedProblems problems;

    public EqualsMatcher(Equals eq) {
      leftStorage = eq.left().getStorage();
      rightStorage = eq.right().getStorage();
      leftColumnName = eq.left().getName();
      rightColumnName = eq.right().getName();
      problems = new AggregatedProblems();
    }

    @Override
    public boolean matches(int left, int right) {
      Object leftValue = leftStorage.getItemBoxed(left);
      Object rightValue = rightStorage.getItemBoxed(right);

      if (NumericConverter.isDecimalLike(leftValue)) {
        problems.add(new FloatingPointGrouping(leftColumnName, left));
      }

      if (NumericConverter.isDecimalLike(rightValue)) {
        problems.add(new FloatingPointGrouping(rightColumnName, right));
      }

      return ObjectComparator.areEqual(leftValue, rightValue);
    }

    @Override
    public AggregatedProblems getProblems() {
      return problems;
    }
  }

  static final class EqualsIgnoreCaseMatcher implements Matcher {
    private final StringStorage leftStorage;
    private final StringStorage rightStorage;

    private final Locale locale;

    public EqualsIgnoreCaseMatcher(EqualsIgnoreCase eq) {
      if (eq.left().getStorage() instanceof StringStorage leftStrings) {
        leftStorage = leftStrings;
      } else {
        throw new IllegalArgumentException("Expected left column to have type Text.");
      }

      if (eq.right().getStorage() instanceof StringStorage rightStrings) {
        rightStorage = rightStrings;
      } else {
        throw new IllegalArgumentException("Expected right column to have type Text.");
      }

      locale = eq.locale();
    }

    @Override
    public boolean matches(int left, int right) {
      String leftValue = leftStorage.getItem(left);
      String rightValue = rightStorage.getItem(right);

      if (leftValue == null && rightValue == null) {
        return true;
      }

      if (leftValue == null || rightValue == null) {
        return false;
      }

      return Text_Utils.equals_ignore_case(leftValue, rightValue, locale);
    }
  }

  static final class BetweenMatcher implements Matcher {
    private final Storage<?> leftStorage;
    private final Storage<?> rightLowerStorage;
    private final Storage<?> rightUpperStorage;

    public BetweenMatcher(Between between) {
      leftStorage = between.left().getStorage();
      rightLowerStorage = between.rightLower().getStorage();
      rightUpperStorage = between.rightUpper().getStorage();
    }

    @Override
    public boolean matches(int left, int right) {
      Object leftValue = leftStorage.getItemBoxed(left);
      Object rightLowerValue = rightLowerStorage.getItemBoxed(right);
      Object rightUpperValue = rightUpperStorage.getItemBoxed(right);

      // If any value is missing, such a pair of rows is never correlated with Between as we assume
      // the ordering is not well-defined for missing values.
      if (leftValue == null || rightLowerValue == null || rightUpperValue == null) {
        return false;
      }

      return ObjectComparator.DEFAULT.compare(leftValue, rightLowerValue) >= 0
          && ObjectComparator.DEFAULT.compare(leftValue, rightUpperValue) <= 0;
    }
  }
}
