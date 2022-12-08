package org.enso.table.data.table.join.scan;

import org.enso.base.Text_Utils;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.table.join.Between;
import org.enso.table.data.table.join.Equals;
import org.enso.table.data.table.join.EqualsIgnoreCase;
import org.enso.table.data.table.join.JoinCondition;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.data.table.problems.FloatingPointGrouping;

import java.util.Comparator;
import java.util.Locale;
import java.util.function.BiFunction;

public class MatcherFactory {
  private final Comparator<Object> objectComparator;
  private final BiFunction<Object, Object, Boolean> equalityFallback;

  public MatcherFactory(Comparator<Object> objectComparator, BiFunction<Object, Object, Boolean> equalityFallback) {
    this.objectComparator = objectComparator;
    this.equalityFallback = equalityFallback;
  }

  public Matcher create(JoinCondition condition) {
    return switch (condition) {
      case Equals eq -> new EqualsMatcher(eq, equalityFallback);
      case EqualsIgnoreCase eq -> new EqualsIgnoreCaseMatcher(eq);
      case Between between -> new BetweenMatcher(between, objectComparator);
      default -> throw new UnsupportedOperationException("Unsupported join condition: " + condition);
    };
  }

  static final class EqualsMatcher implements Matcher {

    private final BiFunction<Object, Object, Boolean> equalityFallback;
    private final Storage<?> leftStorage;
    private final Storage<?> rightStorage;
    private final String leftColumnName;
    private final String rightColumnName;

    private final AggregatedProblems problems;

    public EqualsMatcher(Equals eq, BiFunction<Object, Object, Boolean> equalityFallback) {
      leftStorage = eq.left().getStorage();
      rightStorage = eq.right().getStorage();
      leftColumnName = eq.left().getName();
      rightColumnName = eq.right().getName();
      this.equalityFallback = equalityFallback;
      problems = new AggregatedProblems();
    }

    @Override
    public boolean matches(int left, int right) {
      Object leftValue = leftStorage.getItemBoxed(left);
      Object rightValue = rightStorage.getItemBoxed(right);

      if (NumericConverter.isCoercibleToDouble(leftValue)) {
        problems.add(new FloatingPointGrouping(leftColumnName, left));
      }

      if (NumericConverter.isCoercibleToDouble(rightValue)) {
        problems.add(new FloatingPointGrouping(rightColumnName, right));
      }

      // We could do a fast-path for some known primitive types, but it doesn't matter as it will be replaced with hashing soon anyway.
      return equalityFallback.apply(leftValue, rightValue);
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

    private final Comparator<Object> objectComparator;
    private final Storage<?> leftStorage;
    private final Storage<?> rightLowerStorage;
    private final Storage<?> rightUpperStorage;
    public BetweenMatcher(Between between, Comparator<Object> objectComparator) {
      this.objectComparator = objectComparator;
      leftStorage = between.left().getStorage();
      rightLowerStorage = between.rightLower().getStorage();
      rightUpperStorage = between.rightUpper().getStorage();
    }

    @Override
    public boolean matches(int left, int right) {
      Object leftValue = leftStorage.getItemBoxed(left);
      Object rightLowerValue = rightLowerStorage.getItemBoxed(right);
      Object rightUpperValue = rightUpperStorage.getItemBoxed(right);

      // If any value is missing, such a pair of rows is never correlated with Between as we assume the ordering is not well-defined for missing values.
      if (leftValue == null || rightLowerValue == null || rightUpperValue == null) {
        return false;
      }

      // We could do a fast-path for some known primitive types, but it doesn't matter as it should be replaced with sorting optimization soon(ish).
      return objectComparator.compare(leftValue, rightLowerValue) >= 0 && objectComparator.compare(leftValue, rightUpperValue) <= 0;
    }
  }
}
