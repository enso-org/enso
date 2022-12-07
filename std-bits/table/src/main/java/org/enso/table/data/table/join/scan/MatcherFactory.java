package org.enso.table.data.table.join.scan;

import org.enso.table.data.table.join.Between;
import org.enso.table.data.table.join.Equals;
import org.enso.table.data.table.join.EqualsIgnoreCase;
import org.enso.table.data.table.join.JoinCondition;

import java.util.Comparator;
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
      case Equals eq -> new Matcher.EqualsMatcher(eq, equalityFallback);
      case EqualsIgnoreCase eq -> new Matcher.EqualsIgnoreCaseMatcher(eq);
      case Between between -> new Matcher.BetweenMatcher(between, objectComparator);
      default -> throw new UnsupportedOperationException("Unsupported join condition: " + condition);
    };
  }
}
