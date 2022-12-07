package org.enso.table.data.table.join.scan;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.table.join.Between;
import org.enso.table.data.table.join.Equals;
import org.enso.table.data.table.join.EqualsIgnoreCase;

import java.util.Comparator;
import java.util.Locale;
import java.util.function.BiFunction;

public interface Matcher {
  boolean matches(int left, int right);

  class EqualsMatcher implements Matcher {

    private final BiFunction<Object, Object, Boolean> equalityFallback;
    private final Storage<?> leftStorage;
    private final Storage<?> rightStorage;
    public EqualsMatcher(Equals eq, BiFunction<Object, Object, Boolean> equalityFallback) {
      leftStorage = eq.left().getStorage();
      rightStorage = eq.right().getStorage();
      this.equalityFallback = equalityFallback;
    }

    @Override
    public boolean matches(int left, int right) {
      Object leftValue = leftStorage.getItemBoxed(left);
      Object rightValue = rightStorage.getItemBoxed(right);
      // We could do a fast-path for some known primitive types, but it doesn't matter as it will be replaced with hashing soon anyway.
      return equalityFallback.apply(leftValue, rightValue);
    }
  }

  class EqualsIgnoreCaseMatcher implements Matcher {
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
      return Text_Utils.equals_ignore_case(leftValue, rightValue, locale);
    }
  }

  class BetweenMatcher implements Matcher {

    private final Comparator<Object> objectComparator;
    public BetweenMatcher(Between between, Comparator<Object> objectComparator) {
      this.objectComparator = objectComparator;
    }

    @Override
    public boolean matches(int left, int right) {
      // We could do a fast-path for some known primitive types, but it doesn't matter as it should be replaced with sorting optimization soon(ish).
      // TODO
      throw new IllegalStateException("Not implemented yet.");
    }
  }
}
