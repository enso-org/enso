package org.enso.table.data.table.join.scan;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.table.join.Between;
import org.enso.table.data.table.join.Equals;
import org.enso.table.data.table.join.EqualsIgnoreCase;
import org.enso.table.data.table.join.JoinCondition;

import java.util.Locale;

public interface Matcher {
  boolean matches(int left, int right);

  static Matcher create(JoinCondition condition) {
    return switch (condition) {
      case Equals eq -> new EqualsMatcher(eq);
      case EqualsIgnoreCase eq -> new EqualsIgnoreCaseMatcher(eq);
      case Between between -> new BetweenMatcher(between);
      default -> throw new UnsupportedOperationException("Unsupported join condition: " + condition);
    };
  }


  class EqualsMatcher implements Matcher {

    private final Storage<?> leftStorage;
    private final Storage<?> rightStorage;
    public EqualsMatcher(Equals eq) {
      leftStorage = eq.left().getStorage();
      rightStorage = eq.right().getStorage();
    }

    @Override
    public boolean matches(int left, int right) {
      Object leftValue = leftStorage.getItemBoxed(left);
      Object rightValue = rightStorage.getItemBoxed(right);
      // TODO normalize equality of strings and decimals with ints
      return leftValue.equals(rightValue);
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
    public BetweenMatcher(Between between) {
    }

    @Override
    public boolean matches(int left, int right) {
      throw new IllegalStateException("Not implemented yet.");
    }
  }
}
