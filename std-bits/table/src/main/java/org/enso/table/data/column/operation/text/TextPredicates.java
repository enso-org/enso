package org.enso.table.data.column.operation.text;

import com.ibm.icu.impl.UnicodeRegex;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiPredicate;
import java.util.regex.Pattern;
import org.enso.base.Regex_Utils;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.operation.comparators.GenericComparators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.UnexpectedTypeException;

public final class TextPredicates extends GenericComparators<String> {
  public static final TextPredicates STARTS_WITH = new TextPredicates(Text_Utils::starts_with);
  public static final TextPredicates ENDS_WITH = new TextPredicates(Text_Utils::ends_with);
  public static final TextPredicates CONTAINS = new TextPredicates(Text_Utils::contains);
  public static final TextPredicates LIKE = new TextPredicates(TextPredicates::LikePredicate);
  public static final TextPredicates REGEX_MATCH =
      new TextPredicates(TextPredicates::RegexMatchPredicate);

  private TextPredicates(BiPredicate<String, String> predicate) {
    super(TextType.VARIABLE_LENGTH, predicate);
  }

  @Override
  protected boolean onIncomparable(Object left, Object right) {
    throw new UnexpectedTypeException("a Text", right.toString());
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    var storageType = left.getType();
    return storageType instanceof TextType || storageType instanceof NullType;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return canApplyMap(left, null);
  }

  private static final Map<String, Pattern> regexCache = new HashMap<>();

  private static Pattern createRegexPatternFromSql(String sqlPattern) {
    String regex = Regex_Utils.sql_like_pattern_to_regex(sqlPattern);
    return createRegexPattern(regex);
  }

  private static Pattern createRegexPattern(String regex) {
    String unicodeTransformed = UnicodeRegex.fix(regex);
    /*
     * There is <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8032926">a bug with Java
     * Regex in Unicode normalized mode (CANON_EQ) with quoting</a>. Once that bug is fixed, we should
     * add all relevant Unicode flags here too, consistently with the Default Enso regex engine.
     */
    return Pattern.compile(unicodeTransformed, Pattern.DOTALL);
  }

  private static boolean LikePredicate(String left, String right) {
    return regexCache
        .computeIfAbsent(right, TextPredicates::createRegexPatternFromSql)
        .matcher(left)
        .matches();
  }

  private static boolean RegexMatchPredicate(String left, String right) {
    return regexCache
        .computeIfAbsent(right, TextPredicates::createRegexPattern)
        .matcher(left)
        .matches();
  }
}
