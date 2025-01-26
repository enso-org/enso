package org.enso.table.data.column.operation.map.text;

import com.ibm.icu.impl.UnicodeRegex;
import java.util.regex.Pattern;
import org.enso.base.Regex_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public class LikeOp extends StringBooleanOp {
  public LikeOp() {
    super(Storage.Maps.LIKE);
  }

  /**
   * There is <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8032926">a bug with Java
   * Regex in Unicode normalized mode (CANON_EQ) with quoting</a>. Once that bug is fixed, we should
   * add all relevant Unicode flags here too, consistently with the Default Enso regex engine.
   */
  private static final int REGEX_FLAGS = Pattern.DOTALL;

  private Pattern createRegexPatternFromSql(String sqlPattern) {
    String regex = Regex_Utils.sql_like_pattern_to_regex(sqlPattern);
    String unicodeTransformed = UnicodeRegex.fix(regex);
    return Pattern.compile(unicodeTransformed, REGEX_FLAGS);
  }

  @Override
  protected boolean doString(String a, String b) {
    return createRegexPatternFromSql(b).matcher(a).matches();
  }

  @Override
  public Storage<Boolean> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BoolStorage.makeEmpty(storage.getSize());
    } else if (arg instanceof String argString) {
      Pattern pattern = createRegexPatternFromSql(argString);
      long size = storage.getSize();
      var builder = Builder.getForBoolean(size);
      Context context = Context.getCurrent();
      for (long i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendBoolean(pattern.matcher(storage.getItemBoxed(i)).matches());
        }

        context.safepoint();
      }
      return builder.seal();
    } else {
      throw new UnexpectedTypeException("a Text");
    }
  }
}
