package org.enso.table.data.column.operation.map.text;

import java.util.BitSet;
import java.util.regex.Pattern;

import com.ibm.icu.impl.UnicodeRegex;
import org.enso.base.Regex_Utils;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;

public class LikeOp extends StringBooleanOp {
  public LikeOp() {
    super(Storage.Maps.LIKE);
  }


  /**
   * There is <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8032926">a bug with Java Regex in Unicode normalized mode (CANON_EQ) with quoting</a>.
   * Once that bug is fixed, we should add all relevant Unicode flags here too,
   * consistently with the Default Enso regex engine.
   */
  private final static int REGEX_FLAGS = Pattern.DOTALL;

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
  public BoolStorage runMap(SpecializedStorage<String> storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (arg == null) {
      BitSet newVals = new BitSet();
      BitSet newMissing = new BitSet();
      newMissing.set(0, storage.size());
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    } else if (arg instanceof String argString) {
      Pattern pattern = createRegexPatternFromSql(argString);
      BitSet newVals = new BitSet();
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (storage.isNa(i)) {
          newMissing.set(i);
        } else if (pattern.matcher(storage.getItem(i)).matches()) {
          newVals.set(i);
        }
      }
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    } else {
      throw new UnexpectedTypeException("a Text");
    }
  }
}
