package org.enso.table.data.column.operation.map.text;

import java.util.BitSet;
import java.util.regex.Pattern;
import org.enso.base.Regex_Utils;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;

public class LikeOp extends StringBooleanOp {
  public LikeOp() {
    super(Storage.Maps.LIKE);
  }

  /** These flags should be consistent with default Enso regex settings. */
  private final static int UNICODE_REGEX = Pattern.CANON_EQ | Pattern.UNICODE_CHARACTER_CLASS | Pattern.UNICODE_CASE;
  private final static int REGEX_FLAGS = UNICODE_REGEX | Pattern.DOTALL;

  private Pattern createRegexPatternFromSql(String sqlPattern) {
    return Pattern.compile(Regex_Utils.sql_like_pattern_to_regex(sqlPattern), REGEX_FLAGS);
  }

  @Override
  protected boolean doString(String a, String b) {
    return createRegexPatternFromSql(b).matcher(a).matches();
  }

  @Override
  public Storage runMap(SpecializedStorage<String> storage, Object arg) {
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
