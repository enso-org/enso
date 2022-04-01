package org.enso.table.aggregations;

import com.ibm.icu.text.BreakIterator;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.List;

/***
 * Aggregate Column finding the longest or shortest string in a group.
 */
public class ShortestOrLongest extends Aggregator {
  private final Storage storage;
  private final int minOrMax;

  public ShortestOrLongest(String name, Column column, int minOrMax) {
    super(name, Storage.Type.STRING);
    this.storage = column.getStorage();
    this.minOrMax = minOrMax;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    long length = 0;
    Object current = null;

    for (int row: indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null) {
        if (!(value instanceof String)) {
          this.addProblem(new InvalidAggregation(this.getName(), row, "Not a text value."));
          return null;
        }

        long valueLength = GraphemeLength((String)value);
        if (current == null || Long.compare(valueLength, length) == minOrMax) {
          length = valueLength;
          current = value;
        }
      }
    }

    return current;
  }

  private static long GraphemeLength(String text) {
    BreakIterator iter = BreakIterator.getCharacterInstance();
    iter.setText(text);

    int count = 0;
    for (int end = iter.next(); end != BreakIterator.DONE; end = iter.next()) {
      count++;
    }

    return count;
  }
}
