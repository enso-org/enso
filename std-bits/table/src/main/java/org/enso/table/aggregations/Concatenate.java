package org.enso.table.aggregations;

import com.ibm.icu.text.BreakIterator;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.List;

public class Concatenate extends AggregateColumn {
  private final Storage storage;
  private final String join;
  private final String prefix;
  private final String suffix;
  private final String quote;

  public Concatenate(String name, Column column, String join, String prefix, String suffix, String quote) {
    super(name, Storage.Type.STRING);
    this.storage = column.getStorage();

    this.join = join == null ? "" : join;
    this.prefix = prefix;
    this.suffix = suffix;
    this.quote = quote == null ? "" : quote;
  }

  @Override
  public Object aggregate(List<Integer> rows) {
    StringBuilder current = null;
    for (int row: rows) {
      Object value = storage.getItemBoxed(row);
      if (value == null || value instanceof String) {
        String textValue = ToQuotedString(value, quote, join);
        if (current == null) {
          current = new StringBuilder();
          current.append(textValue);
        } else {
          current.append(join);
          current.append(textValue);
        }
      } else {
        return new InvalidAggregation(this.getName(), row, "Non-Text value - cannot Concatenate");
      }
    }

    if (current == null) {
      return null;
    }

    if (prefix != null) { current.insert(0, prefix); }
    current.append(suffix);
    return current.toString();
  }

  private static String ToQuotedString(Object value, final String quote, final String join) {
    if (value == null) {
      return "";
    }

    String textValue = value.toString();
    if (!quote.equals("") && (textValue.equals("") || textValue.contains(join))) {
      return quote + textValue.replace(quote, quote + quote) + quote;
    }

    return textValue;
  }
}
