package org.enso.table.aggregations;

import java.util.List;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;
import org.enso.table.data.table.problems.UnquotedDelimiter;
import org.graalvm.polyglot.Context;

public class Concatenate extends Aggregator {
  private final Storage<?> storage;
  private final String separator;
  private final String prefix;
  private final String suffix;
  private final String quote;

  public Concatenate(
      String name, Column column, String separator, String prefix, String suffix, String quote) {
    super(name, TextType.VARIABLE_LENGTH);
    this.storage = column.getStorage();

    this.separator = separator == null ? "" : separator;
    this.prefix = prefix;
    this.suffix = suffix;
    this.quote = quote == null ? "" : quote;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Context context = Context.getCurrent();
    StringBuilder current = null;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value == null || value instanceof String) {
        String textValue = toQuotedString(value, quote, separator);

        if (!separator.equals("") && quote.equals("") && textValue.contains(separator)) {
          this.addProblem(new UnquotedDelimiter(this.getName(), row, "Unquoted delimiter."));
        }

        if (current == null) {
          current = new StringBuilder();
          current.append(textValue);
        } else {
          current.append(separator);
          current.append(textValue);
        }
      } else {
        this.addProblem(new InvalidAggregation(this.getName(), row, "Not a text value."));
        return null;
      }

      context.safepoint();
    }

    if (current == null) {
      return null;
    }

    if (prefix != null) {
      current.insert(0, prefix);
    }
    current.append(suffix);
    return current.toString();
  }

  private static String toQuotedString(Object value, final String quote, final String separator) {
    if (value == null) {
      return "";
    }

    String textValue = value.toString();
    if (!quote.isEmpty()) {
      boolean includes_separator = !separator.isEmpty() && textValue.contains(separator);
      boolean includes_quote = textValue.contains(quote);
      boolean needs_quoting = textValue.isEmpty() || includes_separator || includes_quote;
      if (needs_quoting) {
        return quote + textValue.replace(quote, quote + quote) + quote;
      }
    }

    return textValue;
  }
}
