package org.enso.table.data.table.problems;

public class UnquotedDelimiter extends ColumnAggregatedProblems {
  private final String message;

  public UnquotedDelimiter(String columnName, int row, String message) {
    super(columnName, row);
    this.message = message;
  }

  @Override
  public String getMessage() { return message; }

  @Override
  public boolean merge(ColumnAggregatedProblems another) {
    if (another instanceof UnquotedDelimiter &&
        this.getColumnName().equals(another.getColumnName()) &&
        this.message.equals(((UnquotedDelimiter) another).message)) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }
}
