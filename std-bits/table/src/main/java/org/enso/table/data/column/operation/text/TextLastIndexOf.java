package org.enso.table.data.column.operation.text;

public class TextLastIndexOf extends AbstractTextIndexOfOperation {
  public static final TextLastIndexOf INSTANCE = new TextLastIndexOf();

  private TextLastIndexOf() {
    super();
  }

  @Override
  protected int findCodeunitIndex(String value, String needle) {
    return value.lastIndexOf(needle);
  }
}
