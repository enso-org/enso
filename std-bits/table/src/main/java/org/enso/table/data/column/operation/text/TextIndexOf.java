package org.enso.table.data.column.operation.text;

public class TextIndexOf extends AbstractTextIndexOfOperation {
  public static final TextIndexOf INSTANCE = new TextIndexOf();

  private TextIndexOf() {
    super();
  }

  @Override
  protected int findCodeunitIndex(String value, String needle) {
    return value.indexOf(needle);
  }
}
