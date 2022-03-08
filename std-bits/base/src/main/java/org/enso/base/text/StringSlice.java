package org.enso.base.text;

class StringSlice implements CharSequence {
  private final CharSequence text;
  private final int subStart, subEnd;

  public StringSlice(CharSequence text, int start, int end) {
    this.text = text;
    this.subStart = start;
    this.subEnd = end;
  }

  @Override
  public int length() {
    return subEnd - subStart;
  }

  @Override
  public char charAt(int index) {
    return text.charAt(subStart + index);
  }

  @Override
  public CharSequence subSequence(int start, int end) {
    return new StringSlice(text, subStart + start, subStart + end);
  }

  @Override
  public String toString() {
    return text.subSequence(subStart, subEnd).toString();
  }
}
