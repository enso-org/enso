package org.enso.base.text;

/**
 * Represents a span of UTF-16 code units within a String.
 *
 * <p>The start index indicates the first code unit of the span and the end index indicates the
 * first code unit after the end of the span.
 */
public class Utf16Span {

  public final int codeunit_start, codeunit_end;

  /** Constructs a span of UTF-16 code units. */
  public Utf16Span(int codeunit_start, int codeunit_end) {
    this.codeunit_start = codeunit_start;
    this.codeunit_end = codeunit_end;
  }
}
