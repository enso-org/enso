package org.enso.base.text;

import org.enso.base.Text_Utils;

/**
 * Represents a span of code units within a String.
 *
 * <p>The start index indicates the first code unit of the span and the end index indicates the
 * first code unit after the end of the span.
 */
public class Utf16Span {

  public final long start, end;

  public Utf16Span(long start, long end) {
    this.start = start;
    this.end = end;
  }
}
