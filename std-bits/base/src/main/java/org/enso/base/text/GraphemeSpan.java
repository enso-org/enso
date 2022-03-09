package org.enso.base.text;

/**
 * Represents a span of characters within a Text.
 *
 * <p>The indices are stored in grapheme cluster space.
 *
 * <p>The start index indicates the first grapheme of the span and the end index indicates the
 * first grapheme after the end of the span.
 */
public class GraphemeSpan {

  public final long start, end;

  public GraphemeSpan(long start, long end) {
    this.start = start;
    this.end = end;
  }
}
