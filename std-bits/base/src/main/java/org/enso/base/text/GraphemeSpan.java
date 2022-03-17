package org.enso.base.text;

/**
 * Represents a span of characters (understood as extended grapheme clusters) within a Text.
 *
 * <p>The start index indicates the first grapheme of the span and the end index indicates the first
 * grapheme after the end of the span.
 *
 * <p>Represents an empty span if start and end indices are equal. Such an empty span refers to the
 * space just before the grapheme corresponding to index start.
 */
public class GraphemeSpan {

  public final long start, end;

  /**
   * Constructs a span of characters (understood as extended grapheme clusters).
   *
   * @param start index of the first extended grapheme cluster contained within the span (or
   *     location of the span if it is empty)
   * @param end index of the first extended grapheme cluster after start that is not contained
   *     within the span
   */
  public GraphemeSpan(long start, long end) {
    this.start = start;
    this.end = end;
  }
}
