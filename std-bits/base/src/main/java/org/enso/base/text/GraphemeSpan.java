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
public class GraphemeSpan extends Utf16Span {

  public final int grapheme_start, grapheme_end;

  /**
   * Constructs a span of characters (understood as extended grapheme clusters).
   *
   * @param grapheme_start index of the first extended grapheme cluster contained within the span
   *     (or location of the span if it is empty)
   * @param grapheme_end index of the first extended grapheme cluster after start that is not
   *     contained
   * @param codeunit_start code unit index of {@code grapheme_start}
   * @param codeunit_end code unit index of {@code grapheme_end}
   */
  public GraphemeSpan(int grapheme_start, int grapheme_end, int codeunit_start, int codeunit_end) {
    super(codeunit_start, codeunit_end);
    this.grapheme_start = grapheme_start;
    this.grapheme_end = grapheme_end;
  }
}
