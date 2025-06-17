package org.enso.table.read;

/** An exception thrown when a problem occured when reading a fixed-width file. */
public class FixedWidthLineTooLongException extends RuntimeException {
  public final long sourceLineNumber;
  public final long maxLineLength;

  public FixedWidthLineTooLongException(long sourceLineNumber, long maxLineLength) {
    this.sourceLineNumber = sourceLineNumber;
    this.maxLineLength = maxLineLength;
  }
}
