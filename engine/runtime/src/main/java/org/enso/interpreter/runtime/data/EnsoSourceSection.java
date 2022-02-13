package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.source.SourceSection;

/**
 * Wrapper for exposing source sections in Enso. Delegates to the original methods with no behaviour
 * changes.
 */
public class EnsoSourceSection {
  private final SourceSection sourceSection;

  public EnsoSourceSection(SourceSection sourceSection) {
    this.sourceSection = sourceSection;
  }

  public int getStartLine() {
    return sourceSection.getStartLine();
  }

  public int getEndLine() {
    return sourceSection.getEndLine();
  }

  public int getEndColumn() {
    return sourceSection.getEndColumn();
  }

  public int getCharIndex() {
    return sourceSection.getCharIndex();
  }

  public int getCharLength() {
    return sourceSection.getCharLength();
  }

  public int getCharEndIndex() {
    return sourceSection.getCharEndIndex();
  }

  public CharSequence getCharacters() {
    return sourceSection.getCharacters();
  }

  public int getStartColumn() {
    return sourceSection.getStartColumn();
  }

  public boolean isAvailable() {
    return sourceSection.isAvailable();
  }

  public boolean hasLines() {
    return sourceSection.hasLines();
  }

  public boolean hasColumns() {
    return sourceSection.hasColumns();
  }

  public boolean hasCharIndex() {
    return sourceSection.hasCharIndex();
  }

  public EnsoSource getSource() {
    return new EnsoSource(sourceSection.getSource());
  }
}
