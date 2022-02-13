package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

/** Wrapper for exposing sources to Enso. Delegates to original methods with no behavior changes. */
public class EnsoSource {
  private final Source source;

  public EnsoSource(Source source) {
    this.source = source;
  }

  public String getLanguage() {
    return source.getLanguage();
  }

  public String getName() {
    return source.getName();
  }

  public String getPath() {
    return source.getPath();
  }

  public boolean isInternal() {
    return source.isInternal();
  }

  public CharSequence getCharacters() {
    return source.getCharacters();
  }

  public int getLength() {
    return source.getLength();
  }

  public CharSequence getCharacters(int lineNumber) {
    return source.getCharacters(lineNumber);
  }

  public int getLineCount() {
    return source.getLineCount();
  }
}
