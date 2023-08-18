package org.enso.interpreter.epb;

import com.oracle.truffle.api.source.Source;
import org.enso.polyglot.ForeignLanguage;

/** A class containing helpers for creating and parsing EPB code */
public class EpbParser {
  /** A parsing result. */
  public static class Result {
    private final ForeignLanguage language;
    private final String foreignSource;

    private Result(ForeignLanguage language, String foreignSource) {
      this.language = language;
      this.foreignSource = foreignSource;
    }

    /** @return the foreign language code to eval */
    public String getForeignSource() {
      return foreignSource;
    }

    /** @return the foreign language in which the source is written */
    public ForeignLanguage getLanguage() {
      return language;
    }
  }

  /**
   * Parses an EPB source
   *
   * @param source the source to parse
   * @return the result of parsing
   */
  public static Result parse(Source source) {
    String src = source.getCharacters().toString();
    String[] langAndCode = src.split("#", 2);
    return new Result(ForeignLanguage.valueOf(langAndCode[0]), langAndCode[1]);
  }
}
