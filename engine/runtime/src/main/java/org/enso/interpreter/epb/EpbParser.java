package org.enso.interpreter.epb;

import com.oracle.truffle.api.source.Source;

/** A class containing helpers for creating and parsing EPB code */
public class EpbParser {
  private static final String separator = "#";

  /** Lists all the languages supported in polyglot eval. */
  public enum ForeignLanguage {
    JS;

    /** @return a Truffle language ID associated with this language */
    public String toTruffleLanguage() {
      return "js";
    }
  }

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

  /**
   * Builds a new source instance that can later be parsed by this class.
   *
   * @param language the foreign language to use
   * @param foreignSource the foreign source to evaluate
   * @param name the name of the source
   * @return a source instance, parsable by the EPB language
   */
  public static Source buildSource(ForeignLanguage language, String foreignSource, String name) {
    return Source.newBuilder(EpbLanguage.ID, language + separator + foreignSource, name).build();
  }

  /**
   * Transforms an Enso-side syntactic language tag into a recognized language object.
   *
   * @param tag the tag to parse
   * @return a corresponding language value, or null if the language is not recognized
   */
  public static ForeignLanguage getLanguage(String tag) {
    if (tag.equals("js")) {
      return ForeignLanguage.JS;
    }
    return null;
  }
}
