package org.enso.polyglot;

import com.oracle.truffle.api.source.Source;
import java.util.Arrays;

/** Lists all the languages supported in polyglot eval. */
public enum ForeignLanguage {
  JS("js", "js"),
  PY("python", "python"),
  R("R", "r");

  public static final String ID = "epb";

  private final String truffleId;
  private final String syntacticTag;

  ForeignLanguage(String truffleId, String syntacticTag) {
    this.truffleId = truffleId;
    this.syntacticTag = syntacticTag;
  }

  /** @return a Truffle language ID associated with this language */
  public String getTruffleId() {
    return truffleId;
  }

  /**
   * Transforms an Enso-side syntactic language tag into a recognized language object.
   *
   * @param tag the tag to parse
   * @return a corresponding language value, or null if the language is not recognized
   */
  public static ForeignLanguage getBySyntacticTag(String tag) {
    return Arrays.stream(values()).filter(l -> l.syntacticTag.equals(tag)).findFirst().orElse(null);
  }

  /**
   * Builds a new source instance that can later be parsed by this class.
   *
   * @param foreignSource the foreign source to evaluate
   * @param name the name of the source
   * @return a source instance, parsable by the EPB language
   */
  public Source buildSource(String foreignSource, String name) {
    return Source.newBuilder(ID, this + "#" + foreignSource, name).build();
  }
}
