package org.enso.table.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import org.enso.base.text.CaseInsensitiveUnicodeNormalizedTextEquivalence;
import org.enso.base.text.UnicodeNormalizedTextEquivalence;
import org.enso.table.data.table.Column;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.problems.DuplicateNames;
import org.enso.table.util.problems.InvalidNames;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.EconomicSet;
import org.graalvm.collections.Equivalence;
import org.graalvm.collections.Pair;

public class NameDeduplicator {

  /**
   * Creates an instance that will not report any problems.
   *
   * <p>It may be useful when problem handling is done manually (e.g. Unique_Name_Strategy), or the
   * problems should just be ignored.
   */
  public static NameDeduplicator createIgnoringProblems(NamingProperties namingProperties) {
    return new NameDeduplicator(namingProperties, BlackholeProblemAggregator.INSTANCE);
  }

  public static NameDeduplicator createIgnoringProblems() {
    return new NameDeduplicator(BlackholeProblemAggregator.INSTANCE);
  }

  public static NameDeduplicator createDefault(ProblemAggregator problemAggregator) {
    return new NameDeduplicator(problemAggregator);
  }

  private final EconomicSet<String> usedNames;
  private final List<String> invalidNames = new ArrayList<>();
  private final EconomicMap<String, String> truncatedNames;
  private final List<String> duplicatedNames = new ArrayList<>();

  private final String invalidNameReplacement;
  private final NamingProperties namingProperties;

  private NameDeduplicator(ProblemAggregator parentAggregator) {
    this("Column", new DefaultNamingProperties(), parentAggregator);
  }

  private NameDeduplicator(NamingProperties namingProperties, ProblemAggregator parentAggregator) {
    this("Column", namingProperties, parentAggregator);
  }

  private NameDeduplicator(
      String invalidNameReplacement,
      NamingProperties namingProperties,
      ProblemAggregator parentAggregator) {
    // We just create the problem aggregator, which registers it in the hierarchy and will ensure
    // the problems are
    // summarized when the time comes. We don't need to store a reference to it.
    new NamingProblemsSummarizingProblemAggregator(parentAggregator);

    this.namingProperties = namingProperties;
    if (hasSizeLimit()) {
      if (namingProperties.encoded_size(invalidNameReplacement) > namingProperties.size_limit()) {
        invalidNameReplacement =
            namingProperties.truncate(invalidNameReplacement, namingProperties.size_limit());
        if (invalidNameReplacement.isEmpty()) {
          throw new IllegalStateException(
              "The size limit of naming properties ("
                  + namingProperties.size_limit()
                  + ") is too small to fit the invalid name replacement in NameDeduplicator.");
        }
      }
    }

    this.invalidNameReplacement = invalidNameReplacement;

    Equivalence nameEquivalence;
    if (namingProperties.is_case_sensitive()) {
      nameEquivalence = UnicodeNormalizedTextEquivalence.INSTANCE;
    } else {
      nameEquivalence = new CaseInsensitiveUnicodeNormalizedTextEquivalence(Locale.ROOT);
    }
    usedNames = EconomicSet.create(nameEquivalence);
    truncatedNames = EconomicMap.create(nameEquivalence);
  }

  public String makeValidAndTruncate(String input) {
    if (!Column.isColumnNameValid(input)) {
      this.invalidNames.add(input);
      input = this.invalidNameReplacement;
    }

    if (input.indexOf('\0') >= 0) {
      this.invalidNames.add(input);
      input = this.invalidNameReplacement;
    }

    if (hasSizeLimit()) {
      long encodedSize = namingProperties.encoded_size(input);
      if (encodedSize > namingProperties.size_limit()) {
        String truncated = namingProperties.truncate(input, namingProperties.size_limit());
        if (truncated.isEmpty()) {
          // This is a very rare edge case, but with a low (but still >0) limit, we can get an empty
          // string after
          // truncation if it was using non-ASCII characters for example. In this case, we use the
          // replacement and
          // re-truncate.
          truncated =
              namingProperties.truncate(this.invalidNameReplacement, namingProperties.size_limit());
        }

        this.truncatedNames.put(input, truncated);
        input = truncated;
      }
    }

    return input;
  }

  public List<String> makeUniqueList(List<String> names) {
    return names.stream().map(this::makeUnique).collect(Collectors.toList());
  }

  public String[] makeUniqueArray(String[] names) {
    return Arrays.stream(names).map(this::makeUnique).toArray(String[]::new);
  }

  public void markUsed(String name) {
    usedNames.add(name);
  }

  /**
   * Makes a name unique.
   *
   * <p>If a name has been used, it will suffixed with `_n` where n is the first integer greater
   * than or equal to 1 such that the name is unique.
   *
   * <p>An invalid name will be replaced with {@code invalidNameReplacement}. The suffix will always
   * be added to invalid names. For example: Column_1.
   *
   * @param name input name to make unique.
   * @return unique name following above rules.
   */
  public String makeUnique(String name) {
    String validName = makeValidAndTruncate(name);
    boolean wasValid = Column.isColumnNameValid(name);
    boolean wasTruncated = truncatedNames.containsKey(name);

    // Only if the name was invalid and it was replaced with the replacement sequence, we start with
    // the suffix.
    // Otherwise (if th name was truncated only), the first attempt is done withot suffix.
    int currentIndex = wasValid ? 0 : 1;

    NameIterator nameIterator = new NameIterator(validName, wasTruncated);

    String currentName = nameIterator.getName(currentIndex);
    while (usedNames.contains(currentName)) {
      if (currentIndex == 0) {
        duplicatedNames.add(name);
      }
      currentIndex++;
      currentName = nameIterator.getName(currentIndex);
    }

    usedNames.add(currentName);

    if (nameIterator.wasLastCallTruncated()) {
      assert namingProperties.encoded_size(currentName) <= namingProperties.size_limit();
      truncatedNames.put(name, currentName);
    }

    return currentName;
  }

  public boolean isUnique(String name) {
    return !usedNames.contains(name);
  }

  private class NameIterator {
    private final String initialName;
    private boolean wasLastGeneratedNameTruncated;

    NameIterator(String initialName, boolean isTruncated) {
      this.initialName = initialName;
      wasLastGeneratedNameTruncated = isTruncated;
    }

    String getName(int index) {
      if (index == 0) {
        return initialName;
      }

      String suffix = " " + index;
      if (hasSizeLimit()) {
        long sizeLimit = namingProperties.size_limit();
        long prefixSize = namingProperties.encoded_size(initialName);
        long suffixSize = namingProperties.encoded_size(suffix);

        if (suffixSize > sizeLimit) {
          throw new IllegalArgumentException(
              "When trying to generate a unique name based on `"
                  + initialName
                  + "`, "
                  + "the suffix length `"
                  + suffix
                  + "` has exceeded the maximum name size. There are too many taken names "
                  + "and the algorithm is unable to generate a fresh name fitting the limit.");
        }

        if (prefixSize + suffixSize > sizeLimit) {
          wasLastGeneratedNameTruncated = true;
          long maxPrefixSize = namingProperties.size_limit() - suffixSize;
          return namingProperties.truncate(initialName, maxPrefixSize) + suffix;
        }
      }

      return initialName + suffix;
    }

    boolean wasLastCallTruncated() {
      return wasLastGeneratedNameTruncated;
    }
  }

  public String[] getInvalidNames() {
    return this.invalidNames.toArray(String[]::new);
  }

  public String[] getDuplicatedNames() {
    return this.duplicatedNames.toArray(String[]::new);
  }

  public List<Pair<String, String>> getTruncatedNames() {
    ArrayList<Pair<String, String>> output = new ArrayList<>(truncatedNames.size());
    var cursor = truncatedNames.getEntries();
    while (cursor.advance()) {
      output.add(Pair.create(cursor.getKey(), cursor.getValue()));
    }
    return output;
  }

  /**
   * Changes names from the second list so that they do not clash with names from the first list and
   * with each other.
   */
  public List<String> combineWithPrefix(
      List<String> first, List<String> second, String secondPrefix) {
    first.forEach(this::markUsed);
    ArrayList<String> output = new ArrayList<>(second.size());
    // First pass - we add only the names that are already unique, and mark them as used in
    // preparation for the second pass.
    for (String name : second) {
      name = makeValidAndTruncate(name);
      if (isUnique(name)) {
        output.add(name);
        markUsed(name);
      } else {
        output.add(null);
      }
    }

    // Second pass - we go over the duplicated names and disambiguate them by adding a prefix and
    // a suffix if necessary.
    for (int i = 0; i < second.size(); i++) {
      String name = second.get(i);
      if (output.get(i) == null) {
        var prefixed = secondPrefix + name;
        output.set(i, makeUnique(prefixed));
      }
    }
    return output;
  }

  private boolean hasSizeLimit() {
    return namingProperties != null && namingProperties.size_limit() != null;
  }

  private class NamingProblemsSummarizingProblemAggregator extends ProblemAggregator {

    protected NamingProblemsSummarizingProblemAggregator(ProblemAggregator parent) {
      super(parent);
    }

    @Override
    public ProblemSummary summarize() {
      var summary = super.summarize();
      if (!invalidNames.isEmpty()) {
        summary.add(new InvalidNames(getInvalidNames()));
      }
      if (!duplicatedNames.isEmpty()) {
        summary.add(new DuplicateNames(getDuplicatedNames()));
      }
      return summary;
    }
  }

  private static class DefaultNamingProperties implements NamingProperties {

    @Override
    public Long size_limit() {
      return null;
    }

    @Override
    public long encoded_size(String name) {
      throw new IllegalStateException(
          "`DefaultNamingProperties.encoded_size` called but no limit is set.");
    }

    @Override
    public String truncate(String name, long max_encoded_size) {
      throw new IllegalStateException(
          "`DefaultNamingProperties.encoded_size` called but no limit is set.");
    }

    @Override
    public boolean is_case_sensitive() {
      return true;
    }
  }
}
