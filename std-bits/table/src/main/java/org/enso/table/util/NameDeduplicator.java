package org.enso.table.util;

import org.enso.table.problems.Problem;
import org.enso.table.util.problems.DuplicateNames;
import org.enso.table.util.problems.InvalidNames;

import java.util.*;
import java.util.stream.Collectors;

public class NameDeduplicator {
  private final Set<String> usedNames = new HashSet<>();
  private final List<String> invalidNames = new ArrayList<>();
  private final List<String> duplicatedNames = new ArrayList<>();

  private final String invalidNameReplacement;

  public NameDeduplicator() {
    this("Column");
  }

  public NameDeduplicator(String invalidNameReplacement) {
    this.invalidNameReplacement = invalidNameReplacement;
  }

  public String makeValid(String input) {
    if (input == null || input.isEmpty()) {
      this.invalidNames.add(input);
      return this.invalidNameReplacement;
    }

    if (input.indexOf('\0') >= 0) {
      this.invalidNames.add(input);
      return this.invalidNameReplacement;
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
    String validName = makeValid(name);

    // If an invalid name then starts as `Column_1`.
    int currentIndex = validName.equals(name) ? 0 : 1;

    String currentName = getName(validName, currentIndex);
    while (usedNames.contains(currentName)) {
      if (currentIndex == 0) {
        duplicatedNames.add(name);
      }
      currentIndex++;
      currentName = getName(validName, currentIndex);
    }

    usedNames.add(currentName);
    return currentName;
  }

  public boolean isUnique(String name) {
    return !usedNames.contains(name);
  }

  private static String getName(String name, int index) {
    if (index == 0) {
      return name;
    }
    return name + " " + index;
  }

  public String[] getInvalidNames() {
    return this.invalidNames.toArray(String[]::new);
  }

  public String[] getDuplicatedNames() {
    return this.duplicatedNames.toArray(String[]::new);
  }

  public List<Problem> getProblems() {
    List<Problem> output = new ArrayList<>(2);
    if (!this.invalidNames.isEmpty()) {
      output.add(new InvalidNames(this.getInvalidNames()));
    }
    if (!this.duplicatedNames.isEmpty()) {
      output.add(new DuplicateNames(this.getDuplicatedNames()));
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
}
