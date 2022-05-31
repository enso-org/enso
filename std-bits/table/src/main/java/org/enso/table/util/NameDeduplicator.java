package org.enso.table.util;

import org.enso.table.problems.Problem;
import org.enso.table.util.problems.DuplicateNames;
import org.enso.table.util.problems.InvalidNames;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class NameDeduplicator {
  private final Set<String> usedNames = new HashSet<>();
  private final List<String> invalidNames = new ArrayList<>();
  private final List<String> duplicatedNames = new ArrayList<>();

  public String makeValid(String input) {
    if (input == null || input.isEmpty()) {
      this.invalidNames.add(input);
      return "Column";
    }

    return input;
  }

  public List<String> makeUnique(List<String> names) {
    return names.stream().map(this::makeUnique).collect(Collectors.toList());
  }

  public String makeUnique(String name) {
    String validName = makeValid(name);
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

  private static String getName(String name, int index) {
    if (index == 0) {
      return name;
    }
    return name + "_" + index;
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
}
