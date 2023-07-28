package org.enso.benchmarks;

import java.util.List;
import java.util.Optional;
import org.graalvm.polyglot.Value;

public final class BenchSuiteWrapper {
  private final BenchSuite suite;
  private final String moduleQualifiedName;

  public BenchSuiteWrapper(BenchSuite suite, String moduleQualifiedName) {
    this.suite = suite;
    this.moduleQualifiedName = moduleQualifiedName;
  }

  public List<BenchGroup> getGroups() {
    return suite.groups();
  }

  public String getModuleQualifiedName() {
    return moduleQualifiedName;
  }

  public Value getDefaultInputArgument() {
    return Value.asValue(null);
  }

  public BenchGroup findGroupByName(String groupName) {
    return suite
        .groups()
        .stream()
        .filter(grp -> grp.name().equals(groupName))
        .findFirst()
        .orElse(null);
  }

  public BenchSpec findSpecByName(String groupName, String specName) {
    Optional<BenchGroup> group = suite
        .groups()
        .stream()
        .filter(grp -> grp.name().equals(groupName))
        .findFirst();
    if (group.isPresent()) {
      return group.get().specs()
          .stream()
          .filter(spec -> spec.name().equals(specName))
          .findFirst()
          .orElseGet(() -> null);
    }
    return null;
  }
}
