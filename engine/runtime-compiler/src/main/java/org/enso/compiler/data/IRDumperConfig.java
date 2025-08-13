package org.enso.compiler.data;

public final class IRDumperConfig {
  public enum DumpLevel {
    DEFAULT(),
    NO_MINI_PASS_CHAINING();

    private static DumpLevel fromInt(int level) {
      return switch (level) {
        case 1 -> DEFAULT;
        case 2 -> NO_MINI_PASS_CHAINING;
        default -> throw new IllegalArgumentException("Unknown dump level: " + level);
      };
    }
  }

  private final String moduleName;
  private final DumpLevel dumpLevel;

  private IRDumperConfig(String moduleName, DumpLevel dumpLevel) {
    this.moduleName = moduleName;
    this.dumpLevel = dumpLevel;
  }

  /** Shortcut for {@link #forModuleName(String, DumpLevel)}. */
  public static IRDumperConfig forModuleName(String moduleName) {
    return forModuleName(moduleName, DumpLevel.DEFAULT);
  }

  /**
   * @param moduleName Substring of the module name to dump.
   */
  public static IRDumperConfig forModuleName(String moduleName, DumpLevel dumpLevel) {
    return new IRDumperConfig(moduleName, dumpLevel);
  }

  public static IRDumperConfig parseFromProperty(String prop) {
    if (prop != null) {
      if (prop.contains(":")) {
        var items = prop.split(":", 2);
        var modName = items[0];
        var level = items[1].trim();
        int numLevel;
        try {
          numLevel = Integer.parseInt(level);
        } catch (NumberFormatException e) {
          throw new IllegalArgumentException(
              "Invalid dump level: " + level + " for module: " + modName, e);
        }
        var dumpLevel = DumpLevel.fromInt(numLevel);
        return new IRDumperConfig(modName, dumpLevel);
      } else {
        return new IRDumperConfig(prop, DumpLevel.DEFAULT);
      }
    }
    return null;
  }

  public String getModuleName() {
    return moduleName;
  }

  public DumpLevel getDumpLevel() {
    return dumpLevel;
  }
}
