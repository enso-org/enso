package org.enso.refactoring.validation;

public final class MethodNameValidation {

  public static final String DEFAULT_NAME = "operator";

  private static final char CHAR_UNDERSCORE = '_';
  private static final char CHAR_LOWERCASE_A = 'a';
  private static final char CHAR_LOWERCASE_Z = 'z';
  private static final char CHAR_UPPERCASE_A = 'A';
  private static final char CHAR_UPPERCASE_Z = 'Z';

  /**
   * Normalize the name to make it a valid identifier of a method.
   *
   * @param name the name to normalize.
   * @return the normalized name.
   */
  public static String normalize(String name) {
    var normalizedName = toLowerSnakeCase(name);
    if (normalizedName.isEmpty()) {
      return DEFAULT_NAME;
    } else {
      return normalizedName;
    }
  }

  /**
   * @return {@code true} if the provided name is a valid identifier of a method and {@code false}
   *     otherwise.
   */
  public static boolean isAllowedName(String name) {
    return !name.isEmpty()
        && isAllowedFirstCharacter(name.charAt(0))
        && (name.charAt(0) != CHAR_UNDERSCORE || name.length() > 1)
        && name.chars().allMatch(MethodNameValidation::isAllowedNameCharacter);
  }

  /**
   * Convert the provided name to the lower snake case format removing all unsupported characters.
   *
   * @param name the provided name that will be converted to snake case
   * @return the name converted to snake case
   */
  private static String toLowerSnakeCase(String name) {
    if (name == null || name.isEmpty()) {
      return "";
    }

    StringBuilder result = new StringBuilder();
    boolean lastWasUpper = false;

    for (int i = 0; i < name.length(); i++) {
      char c = name.charAt(i);
      if (Character.isLetter(c)) {
        if (Character.isUpperCase(c)) {
          if (result.length() > 0 && !lastWasUpper) {
            result.append(CHAR_UNDERSCORE);
          }
          result.append(Character.toLowerCase(c));
          lastWasUpper = true;
        } else {
          result.append(c);
          lastWasUpper = false;
        }
      } else if (Character.isDigit(c) && i == 0) {
        result.append(CHAR_UNDERSCORE);
        result.append(c);
        lastWasUpper = false;
      } else if (isAllowedNameCharacter(c)) {
        result.append(c);
        lastWasUpper = false;
      } else if (c == ' ') {
        result.append(CHAR_UNDERSCORE);
        lastWasUpper = false;
      }
    }

    // Replace multiple underscores with a single underscore
    String intermediateResult = result.toString().replaceAll("__+", "_");

    // Remove trailing underscores
    return intermediateResult.replaceAll("_$", "");
  }

  private static boolean isAllowedFirstCharacter(int c) {
    return isLowerCaseAscii(c) || c == CHAR_UNDERSCORE;
  }

  private static boolean isAllowedNameCharacter(int c) {
    return isAlphanumericAscii(c) || c == CHAR_UNDERSCORE;
  }

  private static boolean isAlphanumericAscii(int c) {
    return isLowerCaseAscii(c) || Character.isDigit(c);
  }

  private static boolean isLowerCaseAscii(int c) {
    return c >= CHAR_LOWERCASE_A && c <= CHAR_LOWERCASE_Z;
  }
}
