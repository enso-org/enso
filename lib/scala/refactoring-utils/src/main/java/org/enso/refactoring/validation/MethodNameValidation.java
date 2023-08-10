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
    if (name.isEmpty()) {
      return DEFAULT_NAME;
    }
    if (isAllowedFirstCharacter(Character.toLowerCase(name.charAt(0)))) {
      return toLowerSnakeCase(name);
    }
    return toLowerSnakeCase(DEFAULT_NAME + "_" + name);
  }

  /**
   * @return {@code true} if the provided name is a valid identifier of a method and {@code false}
   *     otherwise.
   */
  public static boolean isAllowedName(String name) {
    return !name.isEmpty()
        && isAllowedFirstCharacter(name.charAt(0))
        && name.chars().allMatch(MethodNameValidation::isAllowedNameCharacter);
  }

  private static String toLowerSnakeCase(String name) {
    if (name.isEmpty()) {
      return name;
    }

    StringBuilder result = new StringBuilder(name.length());
    char[] chars = name.toCharArray();
    char previous = name.charAt(0);
    for (int i = 0; i < chars.length; i++) {
      char current = name.charAt(i);

      if (current == CHAR_UNDERSCORE && previous == CHAR_UNDERSCORE) {
        continue;
      }

      if (isLetterAscii(current) || Character.isDigit(current) || current == CHAR_UNDERSCORE) {
        if (Character.isUpperCase(current)
            && (Character.isLowerCase(previous) || Character.isDigit(previous))) {
          result.append(CHAR_UNDERSCORE);
        }
        if (Character.isLowerCase(current) && Character.isDigit(previous)) {
          result.append(CHAR_UNDERSCORE);
        }
        result.append(Character.toLowerCase(current));
        previous = current;
      }

      if (Character.isWhitespace(current) && previous != CHAR_UNDERSCORE) {
        result.append(CHAR_UNDERSCORE);
        previous = CHAR_UNDERSCORE;
      }
    }

    char lastChar = result.charAt(result.length() - 1);
    if (lastChar == CHAR_UNDERSCORE) {
      result.setLength(result.length() - 1);
    }

    return result.toString();
  }

  private static boolean isAllowedFirstCharacter(int c) {
    return isLowerCaseAscii(c);
  }

  private static boolean isAllowedNameCharacter(int c) {
    return isAlphanumericAscii(c) || c == CHAR_UNDERSCORE;
  }

  private static boolean isAlphanumericAscii(int c) {
    return isLowerCaseAscii(c) || Character.isDigit(c);
  }

  private static boolean isLetterAscii(int c) {
    return isLowerCaseAscii(c) || isUpperCaseAscii(c);
  }

  private static boolean isLowerCaseAscii(int c) {
    return c >= CHAR_LOWERCASE_A && c <= CHAR_LOWERCASE_Z;
  }

  private static boolean isUpperCaseAscii(int c) {
    return c >= CHAR_UPPERCASE_A && c <= CHAR_UPPERCASE_Z;
  }
}
