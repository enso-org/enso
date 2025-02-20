package org.enso.base.parser;

import java.util.Optional;

public enum NegativeSign {
  /** No sign encountered, so could be either. */
  UNKNOWN,
  /** Minus or Plus sign - e.g. +123 or -123. */
  MINUS,
  /** Brackets - e.g. (123) */
  BRACKET_OPEN;

  /**
   * Checks if the given character is a valid negative sign.
   *
   * @param c the character to check
   * @return the new state of the negative sign or Optional.empty if the character is invalid.
   */
  public Optional<NegativeSign> checkValid(char c) {
    var result =
        switch (this) {
          case UNKNOWN -> c == '-' || c == '+' ? MINUS : c == '(' ? BRACKET_OPEN : null;
          case MINUS -> c == '(' ? null : this;
          case BRACKET_OPEN -> c != '(' ? null : this;
        };
    return Optional.ofNullable(result);
  }

  static boolean isOpenSign(char c) {
    return c == '-' || c == '+' || c == '(';
  }

  static boolean isCloseSign(char c) {
    return c == ')';
  }

  static boolean isSign(char c) {
    return isOpenSign(c) || isCloseSign(c);
  }
}
