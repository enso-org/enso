package org.enso.compiler.core.ir;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import org.enso.persist.Persistable;

/**
 * Explains a reason why a type ascription is provided to an IR element. Used by {@link
 * Type.Ascription}.
 */
@Persistable(id = 11258, allowInlining = true)
public final class AscriptionReason {
  private static final AscriptionReason EMPTY = new AscriptionReason(0);
  private final List<Object> args;
  private final byte type;

  /** serde constructor */
  AscriptionReason(byte type, List<Object> args) {
    this.type = (byte) type;
    this.args = args;
  }

  private AscriptionReason(int type, Object... args) {
    this((byte) type, Arrays.asList(args));
    assert this.type == type;
  }

  final byte type() {
    return type;
  }

  final List<Object> args() {
    return args;
  }

  /**
   * Human readable explanation of the type check. Usually printed when the check fails.
   *
   * @return textual message representing the reason of the check or {@code null} for {@link
   *     #empty()}
   */
  public String comment() {
    return switch (type) {
      case 1 -> "`%s`".formatted(args.toArray());
      case 2 -> "the result of `%s`".formatted(args.toArray());
      default -> null;
    };
  }

  /**
   * No reason, no explanation.
   *
   * @return instance of "empty" reason.
   */
  public static AscriptionReason empty() {
    return EMPTY;
  }

  /**
   * Check of a function parameter type.
   *
   * @param name parameter name
   * @return new ascription
   */
  public static AscriptionReason forParameter(String name) {
    return new AscriptionReason(1, name);
  }

  /**
   * Type check on a function result. The {@code -> Integer} syntax.
   *
   * @param fnName
   * @return new ascription
   */
  public static AscriptionReason forFunctionResult(String fnName) {
    return new AscriptionReason(2, fnName);
  }

  /**
   * Should such a type check check for all types including hidden ones or not. Neither {@link
   * #forParameter} nor {@link #forFunctionResult} check hidden types.
   *
   * @return {@code true} to check also hidden types
   */
  public boolean isAllTypes() {
    return switch (type) {
      case 1, 2 -> false;
      default -> true;
    };
  }

  @Override
  public int hashCode() {
    int hash = 5;
    hash = 97 * hash + Objects.hashCode(this.args);
    hash = 97 * hash + this.type;
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final AscriptionReason other = (AscriptionReason) obj;
    if (this.type != other.type) {
      return false;
    }
    return Objects.equals(this.args, other.args);
  }
}
