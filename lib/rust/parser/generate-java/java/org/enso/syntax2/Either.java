package org.enso.syntax2;

public final class Either<Left, Right> {
  private final Left left;
  private final Right right;

  private Either(Left leftIn, Right rightIn) {
    left = leftIn;
    right = rightIn;
  }

  public Left getLeft() {
    return left;
  }

  public Right getRight() {
    return right;
  }

  @Override
  public String toString() {
    if (right == null) {
      return "Either{" + "left=" + left + '}';
    } else {
      return "Either{" + "right=" + right + '}';
    }
  }

  public static <L, R> Either<L, R> left(L left) {
    return new Either<>(left, null);
  }

  public static <L, R> Either<L, R> right(R right) {
    return new Either<>(null, right);
  }
}
