package org.enso.syntax2;

public class Either<Left, Right> {
    protected Left left;
    protected Right right;
    protected Either(Left leftIn, Right rightIn) {
        left = leftIn;
        right = rightIn;
    }
    public static <L, R> Either<L, R> left(L left) {
        return new Either<>(left, null);
    }
    public static <L, R> Either<L, R> right(R right) {
        return new Either<>(null, right);
    }
}
