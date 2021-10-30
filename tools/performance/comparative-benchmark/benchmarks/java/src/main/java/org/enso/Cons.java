package org.enso;

import java.util.function.BiFunction;

public class Cons {
    public Long item;
    public Cons next;

    public Cons(Long item, Cons next) {
      this.item = item;
      this.next = next;
    }

    public long sum() {
      long result = 0;
      Cons next = this;
      while (next != null) {
        result += next.item;
        next = next.next;
      }
      return result;
    }

    public Cons reverse() {
      Cons result = null;
      Cons current = this;
      while (current != null) {
        result = new Cons(current.item, result);
        current = current.next;
      }
      return result;
    }

    public <X> X fold(BiFunction<X, Long, X> fun, X acc) {
      Cons current = this;
      while (current != null) {
        acc = fun.apply(acc, current.item);
        current = current.next;
      }
      return acc;
    }

    public static Cons genList(long i) {
      Cons result = null;
      while (i-- > 0) {
        result = new Cons(i, result);
      }
      return result;
    }
  }
