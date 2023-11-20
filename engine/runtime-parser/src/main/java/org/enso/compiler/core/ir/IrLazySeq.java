package org.enso.compiler.core.ir;

import java.util.NoSuchElementException;
import org.enso.persist.Persistance;
import scala.collection.Iterator;
import scala.collection.SeqFactory;
import scala.collection.immutable.AbstractSeq;

final class IrLazySeq extends AbstractSeq {
  private final Persistance.Reference<?>[] arr;
  private final int size;

  IrLazySeq(Persistance.Reference<?>[] arr, int size) {
    this.arr = arr;
    this.size = size;
  }

  @Override
  public Object apply(int i) throws IndexOutOfBoundsException {
    return arr[i].get(Object.class);
  }

  @Override
  public int length() {
    return size;
  }

  @Override
  public boolean isDefinedAt(int idx) {
    return 0 <= idx && idx < size;
  }

  @Override
  public boolean isDefinedAt(Object idx) {
    throw new IllegalStateException();
  }

  @Override
  public Object apply(Object i) throws IndexOutOfBoundsException {
    throw new IllegalStateException();
  }

  @Override
  public SeqFactory iterableFactory() {
    return super.iterableFactory();
  }

  @Override
  public Iterator iterator() {
    return new IrIter();
  }

  private final class IrIter implements Iterator {
    private int at;

    @Override
    public boolean hasNext() {
      return at < size;
    }

    @Override
    public Object next() throws NoSuchElementException {
      if (at >= size) {
        throw new NoSuchElementException();
      }
      return apply(at++);
    }
  }
}
