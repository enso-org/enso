package org.enso.persistance;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.function.Function;

import org.enso.persistance.PerInputImpl.InputCache;
import static org.enso.persistance.PerUtils.raise;

public abstract class Persistance<T> {
  final Class<T> clazz;
  final boolean includingSubclasses;
  final int id;

  protected Persistance(Class<T> clazz, boolean includingSubclasses, int id) {
    this.clazz = clazz;
    this.includingSubclasses = includingSubclasses;
    this.id = id;
  }

  protected abstract void writeObject(T obj, Output out) throws IOException;
  protected abstract T readObject(Input in) throws IOException, ClassNotFoundException;

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Persistance{");
    sb.append("clazz=").append(clazz.getName());
    sb.append(", id=").append(id);
    sb.append('}');
    return sb.toString();
  }

  public static interface Output extends DataOutput {
    public abstract <T> void writeInline(Class<T> clazz, T obj) throws IOException;
    public abstract void writeObject(Object obj) throws IOException;
  }

  public static interface Input extends DataInput {
    public abstract <T> T readInline(Class<T> clazz) throws IOException;
    public abstract Object readObject() throws IOException;
    public <T> Reference<T> readReference(Class<T> clazz) throws IOException;
  }

  final void writeInline(Object obj, Output out) throws IOException {
    writeObject(clazz.cast(obj), out);
  }

  final T readWith(Input in) {
    try {
      return readObject(in);
    } catch (IOException | ClassNotFoundException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  public static <T> Reference<T> readObject(byte[] arr, Function<Object, Object> readResolve) throws IOException {
    return PerInputImpl.readObject(arr, readResolve);
  }

  public static byte[] writeObject(Object obj, Function<Object, Object> writeReplace) throws IOException {
    return PerGenerator.writeObject(obj, writeReplace);
  }

  public static sealed abstract class Reference<T> permits PerBufferReference, PerMemoryReference {
    Reference() {
    }

    @SuppressWarnings("unchecked")
    public static final <T> Reference<T> none() {
      return (Reference<T>) PerMemoryReference.NULL;
    }

    public <V> V get(Class<V> expectedType) {
      var value = switch (this) {
        case PerMemoryReference m -> m.value();
        case PerBufferReference<T> b -> b.readObject(expectedType);
      };
      return expectedType.cast(value);
    }

    public static <V> Reference<V> of(V obj) {
      return new PerMemoryReference<>(obj);
    }
  }
}
