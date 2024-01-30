package org.enso.persist;

import static org.enso.persist.PerUtils.raise;

import java.io.DataInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import org.enso.persist.Persistance.Input;
import org.enso.persist.Persistance.Reference;

final class PerInputImpl implements Input {
  private final InputCache cache;
  private final ByteBuffer buf;
  private int at;

  PerInputImpl(InputCache cache, int at) {
    this.cache = cache;
    this.buf = cache.buf();
    this.at = at;
  }

  static <T> Reference<T> readObject(byte[] arr, Function<Object, Object> readResolve)
      throws IOException {
    for (var i = 0; i < PerGenerator.HEADER.length; i++) {
      if (arr[i] != PerGenerator.HEADER[i]) {
        throw new IOException("Wrong header");
      }
    }
    var buf = ByteBuffer.wrap(arr);
    var version = buf.getInt(4);
    var cache = new InputCache(buf, readResolve);
    if (version != cache.map().versionStamp) {
      throw new IOException(
          "Incompatible version "
              + Integer.toHexString(version)
              + " != "
              + Integer.toHexString(cache.map().versionStamp));
    }
    var at = buf.getInt(8);

    return PerBufferReference.from(cache, at);
  }

  @Override
  public <T> T readInline(Class<T> clazz) {
    Persistance<T> p = cache.map().forType(clazz);
    T res = p.readWith(this);
    var resolve = cache.readResolve().apply(res);
    return clazz.cast(resolve);
  }

  @Override
  public Object readObject() throws IOException {
    var obj = readIndirect(cache, cache.map(), this);
    return obj;
  }

  @Override
  public <T> Persistance.Reference<T> readReference(Class<T> clazz) throws IOException {
    var obj = readIndirectAsReference(cache, cache.map(), this, clazz);
    return obj;
  }

  public int read() throws IOException {
    return buf.get(at++);
  }

  public int read(byte[] b) throws IOException {
    buf.get(at, b);
    at += b.length;
    return b.length;
  }

  public int read(byte[] b, int off, int len) throws IOException {
    buf.get(at, b, off, len);
    at += len;
    return len;
  }

  public long skip(long n) throws IOException {
    at += Math.toIntExact(n);
    return at;
  }

  public int available() throws IOException {
    return buf.limit() - at;
  }

  public void close() throws IOException {}

  @Override
  public void readFully(byte[] b) throws IOException {
    read(b);
  }

  @Override
  public void readFully(byte[] b, int off, int len) throws IOException {
    read(b, off, len);
  }

  @Override
  public int skipBytes(int n) throws IOException {
    at += n;
    return at;
  }

  @Override
  public boolean readBoolean() throws IOException {
    int b = read();
    return b != 0;
  }

  @Override
  public byte readByte() throws IOException {
    return buf.get(at++);
  }

  @Override
  public int readUnsignedByte() throws IOException {
    byte b = readByte();
    return b >= 0 ? b : b + 256;
  }

  @Override
  public short readShort() throws IOException {
    short s = buf.getShort(at);
    at += 2;
    return s;
  }

  @Override
  public int readUnsignedShort() throws IOException {
    short s = readShort();
    return s >= 0 ? s : s + 256 * 256;
  }

  @Override
  public char readChar() throws IOException {
    return (char) readShort();
  }

  @Override
  public int readInt() throws IOException {
    int i = buf.getInt(at);
    at += 4;
    return i;
  }

  @Override
  public long readLong() throws IOException {
    long l = buf.getLong(at);
    at += 8;
    return l;
  }

  @Override
  public float readFloat() throws IOException {
    float f = buf.getFloat(at);
    at += 4;
    return f;
  }

  @Override
  public double readDouble() throws IOException {
    double d = buf.getDouble(at);
    at += 8;
    return d;
  }

  @Override
  public String readLine() throws IOException {
    throw new IOException("No readLine in buffer");
  }

  @Override
  public String readUTF() throws IOException {
    return DataInputStream.readUTF(this);
  }

  @Override
  public String toString() {
    return "Input[at=" + at + " of " + this.buf.limit() + "]";
  }

  static Object readIndirect(InputCache cache, PerMap map, Input in) throws IOException {
    var at = in.readInt();
    if (at < 0) {
      return null;
    }
    var id = in.readInt();
    var p = map.forId(id);

    if (cache.cache().get(at) instanceof Object res) {
      return p.clazz.cast(res);
    }

    var inData = new PerInputImpl(cache, at);
    var res = p.readWith(inData);
    res = cache.readResolve().apply(res);
    var prev = cache.cache().put(at, res);
    if (prev != null) {
      var bothObjectsAreTheSame = Objects.equals(res, prev);
      var sb = new StringBuilder();
      sb.append("Adding at ").append(at).append(" object:\n  ");
      dumpObject(sb, res);
      sb.append("\nbut there already is:\n  ");
      dumpObject(sb, prev);
      sb.append("\nare they equal: ").append(bothObjectsAreTheSame);
      var ex = new IOException(sb.toString());
      if (bothObjectsAreTheSame) {
        PerUtils.LOG.warn(sb.toString(), ex);
      } else {
        throw raise(RuntimeException.class, ex);
      }
    }
    return res;
  }

  private static void dumpObject(StringBuilder sb, Object obj) {
    sb.append(obj.getClass().getName())
        .append("@")
        .append(Integer.toHexString(System.identityHashCode(obj)));
  }

  @SuppressWarnings("unchecked")
  static <T> Reference<T> readIndirectAsReference(
      InputCache buffer, PerMap map, Input in, Class<T> clazz) throws IOException {
    var at = in.readInt();
    if (at < 0) {
      return null;
    }
    var id = in.readInt();
    var p = map.forId(id);
    if (clazz.isAssignableFrom(p.clazz)) {
      return PerBufferReference.from((Persistance<T>) p, buffer, at);
    } else {
      throw new IOException("Expecting " + clazz.getName() + " but found " + p.clazz.getName());
    }
  }

  static final record InputCache(
      ByteBuffer buf,
      Function<Object, Object> readResolve,
      Map<Integer, Object> cache,
      PerMap map) {
    InputCache(ByteBuffer buf, Function<Object, Object> readResolve) {
      this(
          buf,
          readResolve == null ? Function.identity() : readResolve,
          new HashMap<>(),
          PerMap.create());
    }
  }
}
