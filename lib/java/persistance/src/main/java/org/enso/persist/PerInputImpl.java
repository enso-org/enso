package org.enso.persist;

import static org.enso.persist.PerGenerator.INLINED_REFERENCE_ID;
import static org.enso.persist.PerGenerator.NULL_REFERENCE_ID;
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

  static Reference<?> readObject(ByteBuffer buf, Function<Object, Object> readResolve)
      throws IOException {
    for (var i = 0; i < PerGenerator.HEADER.length; i++) {
      if (buf.get(i) != PerGenerator.HEADER[i]) {
        throw new IOException("Wrong header");
      }
    }
    var version = buf.getInt(4);
    var map = PerMap.create();
    if (version != map.versionStamp) {
      throw new IOException(
          "Incompatible version "
              + Integer.toHexString(version)
              + " != "
              + Integer.toHexString(map.versionStamp));
    }

    var tableAt = buf.getInt(8);
    buf.position(tableAt);
    var rawRefMap = InputCache.RawReferenceMap.readFromBuffer(buf);
    var cache = new InputCache(buf, readResolve, map, rawRefMap);
    return cache.getRef(0);
  }

  static Persistance.Reference<?> findReference(Persistance.Input input, int refId) {
    if (refId == NULL_REFERENCE_ID) {
      return Persistance.Reference.none();
    }
    if (refId != INLINED_REFERENCE_ID) {
      var impl = (PerInputImpl) input;
      var ref = impl.cache.getRef(refId);
      return ref;
    }
    return null;
  }

  @Override
  public <T> T readInline(Class<T> clazz) throws IOException {
    Persistance<T> p = cache.map().forType(clazz);
    T res = p.readWith(this);
    var resolve = cache.resolveObject(res);
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

    if (cache.getObjectAt(at) instanceof Object res) {
      return p.clazz.cast(res);
    }

    var inData = new PerInputImpl(cache, at);
    var res = p.readWith(inData);
    res = cache.resolveObject(res);
    var prev = cache.putObjectAt(at, res);
    if (prev != null) {
      var bothObjectsAreTheSame = gentlyEquals(res, prev);
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

  @SuppressWarnings("unchecked")
  private static boolean gentlyEquals(Object o1, Object o2) {
    return Objects.equals(o1, o2);
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
      return Reference.none();
    }
    var id = in.readInt();
    var p = map.forId(id);
    if (clazz.isAssignableFrom(p.clazz)) {
      return PerBufferReference.from((Persistance<T>) p, buffer, at);
    } else {
      throw new IOException("Expecting " + clazz.getName() + " but found " + p.clazz.getName());
    }
  }

  static final class InputCache {
    static class RawReferenceMap {
      private final int[] refIds;
      private final int[] refPerIds;

      private RawReferenceMap(int[] refIds, int[] refPerIds) {
        this.refIds = refIds;
        this.refPerIds = refPerIds;
      }

      static RawReferenceMap readFromBuffer(ByteBuffer buf) {
        var count = buf.getInt();
        assert count > 0 : "There is always the main object in the reference table: " + count;
        var refPerIds = new int[count];
        var refIds = new int[count];
        for (var i = 0; i < count; i++) {
          int refId = buf.getInt();
          if (refId == NULL_REFERENCE_ID) {
            // If this was a null reference, we don't read another integer - as its type id was not
            // stored.
            refIds[i] = NULL_REFERENCE_ID;
          } else {
            refIds[i] = refId;
            refPerIds[i] = buf.getInt();
          }
        }
        return new RawReferenceMap(refIds, refPerIds);
      }

      Reference<?>[] intoReferences(PerMap map, InputCache inputCache) {
        var refs = new Reference[refIds.length];
        for (var i = 0; i < refIds.length; i++) {
          if (refIds[i] == NULL_REFERENCE_ID) {
            refs[i] = Persistance.Reference.none();
          } else {
            var p = map.forId(refPerIds[i]);
            refs[i] = PerBufferReference.cached(p, inputCache, refIds[i]);
          }
        }
        return refs;
      }
    }

    private final Map<Integer, Object> cache = new HashMap<>();
    private final Function<Object, Object> readResolve;
    private final PerMap map;
    private final ByteBuffer buf;
    private final Reference<?>[] refs;

    private InputCache(
        ByteBuffer buf,
        Function<Object, Object> readResolve,
        PerMap map,
        RawReferenceMap rawReferenceMap) {
      this.buf = buf;
      this.readResolve = readResolve;
      this.map = map;
      this.refs = rawReferenceMap.intoReferences(map, this);
    }

    final Object resolveObject(Object res) {
      if (readResolve != null) {
        return readResolve.apply(res);
      } else {
        return res;
      }
    }

    final Object getObjectAt(int at) {
      return cache.get(at);
    }

    final Object putObjectAt(int at, Object obj) {
      return cache.put(at, obj);
    }

    final PerMap map() {
      return map;
    }

    final ByteBuffer buf() {
      return buf;
    }

    final Reference<?> getRef(int index) {
      return refs[index];
    }
  }
}
