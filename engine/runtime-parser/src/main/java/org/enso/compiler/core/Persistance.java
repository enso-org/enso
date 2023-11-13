package org.enso.compiler.core;

import java.io.ByteArrayOutputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.ServiceLoader;

import org.enso.compiler.core.Persistance.Reference;

public abstract class Persistance<T> {
  private final Class<T> clazz;
  private final boolean includingSubclasses;
  private final int id;

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

    /**
    * Looks an associated service up. The service can be specified as
    * additional arguments to {@link #readObject(byte[], Object...)}.
    *
    * @param <T> type of the service requested
    * @param clazz the class representing the requested type
    * @return instance of the requested service or {@code null}
    */
    public abstract <T> T lookup(Class<T> clazz);
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

  public static <T> Reference<T> readObject(byte[] arr, Object... context) throws IOException {
    for (var i = 0; i < Generator.HEADER.length; i++) {
      if (arr[i] != Generator.HEADER[i]) {
        throw new IOException("Wrong header");
      }
    }
    var buf = ByteBuffer.wrap(arr);
    var at = buf.getInt(4);
    var cache = new InputCache(buf, context);

    return Reference.from(cache, at);
  }

  public static byte[] writeObject(Object obj) throws IOException {
    var out = new ByteArrayOutputStream();
    var data = new DataOutputStream(out);
    data.write(Generator.HEADER);
    data.writeInt(PersistanceMap.DEFAULT.versionStamp);
    data.write(new byte[4]); // space
    data.flush();
    var g = new Generator(out, 12);
    var at = g.writeObject(obj);
    var arr = out.toByteArray();
    arr[4] = (byte) ((at >> 24) & 0xff);
    arr[5] = (byte) ((at >> 16) & 0xff);
    arr[6] = (byte) ((at >> 8) & 0xff);
    arr[7] = (byte) (at & 0xff);
    return arr;
  }

  public static final class Generator {
    static final byte[] HEADER = new byte[] { 0x0a, 0x0d, 0x01, 0x0f };

    private final OutputStream main;
    private final Map<Object,Integer> knownObjects = new IdentityHashMap<>();
    private int position;

    private Generator(OutputStream out, int position) {
      this.main = out;
      this.position = position;
    }

    public <T> int writeObject(T obj) throws IOException {
      if (obj == null) {
        return -1;
      }
      var found = knownObjects.get(obj);
      if (found == null) {
        var p = PersistanceMap.DEFAULT.forType(obj.getClass());
        var os = new ByteArrayOutputStream();
        p.writeInline(obj, new ReferenceOutput(this, os));
        found = this.position;
        var arr = os.toByteArray();
        main.write(arr);
        this.position += arr.length;
        knownObjects.put(obj, found);
      }
      return found;
    }

    final void writeIndirect(Object obj, Output out) throws IOException {
      if (obj == null) {
        out.writeInt(-1);
        return;
      }
      var p = PersistanceMap.DEFAULT.forType(obj.getClass());
      var found = knownObjects.get(obj);
      if (found == null) {
        var os = new ByteArrayOutputStream();
        var osData = new ReferenceOutput(this, os);
        p.writeInline(obj, osData);
        found = position;
        if (os.size() == 0) {
          os.write(0);
        }
        var arr = os.toByteArray();
        main.write(arr);
        position += arr.length;
        knownObjects.put(obj, found);
      }
      out.writeInt(found);
      out.writeInt(p.id);
    }
  }

  static Object readIndirect(InputCache cache, PersistanceMap map, Input in) throws IOException {
    var at = in.readInt();
    if (at < 0) {
      return null;
    }
    var id = in.readInt();
    var p = map.forId(id);

    if (cache.cache.get(at) instanceof Object res) {
      return p.clazz.cast(res);
    }

    var inData = new InputImpl(cache, at);
    var res = p.readWith(inData);
    var prev = cache.cache.put(at, res);
    if (prev != null) {
      throw raise(RuntimeException.class, new IOException("Adding at " + at + " object: " + res.getClass().getName() + " but there already is " + prev.getClass().getName()));
    }
    return res;
  }

  @SuppressWarnings("unchecked")
  static <T> Reference<T> readIndirectAsReference(InputCache buffer, PersistanceMap map, Input in, Class<T> clazz) throws IOException {
    var at = in.readInt();
    if (at < 0) {
      return null;
    }
    var id = in.readInt();
    var p = map.forId(id);
    if (clazz.isAssignableFrom(p.clazz)) {
      return Reference.from((Persistance<T>)p, buffer, at);
    } else {
      throw new IOException("Expecting " + clazz.getName() + " but found " + p.clazz.getName());
    }
  }

  public static sealed abstract class Reference<T> {
    private static final Reference<?> NULL = new MemoryReference<>(null);

    private Reference() {
    }

    @SuppressWarnings("unchecked")
    public static final <T> Reference<T> none() {
      return (Reference<T>) NULL;
    }

    public <V> V get(Class<V> expectedType) {
      var value = switch (this) {
        case MemoryReference m -> m.value;
        case BufferReference<T> b -> b.readObject(expectedType);
      };
      return expectedType.cast(value);
    }

    public static <V> Reference<V> of(V obj) {
      return new MemoryReference<>(obj);
    }

    static <V> Reference<V> from(InputCache buffer, int offset) {
      return from(null, buffer, offset);
    }

    static <V> Reference<V> from(Persistance<V> p, InputCache buffer, int offset) {
      return new BufferReference<>(p, buffer, offset);
    }
  }


  private static final class BufferReference<T> extends Reference<T> {
    private final Persistance<T> p;
    private final InputCache cache;
    private final int offset;

    BufferReference(Persistance<T> p, InputCache buffer, int offset) {
      this.p = p;
      this.cache = buffer;
      this.offset = offset;
    }

    @SuppressWarnings("unchecked")
    final <T> T readObject(Class<T> clazz) {
      if (p != null) {
        if (clazz.isAssignableFrom(p.clazz)) {
          clazz = (Class) p.clazz;
        } else {
          throw new ClassCastException();
        }
      }
      var in = new InputImpl(cache, offset);
      var obj = in.readInline(clazz);
      return obj;
    }
  }

  private static final class MemoryReference<T> extends Reference<T> {
    private final T value;

    MemoryReference(T obj) {
      this.value = obj;
    }
  }

  private static final class ReferenceOutput extends DataOutputStream implements Output {
    private final Generator generator;

    ReferenceOutput(Generator g, ByteArrayOutputStream out) {
      super(out);
      this.generator = g;
    }

    @Override
    public <T> void writeInline(Class<T> clazz, T obj) throws IOException {
      var p = PersistanceMap.DEFAULT.forType(clazz);
      p.writeInline(obj, this);
    }

    @Override
    public void writeObject(Object obj) throws IOException {
      this.generator.writeIndirect(obj, this);
    }
  }

  private static final record InputCache(
    ByteBuffer buf, Object[] services, Map<Integer,Object> cache
  ) {
    InputCache(ByteBuffer buf, Object... services) {
      this(buf, services, new HashMap<>());
    }
  }

  private static final class InputImpl implements Input {
    private final InputCache cache;
    private final ByteBuffer buf;
    private int at;

    InputImpl(InputCache cache, int at) {
      this.cache = cache;
      this.buf = cache.buf();
      this.at = at;
    }

    @Override
    public <T> T lookup(Class<T> clazz) {
      if (clazz == Object.class) {
        return null;
      }
      for (var o : cache.services) {
        if (clazz.isInstance(o)) {
          return clazz.cast(o);
        }
      }
      return null;
    }

    @Override
    public <T> T readInline(Class<T> clazz) {
      var p = PersistanceMap.DEFAULT.forType(clazz);
      return p.readWith(this);
    }

    @Override
    public Object readObject() throws IOException {
      var obj = readIndirect(cache, PersistanceMap.DEFAULT, this);
      return obj;
    }

    @Override
    public <T> Reference<T> readReference(Class<T> clazz) throws IOException {
      var obj = readIndirectAsReference(cache, PersistanceMap.DEFAULT, this, clazz);
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

    public void close() throws IOException {
    }

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
      var b = readByte();
      return b >= 0 ? b : b + 256;
    }

    @Override
    public short readShort() throws IOException {
      var s = buf.getShort(at);
      at  += 2;
      return s;
    }

    @Override
    public int readUnsignedShort() throws IOException {
      var s = readShort();
      return s >= 0 ? s : s + 256 * 256;
    }

    @Override
    public char readChar() throws IOException {
      return (char) readShort();
    }

    @Override
    public int readInt() throws IOException {
      var i = buf.getInt(at);
      at += 4;
      return i;
    }

    @Override
    public long readLong() throws IOException {
      var l = buf.getLong(at);
      at += 8;
      return l;
    }

    @Override
    public float readFloat() throws IOException {
      var f = buf.getFloat(at);
      at += 4;
      return f;
    }

    @Override
    public double readDouble() throws IOException {
      var d = buf.getDouble(at);
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
  }

  private static final class PersistanceMap {
    private final static PersistanceMap DEFAULT = new PersistanceMap();
    private final Map<Integer, Persistance<?>> ids = new HashMap<>();
    private final Map<Class<?>, Persistance<?>> types = new HashMap<>();
    private final int versionStamp;

    private PersistanceMap() {
      var hash = 0;
      for (var p : ServiceLoader.load(Persistance.class, getClass().getClassLoader())) {
        var prevId = ids.put(p.id, p);
        if (prevId != null) {
          throw new IllegalStateException("Multiple registrations for ID " + p.id + " " + prevId + " != " + p);
        }
        hash += p.id;
        var prevType = types.put(p.clazz, p);
        if (prevType != null) {
          throw new IllegalStateException("Multiple registrations for " + p.clazz.getName() + " " + prevId + " != " + p);
        }
      }
      versionStamp = hash;
    }

    @SuppressWarnings("unchecked")
    private synchronized <T> Persistance<T> searchSupertype(String name, Class<T> type) {
      // synchronized as it mutes the types map
      // however over time the types map gets saturated and
      // the synchronization will get less frequent
      // please note that Persistance as well as Class (as a key) have all fields final =>
      // as soon as they become visible from other threads, they have to look consistent
      NOT_FOUND: if (type != null) {
        var p = types.get(type);
        if (p != null) {
          if (!p.includingSubclasses) {
            break NOT_FOUND;
          }
        } else {
          for (var in : type.getInterfaces()) {
            p = searchSupertype(name, in);
            if (p != null) {
              break;
            }
          }
          if (p == null && !type.isInterface()) {
            p = searchSupertype(name, type.getSuperclass());
          }
          types.put(type, p);
        }
        return (Persistance<T>) p;
      }
      if (type == null) {
        throw raise(RuntimeException.class, new IOException("No persistance for " + name));
      } else {
        return null;
      }
    }

    @SuppressWarnings("unchecked")
    final <T> Persistance<T> forType(Class<T> type) {
      var p = types.get(type);
      if (p == null) {
        p = searchSupertype(type.getName(), type);
      }
      return (Persistance<T>) p;
    }

    final Persistance<?> forId(int id) {
      var p = ids.get(id);
      if (p == null) {
        throw raise(RuntimeException.class, new IOException("No persistance for " + id));
      }
      return p;
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E)t;
  }
}
