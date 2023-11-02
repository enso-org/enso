package org.enso.compiler.core;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.ServiceLoader;

import org.enso.compiler.core.Persistance.Reference;

public abstract class Persistance<T> {
  private final Class<T> clazz;
  private final int id;

  protected Persistance(Class<T> clazz, int id) {
    this.clazz = clazz;
    this.id = id;
  }

  protected abstract void writeObject(T obj, Output out) throws IOException;
  protected abstract T readObject(Input in) throws IOException, ClassNotFoundException;

  public static abstract class Output implements DataOutput {
    Output() {
    }

    public abstract <T> void writeInline(Class<T> clazz, T obj) throws IOException;
    public abstract void writeObject(Object obj) throws IOException;
  }

  public static abstract class Input implements DataInput {
    Input() {
    }

    public abstract <T> T readInline(Class<T> clazz) throws IOException;
    public abstract Object readObject() throws IOException;
  }

  final void writeInline(Object obj, Output out) throws IOException {
    writeObject(clazz.cast(obj), out);
  }

  final int writeDirect(Object obj, Generator into) throws IOException {
    var output = new OutputImpl(into);
    var at = into.buffer.position();
    into.buffer.putInt(id);
    writeObject(clazz.cast(obj), output);
    return at;
  }

  final T readWith(Input in) {
      try {
        return readObject(in);
      } catch (IOException | ClassNotFoundException ex) {
        throw raise(RuntimeException.class, ex);
      }
  }

  public static Generator newGenerator(ByteBuffer buffer) {
    return new Generator(buffer);
  }

  public static final class Generator {
    private final PersistanceMap map = new PersistanceMap();
    private final ByteBuffer buffer;
    private final Map<Object,Integer> knownObjects = new IdentityHashMap<>();

    private Generator(ByteBuffer buffer) {
      this.buffer = buffer;
    }

    public <T> Reference<T> writeObject(T obj) throws IOException {
      if (obj == null) {
        return Reference.none();
      }
      var found = knownObjects.get(obj);
      if (found == null) {
        var p = map.forType(obj.getClass());
        found = this.buffer.position();
        p.writeInline(obj, new OutputImpl(this));
        knownObjects.put(obj, found);
      }
      return Reference.from(buffer, found);
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

    public static <V> Reference<V> from(ByteBuffer buffer, int offset) {
      return new BufferReference<>(buffer, offset);
    }
  }


  private static final class BufferReference<T> extends Reference<T> {
    private final ByteBuffer buffer;
    private final int offset;

    BufferReference(ByteBuffer buffer, int offset) {
      this.buffer = buffer;
      this.offset = offset;
    }

    final <T> T readObject(Class<T> clazz) {
      var in = new InputImpl(buffer, offset);
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

  private static final class OutputImpl extends Output {
    final Generator generator;

    OutputImpl(Generator buffer) {
      this.generator = buffer;
    }

    @Override
    public <T> void writeInline(Class<T> clazz, T obj) throws IOException {
      if (clazz != obj.getClass()) {
        throw new IOException("Cannot store " + obj + " as " + clazz.getName());
      }
      var p = generator.map.forType(clazz);
      p.writeInline(obj, this);
    }



    @Override
    public void writeObject(Object obj) throws IOException {
      var p = generator.map.forType(obj.getClass());
      var at = p.writeDirect(obj, generator);
      writeInt(at);
    }

    @Override
    public void write(int b) throws IOException {
      writeByte(b);
    }

    @Override
    public void write(byte[] b) throws IOException {
      generator.buffer.put(b);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
      write(Arrays.copyOfRange(b, off, len));
    }

    private static final byte[] TRUE = new byte[] { 1 };
    private static final byte[] FALSE = new byte[] { 0 };

    @Override
    public void writeBoolean(boolean v) throws IOException {
      write(v ? TRUE : FALSE);
    }

    @Override
    public void writeByte(int v) throws IOException {
      write(new byte[] { (byte) v });
    }

    @Override
    public void writeShort(int v) throws IOException {
      write(new byte[] { (byte) (v >> 8), (byte) (v & 0xff) });
    }

    @Override
    public void writeChar(int v) throws IOException {
      writeShort(v);
    }

    @Override
    public void writeInt(int v) throws IOException {
      write(new byte[] { (byte) (v >> 24 & 0xff), (byte) (v >> 16 & 0xff), (byte) (v >> 8 & 0xff), (byte) (v & 0xff) });
    }

    @Override
    public void writeLong(long v) throws IOException {
      var hi = v >> 32;
      write(new byte[] {
        (byte) (hi >> 24 & 0xff), (byte) (hi >> 16 & 0xff), (byte) (hi >> 8 & 0xff), (byte) (hi & 0xff),
        (byte) (v >> 24 & 0xff), (byte) (v >> 16 & 0xff), (byte) (v >> 8 & 0xff), (byte) (v & 0xff)
      });
    }

    @Override
    public void writeFloat(float v) throws IOException {
      writeInt(Float.floatToRawIntBits(v));
    }

    @Override
    public void writeDouble(double v) throws IOException {
      writeLong(Double.doubleToRawLongBits(v));
    }

    @Override
    public void writeBytes(String s) throws IOException {
      writeUTF(s);
    }

    @Override
    public void writeChars(String s) throws IOException {
      writeUTF(s);
    }

    @Override
    public void writeUTF(String s) throws IOException {
      var arr = s.getBytes(StandardCharsets.UTF_8);
      writeInt(arr.length);
      write(arr);
    }
  }

  private static final class InputImpl extends Input {
    private final PersistanceMap map = new PersistanceMap();
    private final ByteBuffer buf;
    private int at;

    InputImpl(ByteBuffer buf, int at) {
      this.buf = buf;
      this.at = at;
    }

    @Override
    public <T> T readInline(Class<T> clazz) {
      var p = map.forType(clazz);
      return p.readWith(this);
    }

    @Override
    public Object readObject() throws IOException {
      var id = readInt();
      var p = map.forId(id);
      return p.readWith(this);
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
      var len = readInt();
      var arr = new byte[len];
      readFully(arr);
      return new String(arr, StandardCharsets.UTF_8);
    }
  }

  private static final class PersistanceMap {
    private final Map<Integer, Persistance<?>> ids = new HashMap<>();
    private final Map<Class<?>, Persistance<?>> types = new HashMap<>();
    private final int versionStamp;

    private PersistanceMap() {
      var hash = 0;
      for (var p : ServiceLoader.load(Persistance.class)) {
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
    final <T> Persistance<T> forType(Class<T> type) {
      var p = types.get(type);
      if (p == null) {
        throw new IllegalStateException("No persistance for " + type.getName());
      }
      return (Persistance<T>) p;
    }

    final Persistance<?> forId(int id) {
      var p = ids.get(id);
      if (p == null) {
        throw new IllegalStateException("No persistance for " + id);
      }
      return p;
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E)t;
  }
}
