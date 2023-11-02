package org.enso.compiler.core;

import java.io.DataInput;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

public abstract class Persistance<T> {
  private final Class<T> clazz;
  private final int id;

  protected Persistance(Class<T> clazz, int id) {
    this.clazz = clazz;
    this.id = id;
  }


  protected abstract void writeObject(T obj, ObjectOutput out) throws IOException;
  protected abstract T readObject(ObjectInput in) throws IOException, ClassNotFoundException;

  final int writeWith(Object obj, Generator into) throws IOException {
    var at = into.buffer.position();
    into.buffer.putInt(id);
    var output = new OperationOutput(into);
    writeObject(clazz.cast(obj), output);
    for (var arr : output.data) {
      into.buffer.put(arr);
    }
    return at;
  }

  final T readWith(ObjectInput in) {
      try {
        return readObject(in);
      } catch (IOException | ClassNotFoundException ex) {
        throw new IllegalStateException(ex);
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

    public int writeObject(Object obj) throws IOException {
      if (obj == null) {
        return -1;
      }
      var found = knownObjects.get(obj);
      if (found != null) {
        return found;
      }
      var p = map.forType(obj.getClass());
      var at = p.writeWith(obj, this);
      knownObjects.put(obj, at);
      return at;
    }
  }


  public static sealed abstract class Reference<T> {
    private Reference() {
    }

    public <V> V get(Class<V> expectedType) {
      var value = switch (this) {
        case MemoryReference m -> m.value;
        case BufferReference b -> b.readObject(expectedType);
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
      for (var p : ServiceLoader.load(Persistance.class)) {
        if (p.clazz == clazz) {
            return clazz.cast(p.readWith(new Input(buffer, offset)));
        }
      }
      throw new IllegalStateException("No Persistance for " + clazz.getName());

    }
  }

  private static final class MemoryReference<T> extends Reference<T> {
    private final T value;

    MemoryReference(T obj) {
      this.value = obj;
    }
  }

  private static final class OperationOutput implements ObjectOutput {
    final Generator generator;
    final List<byte[]> data = new ArrayList<>();

    OperationOutput(Generator buffer) {
      this.generator = buffer;
    }

    @Override
    public void writeObject(Object obj) throws IOException {
      int at = generator.writeObject(obj);
      writeInt(at);
    }

    @Override
    public void write(int b) throws IOException {
      writeByte(b);
    }

    @Override
    public void write(byte[] b) throws IOException {
      data.add(b);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
      write(Arrays.copyOfRange(b, off, len));
    }

    @Override
    public void flush() throws IOException {
    }

    @Override
    public void close() throws IOException {
    }

    private static final byte[] TRUE = new byte[] { 1 };
    private static final byte[] FALSE = new byte[] { 0 };

    @Override
    public void writeBoolean(boolean v) throws IOException {
      data.add(v ? TRUE : FALSE);
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
  private static final class Input implements ObjectInput {
    private final ByteBuffer buf;
    private int at;

    Input(ByteBuffer buf, int at) {
      this.buf = buf;
      this.at = at;
    }

    @Override
    public Object readObject() throws ClassNotFoundException, IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public int read() throws IOException {
      return buf.get(at++);
    }

    @Override
    public int read(byte[] b) throws IOException {
      buf.get(at += b.length, b);
      return b.length;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
      buf.get(at += len, b, off, len);
      return len;
    }

    @Override
    public long skip(long n) throws IOException {
      at += Math.toIntExact(n);
      return at;
    }

    @Override
    public int available() throws IOException {
      return buf.limit() - at;
    }

    @Override
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
      return buf.getShort(at += 2);
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
      return buf.getInt(at += 4);
    }

    @Override
    public long readLong() throws IOException {
      return buf.getLong(at += 8);
    }

    @Override
    public float readFloat() throws IOException {
      return buf.getFloat(at += 4);
    }

    @Override
    public double readDouble() throws IOException {
      return buf.getDouble(at += 8);
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
}
