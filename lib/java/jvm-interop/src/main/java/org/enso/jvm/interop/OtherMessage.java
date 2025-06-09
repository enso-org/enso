package org.enso.jvm.interop;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;

@Persistable(id = 81901)
record OtherMessage(long id, Message message, List<Object> args)
    implements Function<Channel, OtherResult> {
  private static final Map<Long, TruffleObject> OBJECTS = new HashMap<>();

  @Override
  public OtherResult apply(Channel t) {
    try {
      var receiver = OBJECTS.get(id);
      assert receiver instanceof TruffleObject;
      var res = ReflectionLibrary.getUncached().send(receiver, message, args.toArray());
      return new OtherResult(res);
    } catch (Exception ex) {
      return new OtherResult(ex);
    }
  }

  static synchronized long registerObject(TruffleObject obj) {
    var size = OBJECTS.size();
    OBJECTS.put((long) size, obj);
    return size;
  }

  @Persistable(id = 81902)
  static final class PersistTruffleMessage extends Persistance<Message> {
    public PersistTruffleMessage() {
      super(Message.class, true, 81902);
    }

    @Override
    protected void writeObject(Message obj, Output out) throws IOException {
      assert InteropLibrary.class == obj.getLibraryClass();
      out.writeUTF(obj.getSimpleName());
    }

    @Override
    protected Message readObject(Input in) throws IOException, ClassNotFoundException {
      var name = in.readUTF();
      return Message.resolve(InteropLibrary.class, name);
    }
  }

  @Persistable(id = 81903)
  static final class PersistObjectArray extends Persistance<Object[]> {
    public PersistObjectArray() {
      super(Object[].class, true, 81903);
    }

    @Override
    protected void writeObject(Object[] obj, Output out) throws IOException {
      var size = obj.length;
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(obj[i]);
      }
    }

    @Override
    protected Object[] readObject(Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var arr = new Object[size];
      for (var i = 0; i < size; i++) {
        arr[i] = in.readObject();
      }
      return arr;
    }
  }

  @Persistable(id = 81904)
  static final class PersistList extends Persistance<List> {
    public PersistList() {
      super(List.class, true, 81904);
    }

    @Override
    protected void writeObject(List obj, Output out) throws IOException {
      var size = obj.size();
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(obj.get(i));
      }
    }

    @Override
    protected List readObject(Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var arr = new ArrayList<Object>(size);
      while (size-- > 0) {
        arr.add(in.readObject());
      }
      return arr;
    }
  }

  //
  // primitive types
  //

  @Persistable(id = 101)
  static final class PersistBoolean extends Persistance<Boolean> {
    public PersistBoolean() {
      super(Boolean.class, true, 101);
    }

    @Override
    protected void writeObject(Boolean obj, Output out) throws IOException {
      out.writeBoolean(obj);
    }

    @Override
    protected Boolean readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readBoolean();
    }
  }

  @Persistable(id = 102)
  static final class PersistByte extends Persistance<Byte> {
    public PersistByte() {
      super(Byte.class, true, 102);
    }

    @Override
    protected void writeObject(Byte obj, Output out) throws IOException {
      out.writeByte(obj);
    }

    @Override
    protected Byte readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readByte();
    }
  }

  @Persistable(id = 103)
  static final class PersistShort extends Persistance<Short> {
    public PersistShort() {
      super(Short.class, true, 103);
    }

    @Override
    protected void writeObject(Short obj, Output out) throws IOException {
      out.writeShort(obj);
    }

    @Override
    protected Short readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readShort();
    }
  }

  @Persistable(id = 104)
  static final class PersistInteger extends Persistance<Integer> {
    public PersistInteger() {
      super(Integer.class, true, 104);
    }

    @Override
    protected void writeObject(Integer obj, Output out) throws IOException {
      out.writeInt(obj);
    }

    @Override
    protected Integer readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readInt();
    }
  }

  @Persistable(id = 105)
  static final class PersistLong extends Persistance<Long> {
    public PersistLong() {
      super(Long.class, true, 105);
    }

    @Override
    protected void writeObject(Long obj, Output out) throws IOException {
      out.writeLong(obj);
    }

    @Override
    protected Long readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readLong();
    }
  }

  @Persistable(id = 106)
  static final class PersistFloat extends Persistance<Float> {
    public PersistFloat() {
      super(Float.class, true, 106);
    }

    @Override
    protected void writeObject(Float obj, Output out) throws IOException {
      out.writeFloat(obj);
    }

    @Override
    protected Float readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readFloat();
    }
  }

  @Persistable(id = 107)
  static final class PersistDouble extends Persistance<Double> {
    public PersistDouble() {
      super(Double.class, true, 107);
    }

    @Override
    protected void writeObject(Double obj, Output out) throws IOException {
      out.writeDouble(obj);
    }

    @Override
    protected Double readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readDouble();
    }
  }

  @Persistable(id = 108)
  static final class PersistCharacter extends Persistance<Character> {
    public PersistCharacter() {
      super(Character.class, true, 108);
    }

    @Override
    protected void writeObject(Character obj, Output out) throws IOException {
      out.writeChar(obj);
    }

    @Override
    protected Character readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readChar();
    }
  }

  @Persistable(id = 109)
  static final class PersistString extends Persistance<String> {
    public PersistString() {
      super(String.class, true, 109);
    }

    @Override
    protected void writeObject(String obj, Output out) throws IOException {
      out.writeUTF(obj);
    }

    @Override
    protected String readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readUTF();
    }
  }
}
