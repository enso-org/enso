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
  static final class PersistList extends Persistance<List> {
    public PersistList() {
      super(List.class, true, 81903);
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

  @Persistable(id = 81904)
  static final class PersistBoolean extends Persistance<Boolean> {
    public PersistBoolean() {
      super(Boolean.class, true, 81904);
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

  @Persistable(id = 81905)
  static final class PersistLong extends Persistance<Long> {
    public PersistLong() {
      super(Long.class, true, 81905);
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

  @Persistable(id = 81906)
  static final class PersistString extends Persistance<String> {
    public PersistString() {
      super(String.class, true, 81906);
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
