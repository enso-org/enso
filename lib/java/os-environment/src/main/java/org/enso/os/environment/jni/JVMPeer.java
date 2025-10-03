package org.enso.os.environment.jni;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;

public final class JVMPeer extends Channel.Config {

  public JVMPeer() {}

  @Override
  public Persistance.Pool createPool(Channel<?> ignore) {
    return Persistables.POOL;
  }

  @Persistable(id = 432002)
  static final class PersistList extends Persistance<List> {
    PersistList() {
      super(List.class, true, 432002);
    }

    @Override
    protected void writeObject(List obj, Persistance.Output out) throws IOException {
      out.writeInt(obj.size());
      for (Object o : obj) {
        out.writeObject(o);
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected List readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      int size = in.readInt();
      var lst = new ArrayList(size);
      for (int i = 0; i < size; i++) {
        var obj = in.readObject();
        lst.add(obj);
      }
      return lst;
    }
  }

  @Persistable(id = 4437)
  public static final class PersistString extends Persistance<String> {
    public PersistString() {
      super(String.class, true, 4437);
    }

    @Override
    protected void writeObject(String obj, Persistance.Output out) throws IOException {
      out.writeUTF(obj);
    }

    @Override
    protected String readObject(Persistance.Input in) throws IOException, ClassNotFoundException {
      var obj = in.readUTF();
      return obj;
    }
  }

  @Persistable(id = 4438)
  static final class PersistBigInt extends Persistance<BigInteger> {
    PersistBigInt() {
      super(BigInteger.class, true, 4438);
    }

    @Override
    protected void writeObject(BigInteger obj, Persistance.Output out) throws IOException {
      var arr = obj.toByteArray();
      out.writeInt(arr.length);
      out.write(arr);
    }

    @Override
    protected BigInteger readObject(Persistance.Input in)
        throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var arr = new byte[size];
      in.readFully(arr);
      return new BigInteger(arr);
    }
  }

  @Persistable(id = 4439)
  static final class PersistLong extends Persistance<Long> {
    PersistLong() {
      super(Long.class, true, 4439);
    }

    @Override
    protected void writeObject(Long obj, Persistance.Output out) throws IOException {
      out.writeLong(obj);
    }

    @Override
    protected Long readObject(Persistance.Input in) throws IOException {
      return in.readLong();
    }
  }
}
