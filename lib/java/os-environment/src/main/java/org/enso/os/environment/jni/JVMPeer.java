package org.enso.os.environment.jni;

import java.io.IOException;
import java.lang.foreign.MemorySegment;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;

final class JVMPeer {
  static final Persistance.Pool POOL = Persistables.POOL;

  private JVMPeer() {}

  static long handle(long address, long size) {
    try {
      var seg = MemorySegment.ofAddress(address).reinterpret(size);
      var buf = seg.asByteBuffer();
      var ref = POOL.read(buf, null);
      var msg = ref.get(JVM.Message.class);
      var res = msg.evaluate();
      var bytes = Persistables.POOL.write(res, null);
      seg.copyFrom(MemorySegment.ofArray(bytes));
      return bytes.length;
    } catch (Throwable t) {
      // TBD: proper handling of exceptions is needed
      t.printStackTrace();
      return -1;
    }
  }

  @Persistable(id = 432001)
  public static final class ExecuteMainClass extends JVM.Message<ExecuteMainClass> {
    private final String mainClassWithSlashes;
    private final List<String> args;

    public ExecuteMainClass(String mainClassWithSlashes, List<String> args) {
      super(ExecuteMainClass.class);
      this.mainClassWithSlashes = mainClassWithSlashes;
      this.args = args;
    }

    public String mainClassWithSlashes() {
      return mainClassWithSlashes;
    }

    public List<String> args() {
      return Collections.unmodifiableList(args);
    }

    @Override
    protected final ExecuteMainClass evaluate() throws Exception {
      var clazz = Class.forName(mainClassWithSlashes.replace('/', '.'));
      var method = clazz.getDeclaredMethod("main", String[].class);
      method.setAccessible(true);
      method.invoke(null, (Object) args.toArray(new String[args.size()]));
      return this;
    }
  }

  @Persistable(id = 432002)
  public static final class PersistList extends Persistance<List> {
    public PersistList() {
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
}
