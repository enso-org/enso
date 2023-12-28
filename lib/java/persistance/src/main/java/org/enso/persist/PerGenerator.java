package org.enso.persist;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.Function;

final class PerGenerator {

  static final byte[] HEADER = new byte[] {0x0a, 0x0d, 0x02, 0x0f};
  private final OutputStream main;
  private final Map<Object, Integer> knownObjects = new IdentityHashMap<>();
  private final Map<Class, int[]> knownTypes = new HashMap<>();
  private final PerMap map;
  final Function<Object, Object> writeReplace;
  private int position;

  private PerGenerator(OutputStream out, int position, Function<Object, Object> writeReplace) {
    this.map = PerMap.create();
    this.main = out;
    this.writeReplace = writeReplace == null ? Function.identity() : writeReplace;
    this.position = position;
  }

  static byte[] writeObject(Object obj, Function<Object, Object> writeReplace) throws IOException {
    var out = new ByteArrayOutputStream();
    var data = new DataOutputStream(out);
    var g = new PerGenerator(out, 12, writeReplace);
    data.write(PerGenerator.HEADER);
    data.writeInt(g.versionStamp());
    data.write(new byte[4]); // space
    data.flush();
    var at = g.writeObject(obj);
    var arr = out.toByteArray();
    arr[8] = (byte) ((at >> 24) & 0xff);
    arr[9] = (byte) ((at >> 16) & 0xff);
    arr[10] = (byte) ((at >> 8) & 0xff);
    arr[11] = (byte) (at & 0xff);

    var counts = g.knownTypes;
    var list = new ArrayList<>(counts.entrySet());
    list.sort(
        (a, b) -> {
          return a.getValue()[0] - b.getValue()[0];
        });

    System.err.println("==== Top Bytes & Counts of Classes =====");
    for (var i = 0; i < list.size(); i++) {
      if (i == 30) {
        break;
      }
      var elem = list.get(list.size() - 1 - i);
      System.err.println(
          "  " + elem.getValue()[0] + " " + elem.getValue()[1] + " " + elem.getKey().getName());
    }

    return arr;
  }

  final <T> int writeObject(T t) throws IOException {
    if (t == null) {
      return -1;
    }
    java.lang.Object obj = writeReplace.apply(t);
    java.lang.Integer found = knownObjects.get(obj);
    if (found == null) {
      org.enso.persist.Persistance<?> p = map.forType(obj.getClass());
      java.io.ByteArrayOutputStream os = new ByteArrayOutputStream();
      p.writeInline(obj, new ReferenceOutput(this, os));
      found = this.position;
      byte[] arr = os.toByteArray();
      main.write(arr);
      this.position += arr.length;
      knownObjects.put(obj, found);
    }
    return found;
  }

  final void writeIndirect(Object obj, Persistance.Output out) throws IOException {
    obj = writeReplace.apply(obj);
    if (obj == null) {
      out.writeInt(-1);
      return;
    }
    org.enso.persist.Persistance<?> p = map.forType(obj.getClass());
    if (obj instanceof String s) {
      obj = s.intern();
    }
    java.lang.Integer found = knownObjects.get(obj);
    if (found == null) {
      var os = new ByteArrayOutputStream();
      var osData = new ReferenceOutput(this, os);
      p.writeInline(obj, osData);
      found = position;
      if (os.size() == 0) {
        os.write(0);
      }
      byte[] arr = os.toByteArray();
      main.write(arr);
      position += arr.length;
      knownObjects.put(obj, found);
      var c = knownTypes.get(obj.getClass());
      if (c == null) {
        c = new int[2];
        knownTypes.put(obj.getClass(), c);
      }
      c[0] += arr.length;
      c[1]++;
    }
    out.writeInt(found);
    out.writeInt(p.id);
  }

  final int versionStamp() {
    return map.versionStamp;
  }

  private static final class ReferenceOutput extends DataOutputStream
      implements Persistance.Output {
    private final PerGenerator generator;

    ReferenceOutput(PerGenerator g, ByteArrayOutputStream out) {
      super(out);
      this.generator = g;
    }

    @Override
    public <T> void writeInline(Class<T> clazz, T t) throws IOException {
      var obj = generator.writeReplace.apply(t);
      var p = generator.map.forType(clazz);
      p.writeInline(obj, this);
    }

    @Override
    public void writeObject(Object obj) throws IOException {
      this.generator.writeIndirect(obj, this);
    }
  }
}
