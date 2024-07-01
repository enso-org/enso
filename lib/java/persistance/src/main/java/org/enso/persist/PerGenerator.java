package org.enso.persist;

import java.io.ByteArrayOutputStream;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.Function;
import org.slf4j.Logger;

final class PerGenerator {
  static final byte[] HEADER = new byte[] {0x0a, 0x0d, 0x13, 0x13};
  private final OutputStream main;
  private final Map<Object, WriteResult> knownObjects = new IdentityHashMap<>();
  private int countReferences = 1;
  private final Map<Object, Integer> pendingReferences = new IdentityHashMap<>();
  private final Histogram histogram;
  private final PerMap map;
  private final Function<Object, Object> writeReplace;
  private int position;

  private PerGenerator(
      OutputStream out, Histogram histogram, int position, Function<Object, Object> writeReplace) {
    this.map = PerMap.create();
    this.main = out;
    this.writeReplace = writeReplace == null ? Function.identity() : writeReplace;
    this.position = position;
    this.histogram = histogram;
  }

  static byte[] writeObject(Object obj, Function<Object, Object> writeReplace) throws IOException {
    var histogram = PerUtils.LOG.isDebugEnabled() ? new Histogram() : null;

    var out = new ByteArrayOutputStream();
    var data = new DataOutputStream(out);
    var g = new PerGenerator(out, histogram, 12, writeReplace);
    data.write(PerGenerator.HEADER);
    data.writeInt(g.versionStamp());
    data.write(new byte[4]); // space
    data.flush();

    var at = g.writeObjectAndReferences(obj);

    var arr = out.toByteArray();
    putIntToArray(arr, 8, at);

    if (histogram != null) {
      histogram.dump(PerUtils.LOG, arr.length);
    }
    return arr;
  }

  private static void putIntToArray(byte[] arr, int position, int value) {
    arr[position] = (byte) ((value >> 24) & 0xff);
    arr[position + 1] = (byte) ((value >> 16) & 0xff);
    arr[position + 2] = (byte) ((value >> 8) & 0xff);
    arr[position + 3] = (byte) (value & 0xff);
  }

  private record WriteResult(Persistance<?> usedPersistance, int position) {
    void writePositionAndPersistanceId(DataOutput out) throws IOException {
      if (usedPersistance == null) {
        assert position == NULL_REFERENCE_ID;
        out.writeInt(NULL_REFERENCE_ID);
      } else {
        out.writeInt(position);
        out.writeInt(usedPersistance.id);
      }
    }
  }

  final <T> WriteResult writeObject(T t) throws IOException {
    if (t == null) {
      return new WriteResult(null, NULL_REFERENCE_ID);
    }
    java.lang.Object obj = writeReplace.apply(t);
    WriteResult found = knownObjects.get(obj);
    if (found == null) {
      org.enso.persist.Persistance<?> p = map.forType(obj.getClass());
      java.io.ByteArrayOutputStream os = new ByteArrayOutputStream();
      p.writeInline(obj, new ReferenceOutput(this, os));
      found = new WriteResult(p, this.position);
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
      out.writeInt(NULL_REFERENCE_ID);
      return;
    }
    if (obj instanceof String s) {
      obj = s.intern();
    }
    WriteResult found = knownObjects.get(obj);
    if (found == null) {
      var os = new ByteArrayOutputStream();
      var osData = new ReferenceOutput(this, os);
      org.enso.persist.Persistance<?> p = map.forType(obj.getClass());
      p.writeInline(obj, osData);
      found = new WriteResult(p, position);
      if (os.size() == 0) {
        os.write(0);
      }
      byte[] arr = os.toByteArray();
      main.write(arr);
      position += arr.length;
      knownObjects.put(obj, found);
      if (histogram != null) {
        histogram.register(obj.getClass(), arr.length);
      }
    }
    found.writePositionAndPersistanceId(out);
  }

  final int versionStamp() {
    return map.versionStamp;
  }

  private int registerReference(Persistance.Reference<?> ref) {
    var obj = ref.get(Object.class);
    if (obj == null) {
      return NULL_REFERENCE_ID;
    }

    var existingId = pendingReferences.get(obj);
    if (existingId == null) {
      var currentSize = countReferences++;
      pendingReferences.put(obj, currentSize);
      return currentSize;
    } else {
      return existingId;
    }
  }

  /**
   * Writes an object into the buffer. Writes also all {@link Persistance.Reference} that were left
   * pending during the serialization.
   *
   * @param obj the object to write down
   * @return location of the table {@code int size and then int[size]}
   */
  private int writeObjectAndReferences(Object obj) throws IOException {
    pendingReferences.put(obj, 0);
    WriteResult root = writeObject(obj);

    var refsOut = new ByteArrayOutputStream();
    var refsData = new DataOutputStream(refsOut);
    refsData.writeInt(-1); // space for size of references
    root.writePositionAndPersistanceId(refsData);
    var count = 1;
    for (; ; ) {
      var all = new ArrayList<>(pendingReferences.entrySet());
      all.sort(
          (e1, e2) -> {
            return e1.getValue() - e2.getValue();
          });
      var round = all.subList(count, all.size());
      if (round.isEmpty()) {
        break;
      }
      for (var entry : round) {
        WriteResult writeResult = writeObject(entry.getKey());
        assert count == entry.getValue()
            : "Expecting "
                + count
                + " got "
                + entry.getValue()
                + " (in "
                + entry.getKey().getClass().getCanonicalName()
                + ")";
        writeResult.writePositionAndPersistanceId(refsData);
        count++;
      }
    }
    refsData.flush();
    var arr = refsOut.toByteArray();

    putIntToArray(arr, 0, count);

    var tableAt = this.position;
    this.main.write(arr);
    this.position += arr.length;

    return tableAt;
  }

  static int registerReference(Persistance.Output out, Persistance.Reference<?> ref) {
    var g = ((ReferenceOutput) out).generator;
    return g.registerReference(ref);
  }

  static final class ReferenceOutput extends DataOutputStream implements Persistance.Output {
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

  private static final class Histogram {
    private final Map<Class, int[]> knownTypes = new HashMap<>();

    private void dump(Logger log, int length) {
      var counts = knownTypes;
      var list = new ArrayList<>(counts.entrySet());
      list.sort(
          (a, b) -> {
            return a.getValue()[0] - b.getValue()[0];
          });

      log.debug("==== Top Bytes & Counts of Classes =====");
      for (var i = 0; i < list.size(); i++) {
        if (i == 30) {
          break;
        }
        var elem = list.get(list.size() - 1 - i);
        log.debug(
            "  " + elem.getValue()[0] + " " + elem.getValue()[1] + " " + elem.getKey().getName());
      }
    }

    private void register(Class<?> type, int length) {
      var c = knownTypes.get(type);
      if (c == null) {
        c = new int[2];
        knownTypes.put(type, c);
      }
      c[0] += length;
      c[1]++;
    }
  }

  static final int INLINED_REFERENCE_ID = -2;
  static final int NULL_REFERENCE_ID = -1;
}
