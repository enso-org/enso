package org.enso.persist;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;
import org.junit.Test;
import org.openide.util.lookup.ServiceProvider;

public class PersistanceTest {
  @Test
  public void testUUIDPersistance() throws Exception {
    // @start region="write"
    var obj = UUID.randomUUID();
    var buffer = Persistance.write(obj, null);
    assertNotNull("Byte array is returned", buffer);
    assertNotEquals("It has non-zero length", 0, buffer.length);
    // @end region="write"

    // @start region="read"
    var ref = Persistance.read(buffer, null);
    var loaded = ref.get(UUID.class);
    assertEquals("The same object was recreated", obj, loaded);
    // @end region="read"
  }

  @Test
  public void readResolve() throws Exception {
    var in = new Service(5);
    var arr = Persistance.write(in, (Function<Object, Object>) null);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Remains five", 5, plain.get(Service.class).value());

    var multiOnRead =
        Persistance.read(arr, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);
    assertEquals("Multiplied on read", 15, multiOnRead.get(Service.class).value());
  }

  @Test
  public void writeReplace() throws Exception {
    var in = new Service(5);
    var arr =
        Persistance.write(in, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Multiplied on write", 15, plain.get(Service.class).value());
  }

  @Test
  public void readResolveInline() throws Exception {
    var in = new ServiceSupply(new Service(5));
    var arr = Persistance.write(in, (Function<Object, Object>) null);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Remains five", 5, plain.get(ServiceSupply.class).supply().value());

    var multiOnRead =
        Persistance.read(arr, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);
    assertEquals("Multiplied on read", 15, multiOnRead.get(ServiceSupply.class).supply().value());
  }

  @Test
  public void writeReplaceInline() throws Exception {
    var in = new ServiceSupply(new Service(5));
    var arr =
        Persistance.write(in, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Multiplied on write", 15, plain.get(ServiceSupply.class).supply().value());
  }

  @Test
  public void readResolveReference() throws Exception {
    var in = new IntegerSupply(new Service(5));
    var arr = Persistance.write(in, (Function<Object, Object>) null);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Remains five", 5, (int) plain.get(IntegerSupply.class).supply().get());
    assertEquals("Remains five 2", 5, (int) plain.get(IntegerSupply.class).supply().get());

    var multiOnRead =
        Persistance.read(arr, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);
    assertEquals(
        "Multiplied on read", 15, (int) multiOnRead.get(IntegerSupply.class).supply().get());
  }

  @Test
  public void writeReplaceReference() throws Exception {
    var in = new IntegerSupply(new Service(5));
    var arr =
        Persistance.write(in, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Multiplied on write", 15, (int) plain.get(IntegerSupply.class).supply().get());
  }

  static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
    var arr = Persistance.write(l, (Function<Object, Object>) null);
    if (expectedSize >= 0) {
      assertEquals(expectedSize, arr.length - 12);
    }
    var ref = Persistance.read(arr, (Function<Object, Object>) null);
    return ref.get(clazz);
  }

  // @start region="manual"
  @ServiceProvider(service = Persistance.class)
  public static final class PersistUUID extends Persistance<UUID> {

    public PersistUUID() {
      super(UUID.class, false, 328439);
    }

    @Override
    protected void writeObject(UUID obj, Output out) throws IOException {
      out.writeLong(obj.getMostSignificantBits());
      out.writeLong(obj.getLeastSignificantBits());
    }

    @Override
    protected UUID readObject(Input in) throws IOException, ClassNotFoundException {
      var most = in.readLong();
      var least = in.readLong();
      return new UUID(most, least);
    }
  }

  // @end region="manual"

  public static final class Singleton {
    public static final Singleton INSTANCE = new Singleton();

    private Singleton() {}
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistSingleton extends Persistance<Singleton> {

    public PersistSingleton() {
      super(Singleton.class, false, 432433);
    }

    @Override
    protected void writeObject(Singleton obj, Output out) throws IOException {}

    @Override
    protected Singleton readObject(Input in) throws IOException, ClassNotFoundException {
      return Singleton.INSTANCE;
    }
  }

  // @start region="annotation"
  @Persistable(clazz = Service.class, id = 432434)
  @Persistable(clazz = IntegerSupply.class, id = 432435)
  public record Service(int value) implements Supplier<Integer> {
    @Override
    public Integer get() {
      return value;
    }
  }

  public record IntegerSupply(Supplier<Integer> supply) {}

  // @end region="annotation"

  // @start region="self-annotation"
  @Persistable(id = 432436)
  public record ServiceSupply(Service supply) {}

  // @end region="self-annotation"

  @Persistable(id = 432437)
  public static class SelfLoop {
    public Persistance.Reference<SelfLoop> self;

    public Persistance.Reference<SelfLoop> self() {
      return self;
    }

    public SelfLoop(Persistance.Reference<SelfLoop> self) {
      this.self = self;
    }
  }

  @Test
  public void testReferenceLoopsInPersistance() throws Exception {
    var obj = new SelfLoop(null);
    // make the loop
    obj.self = Persistance.Reference.of(obj, true);

    var loaded = serde(SelfLoop.class, obj, -1);
    var next = loaded.self.get(SelfLoop.class);
    var next2 = next.self.get(SelfLoop.class);
    assertSame("The recreated object again points to itself", next, next2);
    assertSame("The recreated object again points to itself", loaded, next);
  }

  @Persistable(id = 432439)
  public record LongerLoop1(int x, Persistance.Reference<LongerLoop2> y) {}

  @Persistable(id = 432440)
  public record LongerLoop2(Persistance.Reference<LongerLoop3> y) {}

  @Persistable(id = 432441)
  public static class LongerLoop3 {
    public final String a;
    public Persistance.Reference<LongerLoop1> y;

    public String a() {
      return a;
    }

    public Persistance.Reference<LongerLoop1> y() {
      return y;
    }

    public LongerLoop3(String a, Persistance.Reference<LongerLoop1> y) {
      this.a = a;
      this.y = y;
    }
  }

  @Test
  public void testLoopsBetweenDifferentTypes() throws Exception {
    var obj3 = new LongerLoop3("a", null);
    var obj2 = new LongerLoop2(Persistance.Reference.of(obj3, true));
    var obj1 = new LongerLoop1(1, Persistance.Reference.of(obj2, true));
    obj3.y = Persistance.Reference.of(obj1, true);

    var loaded1 = serde(LongerLoop1.class, obj1, -1);
    var r2 = loaded1.y().get(LongerLoop2.class);
    var r3 = r2.y().get(LongerLoop3.class);
    var r1 = r3.y().get(LongerLoop1.class);

    assertSame("The recreated structure contains the loop", loaded1, r1);

    var current = r1;
    for (var i = 0; i < 10; i++) {
      var next =
          loaded1.y().get(LongerLoop2.class).y().get(LongerLoop3.class).y().get(LongerLoop1.class);
      assertSame("current points back to itself", current, next);
      current = next;
    }
  }

  @Test
  public void testReferenceLoopsSavedTwiceInPersistance() throws Exception {
    var obj3 = new LongerLoop3("a", null);
    var obj2 = new LongerLoop2(Persistance.Reference.of(obj3, true));
    var obj1 = new LongerLoop1(1, Persistance.Reference.of(obj2, true));
    obj3.y = Persistance.Reference.of(obj1, true);

    var loaded1 = serde(LongerLoop1.class, obj1, -1);
    // Now we serialize the deserialized object again - this is to test that references read from
    // file can be serialized back to a file.
    var loadedAgain = serde(LongerLoop1.class, loaded1, -1);
    var r2 =
        loadedAgain
            .y()
            .get(LongerLoop2.class)
            .y()
            .get(LongerLoop3.class)
            .y()
            .get(LongerLoop1.class);
    assertSame("The recreated structure contains the loop", loadedAgain, r2);
  }

  @Test
  public void testNullReference() throws Exception {
    var obj1 = new LongerLoop1(1, Persistance.Reference.none());

    var loaded1 = serde(LongerLoop1.class, obj1, -1);
    var inner1 = loaded1.y().get(LongerLoop2.class);
    assertSame("The reference points to null", null, inner1);
  }

  @Persistable(id = 432442)
  public record References(
      Persistance.Reference<? extends Object> a,
      Persistance.Reference<? extends Object> b,
      Persistance.Reference<? extends Object> c) {}

  @Test
  public void referenceEquality1() throws Exception {
    var obj1 = UUID.randomUUID();
    var ra = Persistance.Reference.of(obj1);
    var rb = Persistance.Reference.of(obj1);
    var rc = Persistance.Reference.of(obj1);
    var obj2 = new References(ra, rb, rc);

    assertEquals("Same as they point to the same object", ra, rb);
    assertEquals("Same as they point to the same object", rb, rc);

    var arr = Persistance.write(obj2, (Function<Object, Object>) null);
    var loaded1 = Persistance.read(arr, (Function<Object, Object>) null).get(References.class);

    var r1 = loaded1.a();
    var r2 = loaded1.b();

    assertTrue("Not same", r1 != r2);
    assertEquals("But equal", r1, r2);
    assertEquals("With same hash code", r1.hashCode(), r2.hashCode());
  }
}
