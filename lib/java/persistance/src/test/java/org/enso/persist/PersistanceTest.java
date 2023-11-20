package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;

import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
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

    var multiOnRead = Persistance.read(arr, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);
    assertEquals("Multiplied on read", 15, multiOnRead.get(Service.class).value());
  }

  @Test
  public void writeReplace() throws Exception {
    var in = new Service(5);
    var arr = Persistance.write(in, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Multiplied on write", 15, plain.get(Service.class).value());
  }

  @Test
  public void readResolveInline() throws Exception {
    var in = new ServiceSupply(new Service(5));
    var arr = Persistance.write(in, (Function<Object, Object>) null);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Remains five", 5, plain.get(ServiceSupply.class).supply().value());

    var multiOnRead = Persistance.read(arr, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);
    assertEquals("Multiplied on read", 15, multiOnRead.get(ServiceSupply.class).supply().value());
  }

  @Test
  public void writeReplaceInline() throws Exception {
    var in = new ServiceSupply(new Service(5));
    var arr = Persistance.write(in, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);

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

    var multiOnRead = Persistance.read(arr, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);
    assertEquals("Multiplied on read", 15, (int) multiOnRead.get(IntegerSupply.class).supply().get());
  }

  @Test
  public void writeReplaceReference() throws Exception {
    var in = new IntegerSupply(new Service(5));
    var arr = Persistance.write(in, (obj) -> obj instanceof Service s ? new Service(s.value() * 3) : obj);

    var plain = Persistance.read(arr, (Function<Object, Object>) null);
    assertEquals("Multiplied on write", 15, (int) plain.get(IntegerSupply.class).supply().get());
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
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
  @Persistable(clazz=Service.class, id=432434)
  @Persistable(clazz=IntegerSupply.class, id=432435)
  public record Service(int value) implements Supplier<Integer> {
    @Override
    public Integer get() {
      return value;
    }
  }
  public record IntegerSupply(Supplier<Integer> supply) {}
  // @end region="annotation"

  // @start region="self-annotation"
  @Persistable(id=432436)
  public record ServiceSupply(Service supply) {}
  // @end region="self-annotation"
}
