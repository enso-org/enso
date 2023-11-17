package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.HashMap;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;

import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.junit.Test;
import org.openide.util.lookup.ServiceProvider;

import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;

public class IrPersistanceTest {
  @Test
  public void locationTest() throws Exception {
    var l = new Location(12, 33);
    var n = serde(Location.class, l, 8);

    assertEquals(12, n.start());
    assertEquals(33, n.end());
    assertEquals(l.length(), n.length());
  }

  @Test
  public void identifiedLocation() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), null);
    var in = serde(IdentifiedLocation.class, il, 12);
    assertEquals(il, in);
  }

  @Test
  public void identifiedLocationNoUUID() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), UUID.randomUUID());
    var in = serde(IdentifiedLocation.class, il, 32);
    assertEquals("UUIDs are serialized at the moment", il, in);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void scalaMap() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var in = scala.collection.immutable.Map$.MODULE$.empty().$plus(new Tuple2("Hi", idLoc1));

    var out = serde(scala.collection.immutable.Map.class, in, 36);

    assertEquals("One element", 1, out.size());
    assertEquals(in, out);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void scalaHashMap() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var immutable = join(new Tuple2("Hi", idLoc1), nil());
    var in =
        (scala.collection.mutable.HashMap)
            scala.collection.mutable.HashMap$.MODULE$.apply(immutable);

    var out = serde(scala.collection.mutable.Map.class, in, 36);

    assertEquals("One element", 1, out.size());
    assertEquals(in, out);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void scalaSet() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var in = scala.collection.immutable.Set$.MODULE$.empty().$plus(idLoc1);

    var out = serde(scala.collection.immutable.Set.class, in, 24);

    assertEquals("One element", 1, out.size());
    assertEquals(in, out);
  }

  @Test
  public void scalaList() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var idLoc2 = new IdentifiedLocation(new Location(2, 4), UUID.randomUUID());
    var in = join(idLoc2, join(idLoc1, nil()));

    var out = serde(List.class, in, 64);

    assertEquals("Two elements", 2, out.size());
    assertEquals("UUIDs are serialized at the moment", idLoc2, out.head());
    assertEquals("Tail is the same", idLoc1, out.last());
  }

  @Test
  public void scalaListSharedRef() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var in = join(idLoc1, join(idLoc1, nil()));

    var out = serde(List.class, in, 32);

    assertEquals("Two elements", 2, out.size());
    assertEquals("Head is equal to original", idLoc1, out.head());
    assertEquals("Tail is equal to original", idLoc1, out.last());
    assertSame("Head and tail are the same", out.head(), out.last());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void scalaListSharedRefAtDepth() throws Exception {
    var idLoc1 = Singleton.INSTANCE;
    var in = join(Option.apply(idLoc1), join(Option.apply(idLoc1), nil()));

    var out = serde(List.class, in, -1);

    assertEquals("Two elements", 2, out.size());
    var readHead = (Option<Singleton>) out.head();
    var readTail = (Option<Singleton>) out.last();

    assertEquals("Head is equal to original", idLoc1, readHead.get());
    assertEquals("Tail is equal to original", idLoc1, readTail.get());

    assertNotSame("Head and tail are different", readHead, readTail);
    assertSame("Head and tail are the same", readHead.get(), readTail.get());
  }

  @Test
  public void lazyScalaSequence() throws Exception {
    var s1 = new LazySeq("Hello");
    var s2 = new LazySeq("World");

    var second = new boolean[1];
    @SuppressWarnings("unchecked")
    var in =
        (Seq<String>)
            Seq.fill(
                2,
                () -> {
                  if (second[0]) {
                    return s2;
                  }
                  second[0] = true;
                  return s1;
                });
    assertEquals("Seq with two elements created", 2, in.length());

    LazySeq.forbidden = true;
    var out = serde(Seq.class, in, -1);

    assertEquals("Two elements", 2, out.size());

    LazySeq.forbidden = false;

    assertEquals("Lazily deserialized s2", s2, out.head());
    assertNotSame("Lazily deserialized s2", s2, out.head());
    assertEquals("Lazily deserialized s1", s1, out.last());
    assertNotSame("Lazily deserialized s1", s1, out.head());
  }

  @Test
  public void serializeModule() throws Exception {
    var meta = new MetadataStorage(nil());
    var diag = new DiagnosticStorage(nil());
    var m = new Module(nil(), nil(), nil(), true, Option.empty(), meta, diag);

    var out = serde(Module.class, m, -1);

    assertEquals("Same", m, out);
  }

  @Test
  public void hashMap() throws Exception {
    var map = new HashMap<String, String>();
    map.put("one", "uno");
    map.put("two", "duo");
    map.put("ten", "tre");

    var out = serde(HashMap.class, map, -1);

    assertEquals("Same", map, out);
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

  @SuppressWarnings("unchecked")
  private static final <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }

  private static final <T> scala.collection.immutable.List<T> join(
      T head, scala.collection.immutable.List<T> tail) {
    return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
  }

  private static class LazySeq implements CharSequence {

    private static boolean forbidden;

    private final String value;

    public LazySeq(String value) {
      if (forbidden) {
        throw new IllegalStateException("Cannot create LazySeq right now!");
      }
      this.value = value;
    }

    public char charAt(int index) {
      return value.charAt(index);
    }

    public int length() {
      return value.length();
    }

    public CharSequence subSequence(int beginIndex, int endIndex) {
      return value.subSequence(beginIndex, endIndex);
    }

    @Override
    public int hashCode() {
      int hash = 5;
      hash = 53 * hash + Objects.hashCode(this.value);
      return hash;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      final LazySeq other = (LazySeq) obj;
      return Objects.equals(this.value, other.value);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistLazySeq extends Persistance<LazySeq> {

    public PersistLazySeq() {
      super(LazySeq.class, false, 432432);
    }

    @Override
    protected void writeObject(LazySeq obj, Output out) throws IOException {
      out.writeUTF(obj.value);
    }

    @Override
    protected LazySeq readObject(Input in) throws IOException, ClassNotFoundException {
      var s = in.readUTF();
      return new LazySeq(s);
    }
  }

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

  @Persistable(clazz=Service.class, id=432434)
  public record Service(int value) implements Supplier<Integer> {
    @Override
    public Integer get() {
      return value;
    }
  }

  @Persistable(clazz=IntegerSupply.class, id=432435)
  public record IntegerSupply(Supplier<Integer> supply) {}

  @Persistable(clazz=ServiceSupply.class, id=432436)
  public record ServiceSupply(Service supply) {}
}
