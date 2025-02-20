package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.openide.util.lookup.ServiceProvider;
import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;

public class IrPersistanceTest {
  @Before
  public void resetDebris() {
    LazyString.forbidden = false;
  }

  @Test
  public void locationTest() throws Exception {
    var l = new Location(12, 33);
    var n = serde(Location.class, l, 20);

    assertEquals(12, n.start());
    assertEquals(33, n.end());
    assertEquals(l.length(), n.length());
  }

  @Test
  public void identifiedLocation() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), null);
    var in = serde(IdentifiedLocation.class, il, 21);
    assertEquals(il, in);
  }

  @Test
  public void identifiedLocationWithUUID() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), UUID.randomUUID());
    var in = serde(IdentifiedLocation.class, il, 37);
    assertEquals("UUIDs are serialized at the moment", il, in);
  }

  @Test
  public void identifiedLocationNoUUID() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), UUID.randomUUID());
    Function<Object, Object> fn =
        (obj) ->
            switch (obj) {
              case UUID any -> null;
              default -> obj;
            };
    var in = serde(IdentifiedLocation.class, il, 21, fn);
    var withoutUUID = new IdentifiedLocation(il.location());
    assertEquals("UUIDs are no longer serialized", withoutUUID, in);
  }

  @Test
  public void refHolderWithUUID() throws Exception {
    var id = UUID.randomUUID();
    var il = new RefHolder(id);
    var in = serde(RefHolder.class, il, -1, null);
    assertEquals("UUID is the same", id, in.id().get(UUID.class));
  }

  @Test
  public void twiceHolderWithUUID() throws Exception {
    var id = UUID.randomUUID();
    var il = new RefHolder(id);
    var two = join(il, join(il, nil()));

    var in = serde(List.class, two, -1, null);
    assertEquals("Two elements", 2, in.size());

    var both = new ArrayList<RefHolder>();
    var it = in.iterator();
    while (it.hasNext()) {
      var elem = (RefHolder) it.next();
      assertEquals("UUID is the same", id, elem.id().get(UUID.class));
      both.add(elem);
    }
    var first = both.get(0);
    var second = both.get(1);
    assertSame("Holders are the same", first, second);
    assertSame("Values are the same", first.id(), second.id());
  }

  @Test
  public void refHolderNoUUID() throws Exception {
    var il = new IdHolder(UUID.randomUUID());
    Function<Object, Object> fn =
        (obj) ->
            switch (obj) {
              case UUID any -> null;
              default -> obj;
            };
    var in = serde(IdHolder.class, il, 13, fn);
    var withoutUUID = new IdHolder(null);
    assertEquals("UUIDs are no longer serialized", withoutUUID, in);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void scalaMap() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var in = scala.collection.immutable.Map$.MODULE$.empty().$plus(new Tuple2("Hi", idLoc1));

    var out = serde(scala.collection.immutable.Map.class, in, 45);

    assertEquals("One element", 1, out.size());
    assertEquals(in, out);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void scalaImmutableMapIsLazy() throws Exception {
    var s1 = new LazyString("Hello");
    var s2 = new LazyString("World");
    var in =
        (scala.collection.immutable.Map)
            scala.collection.immutable.Map$.MODULE$
                .empty()
                .$plus(new Tuple2("Hello", s1))
                .$plus(new Tuple2("World", s2));

    LazyString.forbidden = true;
    var out = (scala.collection.immutable.Map) serde(scala.collection.immutable.Map.class, in, 76);

    assertEquals("Two pairs element", 2, out.size());
    assertEquals("Two keys", 2, out.keySet().size());
    assertTrue(out.keySet().contains("Hello"));
    assertTrue(out.keySet().contains("World"));
    LazyString.forbidden = false;

    assertEquals(s1, out.get("Hello").get());
    assertEquals(s2, out.get("World").get());
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

    var out = serde(scala.collection.mutable.Map.class, in, 45);

    assertEquals("One element", 1, out.size());
    assertEquals(in, out);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void scalaSet() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var in = scala.collection.immutable.Set$.MODULE$.empty().$plus(idLoc1);

    var out = serde(scala.collection.immutable.Set.class, in, 33);

    assertEquals("One element", 1, out.size());
    assertEquals(in, out);
  }

  @Test
  public void scalaList() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var idLoc2 = new IdentifiedLocation(new Location(2, 4), UUID.randomUUID());
    var in = join(idLoc2, join(idLoc1, nil()));

    List out = serde(List.class, in, 66);

    assertEquals("Two elements", 2, out.size());
    assertEquals("UUIDs are serialized at the moment", idLoc2, out.head());
    assertEquals("Tail is the same", idLoc1, out.last());
  }

  @Test
  public void scalaListSharedRef() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var in = join(idLoc1, join(idLoc1, nil()));

    List out = serde(List.class, in, 41);

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
  public void lazyJavaList() throws Exception {
    var s1 = new LazyString("Hello");
    var s2 = new LazyString("World");

    var in = java.util.List.of(s1, s2);
    assertEquals("Seq with two elements created", 2, in.size());

    LazyString.forbidden = true;
    var out = serde(java.util.List.class, in, -1);
    assertEquals("Two elements", 2, out.size());
    LazyString.forbidden = false;

    assertEquals("Lazily deserialized s1", s1, out.get(0));
    assertNotSame("Lazily deserialized s1", s1, out.get(0));
    assertEquals("Lazily deserialized s2", s2, out.get(1));
    assertNotSame("Lazily deserialized s2", s2, out.get(1));
  }

  /**
   * The Scala sequence is not lazy because it is actually written (and thus also read) as a List.
   */
  @Test
  public void notLazyScalaSequence() throws Exception {
    var s1 = new LazyString("Hello");
    var s2 = new LazyString("World");

    var second = new boolean[1];
    @SuppressWarnings("unchecked")
    var in =
        (Seq<CharSequence>)
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

    LazyString.forbidden = true;
    try {
      serde(Seq.class, in, -1);
      fail("This should have failed with IllegalStateException");
    } catch (IllegalStateException e) {
      assertEquals("Cannot create LazyString right now!", e.getMessage());
    }

    // But will work once enabled:
    LazyString.forbidden = false;
    var out = serde(Seq.class, in, -1);

    assertEquals("Two elements", 2, out.size());
    assertEquals("deserialized s2", in.head(), out.head());
    assertNotSame("deserialized s2", in.head(), out.head());
    assertEquals("deserialized s1", in.last(), out.last());
    assertNotSame("deserialized s1", in.last(), out.last());
  }

  @Ignore
  @Test
  public void serializeModule() throws Exception {
    var meta = new MetadataStorage();
    var m = new Module(nil(), nil(), nil(), true, null, meta);

    var out = serde(Module.class, m, -1);

    assertEquals("Same", m, out);
  }

  @Test
  public void hashMap() throws Exception {
    var map = new HashMap<String, String>();
    map.put("one", "uno");
    map.put("two", "duo");
    map.put("ten", "tre");

    var out = serde(java.util.Map.class, map, -1);

    assertEquals("Same", map, out);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void hashMapIsLazy() throws Exception {
    var s1 = new LazyString("Hello");
    var s2 = new LazyString("World");
    var in = new HashMap<String, LazyString>();
    in.put("Hello", s1);
    in.put("World", s2);

    LazyString.forbidden = true;
    var out = serde(java.util.Map.class, in, 76);

    assertEquals("Two pairs element", 2, out.size());
    assertEquals("Two keys", 2, out.keySet().size());
    assertTrue(out.keySet().contains("Hello"));
    assertTrue(out.keySet().contains("World"));
    LazyString.forbidden = false;

    assertEquals(s1, out.get("Hello"));
    assertEquals(s2, out.get("World"));
    assertEquals(in, out);
  }

  @Test
  public void inlineReferenceIsLazy() throws Exception {
    var s1 = new LazyString("Hello");
    var in = new InlineReferenceHolder(Persistance.Reference.of(s1, false));

    LazyString.forbidden = true;
    InlineReferenceHolder out = serde(InlineReferenceHolder.class, in, -1);
    Persistance.Reference<CharSequence> ref = out.ref();
    LazyString.forbidden = false;

    assertEquals(s1, ref.get(Object.class));
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

  @Test
  public void nameLiteral() throws Exception {
    var loc = new IdentifiedLocation(new Location(5, 19), null);
    var in = new Name.Literal("anyName", true, loc, Option.empty(), new MetadataStorage());

    var out = serde(Name.Literal.class, in, -1);
    assertEquals("They are structurally equal", in, out);
    assertNotSame("But not ==", in, out);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
    return serde(clazz, l, expectedSize, null);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize, Function<Object, Object> fn)
      throws IOException {
    var arr = Persistance.write(l, fn);
    if (expectedSize >= 0) {
      assertEquals(expectedSize, arr.length - 12);
    }
    var ref = Persistance.read(arr, null);
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

  private static class LazyString implements CharSequence {

    private static boolean forbidden;

    private final String value;

    public LazyString(String value) {
      Objects.requireNonNull(value);
      if (forbidden) {
        throw new IllegalStateException("Cannot create LazyString right now!");
      }
      this.value = value;
    }

    @Override
    public char charAt(int index) {
      return value.charAt(index);
    }

    @Override
    public int length() {
      return value.length();
    }

    @Override
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
      final LazyString other = (LazyString) obj;
      return Objects.equals(this.value, other.value);
    }

    @Override
    public String toString() {
      return value;
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistLazyString extends Persistance<LazyString> {

    public PersistLazyString() {
      super(LazyString.class, false, 432432);
    }

    @Override
    protected void writeObject(LazyString obj, Output out) throws IOException {
      out.writeUTF(obj.value);
    }

    @Override
    protected LazyString readObject(Input in) throws IOException, ClassNotFoundException {
      var s = in.readUTF();
      return new LazyString(s);
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

  @Persistable(clazz = Service.class, id = 432434)
  public record Service(int value) implements Supplier<Integer> {
    @Override
    public Integer get() {
      return value;
    }
  }

  @Persistable(clazz = IntegerSupply.class, id = 432435)
  public record IntegerSupply(Supplier<Integer> supply) {}

  @Persistable(clazz = ServiceSupply.class, id = 432436)
  public record ServiceSupply(Service supply) {}

  @Persistable(clazz = IdHolder.class, id = 432876)
  public record IdHolder(UUID id) {}

  @Persistable(clazz = RefHolder.class, id = 436872)
  public record RefHolder(Persistance.Reference<UUID> id) {
    RefHolder(UUID id) {
      this(Persistance.Reference.of(id));
    }
  }

  @Persistable(clazz = InlineReferenceHolder.class, id = 432437)
  public record InlineReferenceHolder(Persistance.Reference<CharSequence> ref) {}
}
