package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.UUID;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.junit.Test;
import scala.Option;
import scala.collection.immutable.List;

public class PersistanceTest {
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
    var il = new IdentifiedLocation(new Location(5, 19), Option.empty());
    var in = serde(IdentifiedLocation.class, il, 8);
    assertEquals(il, in);
  }

  @Test
  public void identifiedLocationNoUUID() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), Option.apply(UUID.randomUUID()));
    var in = serde(IdentifiedLocation.class, il, 8);
    assertEquals(
        "UUIDs aren't serialized", new IdentifiedLocation(il.location(), Option.empty()), in);
  }

  @Test
  public void scalaList() throws Exception {
    var idLoc1 = new IdentifiedLocation(new Location(1, 5));
    var idLoc2 = new IdentifiedLocation(new Location(2, 4), Option.apply(UUID.randomUUID()));
    var in = join(idLoc2, join(idLoc1, nil()));

    var out = serde(List.class, in, 36);

    assertEquals("Two elements", 2, out.size());
    assertEquals("UUIDs aren't serialized", new IdentifiedLocation(idLoc2.location()), out.head());
    assertEquals("Tail is the same", idLoc1, out.last());
  }

  @Test
  public void serializeModule() throws Exception {
    var meta = new MetadataStorage(nil());
    var diag = new DiagnosticStorage(nil());
    var m = new Module(nil(), nil(), nil(), true, Option.empty(), meta, diag);

    var out = serde(Module.class, m, -1);

    assertEquals("Same", m, out);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
    var out = new ByteArrayOutputStream();
    var gen = Persistance.newGenerator(out);
    var at = gen.writeObject(l);
    var arr = out.toByteArray();
    var buf = ByteBuffer.wrap(arr);
    if (expectedSize >= 0) {
      assertEquals(expectedSize, arr.length);
    }
    var ref = Persistance.Reference.from(buf, at);
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
}
