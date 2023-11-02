package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.UUID;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Location;
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

  private static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
    var buf = ByteBuffer.allocate(512);
    var gen = Persistance.newGenerator(buf);
    var ref = gen.writeObject(l);
    buf.flip();
    if (expectedSize >= 0) {
      assertEquals(expectedSize, buf.limit());
    }
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
