package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.UUID;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Location;
import org.junit.Test;
import scala.Option;

public class PersistanceTest {
  @Test
  public void locationTest() throws Exception {
    var l = new Location(12, 33);
    var n = serde(Location.class, l, 12);

    assertEquals(12, n.start());
    assertEquals(33, n.end());
    assertEquals(l.length(), n.length());
  }

  @Test
  public void identifiedLocation() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), Option.empty());
    var in = serde(IdentifiedLocation.class, il, 20);
    assertEquals(il, in);
  }

  @Test
  public void identifiedLocationNoUUID() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), Option.apply(UUID.randomUUID()));
    var in = serde(IdentifiedLocation.class, il, 20);
    assertEquals(
        "UUIDs aren't serialized", new IdentifiedLocation(il.location(), Option.empty()), in);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
    assertEquals(clazz, l.getClass());
    var buf = ByteBuffer.allocate(512);
    var gen = Persistance.newGenerator(buf);
    var ref = gen.writeObject(l);
    buf.flip();
    if (expectedSize >= 0) {
      assertEquals(expectedSize, buf.limit());
    }
    return ref.get(clazz);
  }
}
