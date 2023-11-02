package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.ByteBuffer;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Location;
import org.junit.Test;
import scala.Option;

public class PersistanceTest {
  @Test
  public void locationTest() throws Exception {
    var l = new Location(12, 33);
    var n = serde(Location.class, l);

    assertEquals(12, n.start());
    assertEquals(33, n.end());
    assertEquals(l.length(), n.length());
  }

  @Test
  public void identifiedLocation() throws Exception {
    var il = new IdentifiedLocation(new Location(5, 19), Option.empty());
    var in = serde(IdentifiedLocation.class, il);
    assertEquals(il, in);
  }

  private static <T> T serde(Class<T> clazz, T l) throws IOException {
    assertEquals(clazz, l.getClass());
    var buf = ByteBuffer.allocate(512);
    var gen = Persistance.newGenerator(buf);
    var ref = gen.writeObject(l);
    return ref.get(clazz);
  }
}
