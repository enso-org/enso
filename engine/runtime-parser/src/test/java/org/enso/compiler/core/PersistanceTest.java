package org.enso.compiler.core;

import static org.junit.Assert.*;

import java.nio.ByteBuffer;
import org.enso.compiler.core.ir.Location;
import org.junit.Test;

public class PersistanceTest {
  @Test
  public void locationTest() throws Exception {
    var l = new Location(12, 33);

    var buf = ByteBuffer.allocate(512);
    var gen = Persistance.newGenerator(buf);
    gen.writeObject(l);

    var ref = Persistance.Reference.from(buf.flip(), 0);
    Location n = ref.get(Location.class);

    assertEquals(12, n.start());
    assertEquals(33, n.end());
    assertEquals(l.length(), n.length());
  }
}
