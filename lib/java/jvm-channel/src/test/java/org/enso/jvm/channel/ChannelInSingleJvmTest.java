package org.enso.jvm.channel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.function.Function;
import org.enso.persist.Persistable;
import org.junit.Test;

public class ChannelInSingleJvmTest {
  @Test
  public void exchangeMessages() {
    var ch = Channel.create(null, Persistables.class);

    var msg = new Increment(10);

    var newMsg = ch.execute(Increment.class, msg);

    assertNotNull("Got a value", newMsg);
    assertEquals("10 + 1", 11, newMsg.valueToIncrement());
    assertEquals("Original value remains", 10, msg.valueToIncrement());
  }

  @Persistable(id = 8341)
  static final class Increment implements Function<Channel, Increment> {
    int valueToIncrement;

    Increment(int valueToIncrement) {
      this.valueToIncrement = valueToIncrement;
    }

    int valueToIncrement() {
      return valueToIncrement;
    }

    @Override
    public Increment apply(Channel t) {
      valueToIncrement++;
      return this;
    }
  }
}
