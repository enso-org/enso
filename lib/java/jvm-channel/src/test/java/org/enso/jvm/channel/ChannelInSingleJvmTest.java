package org.enso.jvm.channel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.function.Function;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.junit.Test;

public class ChannelInSingleJvmTest {
  public static final class PrivateData extends Channel.Config {
    static int countInstances;
    int counter;

    public PrivateData() {
      countInstances++;
    }

    @Override
    public Persistance.Pool createPool(Channel<?> ignore) {
      return Persistables.POOL;
    }
  }

  @Test
  public void exchangeMessageThatModifiesItself() {
    var ch = Channel.create(null, PrivateData.class);
    assertTrue("The created channel is a master", ch.isMaster());

    var msg = new Increment(10);

    var newMsg = ch.execute(Increment.class, msg);

    assertNotNull("Got a value", newMsg);
    assertEquals("10 + 1", 11, newMsg.valueToIncrement());
    assertEquals("Original value remains", 10, msg.valueToIncrement());
  }

  @Test
  public void exchangeMessageThatModifiesPrivateData() {
    PrivateData.countInstances = 0;
    var ch = Channel.create(null, PrivateData.class);
    assertEquals("Two channels & data created", 2, PrivateData.countInstances);
    assertEquals("By default we are at zero", 0, ch.getConfig().counter);

    var msg = new AssignPrivateData(10);
    var newMsg = ch.execute(AssignPrivateData.class, msg);

    assertEquals("PrivateData.counter hasn't been changed", 0, ch.getConfig().counter);

    assertNotNull("Got a value", newMsg);
    assertEquals("10 + 1", 11, newMsg.valueToSet());
    assertEquals("Original value remains", 10, msg.valueToSet());
  }

  @Persistable(id = 8341)
  static final class Increment implements Function<Channel<?>, Increment> {
    int valueToIncrement;

    Increment(int valueToIncrement) {
      this.valueToIncrement = valueToIncrement;
    }

    int valueToIncrement() {
      return valueToIncrement;
    }

    @Override
    public Increment apply(Channel<?> channel) {
      valueToIncrement++;
      assertFalse("We are processed in the slave", channel.isMaster());
      return this;
    }
  }

  @Persistable(id = 8342)
  static record AssignPrivateData(int valueToSet)
      implements Function<Channel<PrivateData>, AssignPrivateData> {
    @Override
    public AssignPrivateData apply(Channel<PrivateData> t) {
      t.getConfig().counter = valueToSet;
      return new AssignPrivateData(t.getConfig().counter + 1);
    }
  }
}
