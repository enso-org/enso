package org.enso.jvm.channel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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

  @Test
  public void smallText() {
    var ch = Channel.create(null, PrivateData.class);

    var msg = new GenerateString(256);
    var newMsg = ch.execute(LongString.class, msg);

    assertEquals(newMsg.text(), 256, newMsg.text().length());
  }

  @Test
  public void longText() {
    var ch = Channel.create(null, PrivateData.class);

    var msg = new GenerateString(32632);
    var newMsg = ch.execute(LongString.class, msg);

    assertEquals(newMsg.text(), 32632, newMsg.text().length());
  }

  @Test
  public void exceptionIsThrows() {
    var ch = Channel.create(null, PrivateData.class);

    var msg = new GenerateString(-73);
    try {
      var newMsg = ch.execute(LongString.class, msg);
      fail("Not expecting a return value: " + newMsg);
    } catch (IllegalArgumentException ex) {
      assertEquals("Length must be positive. Was: -73", ex.getMessage());
      var stackTop = ex.getStackTrace()[0];
      assertEquals(GenerateString.class.getName(), stackTop.getClassName());
      assertEquals("handleGenerationOfStrings", stackTop.getMethodName());
    }
  }

  @Test
  public void throwFactorialOne() throws Exception {
    assertException("1", new CountDownAndThrow(1, 1));
  }

  @Test
  public void throwFactorialTwo() throws Exception {
    assertException("2", new CountDownAndThrow(2, 1));
  }

  @Test
  public void throwFactorialThree() throws Exception {
    assertException("6", new CountDownAndThrow(3, 1));
  }

  @Test
  public void throwFactorialFour() throws Exception {
    assertException("24", new CountDownAndThrow(4, 1));
  }

  @Test
  public void throwFactorialFive() throws Exception {
    assertException("120", new CountDownAndThrow(5, 1));
  }

  private void assertException(String msg, CountDownAndThrow action) {
    var channel = Channel.create(null, PrivateData.class);
    try {
      channel.execute(Void.class, action);
      fail("Expecting an exception to be thrown for " + msg);
    } catch (IllegalStateException ex) {
      assertEquals(msg, ex.getMessage());
      var countDecrementAndSendMessage = 0;
      for (var elem : ex.getStackTrace()) {
        if ("decrementAndSendMessage".equals(elem.getMethodName())) {
          assertEquals("ChannelInSingleJvmTest.java", elem.getFileName());
          assertNotEquals(-1, elem.getLineNumber());
          assertEquals(action.getClass().getName(), elem.getClassName());
          countDecrementAndSendMessage++;
        }
      }
      if (action.value() != countDecrementAndSendMessage) {
        ex.printStackTrace();
        assertEquals(
            "There is exactly right amount of invocations",
            action.value(),
            countDecrementAndSendMessage);
      }
    }
  }

  @Test
  public void verifyStopMethodNameReferencesRealMethodName() throws Exception {
    var stopMethodField = Channel.class.getDeclaredField("STOP_METHOD_NAME");
    stopMethodField.setAccessible(true);
    var stopMethodValue = stopMethodField.get(null);
    for (var m : Channel.class.getDeclaredMethods()) {
      if (m.getName().equals(stopMethodValue)) {
        return;
      }
    }
    fail("STOP_METHOD_NAME field value should be consistent with method name");
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

  @Persistable(id = 8343)
  static record GenerateString(int lengthToGenerate)
      implements Function<Channel<PrivateData>, LongString> {
    @Override
    public LongString apply(Channel<PrivateData> t) {
      return handleGenerationOfStrings(lengthToGenerate);
    }

    private static LongString handleGenerationOfStrings(int len) {
      if (len < 0) {
        throw new IllegalArgumentException("Length must be positive. Was: " + len);
      }
      return new LongString(len);
    }
  }

  @Persistable(id = 8344)
  static record LongString(String text) {
    private LongString(int len) {
      this("Hello".repeat(len / 5) + "!!!!!".substring(5 - len % 5));
    }
  }

  @Persistable(id = 8345)
  record CountDownAndThrow(long value, long acc) implements Function<Channel<?>, Void> {
    @Override
    public Void apply(Channel<?> otherVM) {
      decrementAndSendMessage(value, acc, otherVM);
      return null;
    }

    private static void decrementAndSendMessage(long n, long sum, Channel<?> otherVM) {
      if (n <= 1) {
        throw new IllegalStateException("" + sum);
      } else {
        otherVM.execute(Void.class, new CountDownAndThrow(n - 1, sum * n));
      }
    }
  }
}
