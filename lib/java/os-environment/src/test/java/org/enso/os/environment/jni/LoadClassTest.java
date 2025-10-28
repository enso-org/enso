package org.enso.os.environment.jni;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.math.BigInteger;
import java.nio.file.Files;
import java.util.Random;
import org.enso.jvm.channel.Channel;
import org.enso.jvm.channel.JVM;
import org.junit.Before;
import org.junit.Test;

public class LoadClassTest {
  private static final int MAX = 3000;
  private static final int MIN = 300;
  private static final String PATH = System.getProperty("java.home");
  // set from TestCollectorFeature
  public static String MODULE_PATH;

  private static JVM impl;

  private static JVM jvm() {
    if (impl == null) {
      assert MODULE_PATH != null : "MODULE_PATH field must be set!";
      var path = new File(PATH);
      assert path.isDirectory() : "Java home exists: " + path;
      impl =
          JVM.create(
              path,
              "--module-path=" + MODULE_PATH,
              "--enable-native-access=org.enso.os.environment",
              "-Djdk.module.main=org.enso.os.environment",
              "-Dsay=Ahoj");
    }
    return impl;
  }

  private Channel<JVMPeer> channel;

  @Before
  public void initializeChannel() throws Exception {
    channel = Channel.create(jvm(), JVMPeer.class);
    assertTrue("Created channel is master", channel.isMaster());
  }

  @Test
  public void executeMainClass() throws Exception {
    var out = File.createTempFile("check-main", ".log");
    var gen = new Random();
    for (var i = 0; i < 5; i++) {
      var n = gen.nextInt(MIN, MAX);
      jvm().executeMain("org/enso/os/environment/jni/TestMain", out.getPath(), "" + n);
      var content = Files.readString(out.toPath());
      assertEquals("Factorial of " + n + " is the same", TestMain.factorial(n).toString(), content);
      out.delete();
    }
  }

  @Test
  public void computeFactorialViaMessages() throws Exception {
    TestMain.CORRECT_RESULTS.clear();
    assertEquals("Results are empty", 0, TestMain.CORRECT_RESULTS.size());
    var gen = new Random();
    var n = 0L;
    for (var i = 0; i < 5; i++) {
      n += gen.nextLong(MIN, MAX);
      channel.execute(Void.class, new TestMain.RequestFactorial(n));
    }
    assertEquals(
        "Five results found: " + TestMain.CORRECT_RESULTS, 5, TestMain.CORRECT_RESULTS.size());
    for (var e : TestMain.CORRECT_RESULTS.entrySet()) {
      var expecting = TestMain.factorial(e.getKey());
      assertEquals("fac(" + e.getKey() + ") should be", expecting.toString(), e.getValue());
    }
  }

  @Test
  public void computeFactorialViaSingleMessage() throws Exception {
    var gen = new Random();
    var n = 0L;
    for (var i = 0; i < 5; i++) {
      n += gen.nextLong(MIN, MAX);
      var res = channel.execute(BigInteger.class, new TestMain.ComputeFactorial(n));
      var expecting = TestMain.factorial(n);
      assertEquals("fac(" + n + ") should be", expecting, res);
    }
  }

  @Test
  public void backAndForthFactorialOne() throws Exception {
    var fac = channel.execute(Long.class, new TestMain.CountDownAndReturn(1, 1));
    assertEquals(1, fac.longValue());
  }

  @Test
  public void backAndForthFactorialTwo() throws Exception {
    var fac = channel.execute(Long.class, new TestMain.CountDownAndReturn(2, 1));
    assertEquals(2, fac.longValue());
  }

  @Test
  public void backAndForthFactorialThree() throws Exception {
    var fac = channel.execute(Long.class, new TestMain.CountDownAndReturn(3, 1));
    assertEquals(6, fac.longValue());
  }

  @Test
  public void backAndForthFactorialFour() throws Exception {
    var fac = channel.execute(Long.class, new TestMain.CountDownAndReturn(4, 1));
    assertEquals(24, fac.longValue());
  }

  @Test
  public void backAndForthFactorialFive() throws Exception {
    var fac = channel.execute(Long.class, new TestMain.CountDownAndReturn(5, 1));
    assertEquals(120, fac.longValue());
  }

  @Test
  public void throwFactorialOne() throws Exception {
    assertException("1", new TestMain.CountDownAndThrow(1, 1));
  }

  @Test
  public void throwFactorialTwo() throws Exception {
    assertException("2", new TestMain.CountDownAndThrow(2, 1));
  }

  @Test
  public void throwFactorialThree() throws Exception {
    assertException("6", new TestMain.CountDownAndThrow(3, 1));
  }

  @Test
  public void throwFactorialFour() throws Exception {
    assertException("24", new TestMain.CountDownAndThrow(4, 1));
  }

  @Test
  public void throwFactorialFive() throws Exception {
    assertException("120", new TestMain.CountDownAndThrow(5, 1));
  }

  private void assertException(String msg, TestMain.CountDownAndThrow action) {
    try {
      channel.execute(Void.class, action);
      fail("Expecting an exception to be thrown for " + msg);
    } catch (IllegalStateException ex) {
      assertEquals(msg, ex.getMessage());
    }
  }
}
