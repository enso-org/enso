package org.enso.os.environment.jni;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.math.BigInteger;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.enso.jvm.channel.Channel;
import org.enso.jvm.channel.JVM;
import org.enso.os.environment.lib.HelloTitle;
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
  public void factorialInSecondThread() throws Exception {
    var pool = Executors.newSingleThreadExecutor();
    var v =
        pool.submit(
            () -> {
              var fac = channel.execute(Long.class, new TestMain.CountDownAndReturn(5, 1));
              return fac;
            });
    assertEquals(120, v.get().longValue());
    pool.shutdown();
    pool.awaitTermination(10, TimeUnit.SECONDS);
  }

  @Test
  public void factorialInManyThreads() throws Exception {
    var pool = Executors.newFixedThreadPool(30);
    executeInParallel(1000, pool);
  }

  private void executeInParallel(int count, ExecutorService pool)
      throws ExecutionException, InterruptedException {
    var futures = new ArrayList<Future<Long>>();
    for (int i = 0; i < count; i++) {
      var v =
          pool.submit(
              () -> {
                var fac = channel.execute(Long.class, new TestMain.CountDownAndReturn(5, 1));
                return fac;
              });
      futures.add(v);
    }
    for (var v : futures) {
      assertEquals(120, v.get().longValue());
    }
    pool.shutdown();
    pool.awaitTermination(10, TimeUnit.SECONDS);
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

  @Test
  public void loadFromDynamicLibrary() throws Exception {
    var libPath = System.getenv("OS_ENVIRONMENT_LIB");
    var libFile = new File(libPath);
    assert libFile.isFile() : "Library file must exists at " + libPath;
    var nativeJvm = JVM.create(libFile);
    var tmp = File.createTempFile("nativelib", ".msg");
    var hello = "Hello from native lib!";
    nativeJvm.executeMain("org/enso/os/environment/lib/HelloTitle", tmp.getAbsolutePath(), hello);
    var content = Files.readString(tmp.toPath());
    tmp.delete();
    assertEquals("Proper message has been written into " + tmp, hello, content);
  }

  @Test
  public void loadChannelFromDynamicLibrary() throws Exception {
    var libPath = System.getenv("OS_ENVIRONMENT_LIB");
    assertNotNull("Set OS_ENVIRONMENT_LIB env variable!", libPath);
    var libFile = new File(libPath);
    assert libFile.isFile() : "Library file must exists at " + libPath;
    var nativeJvm = JVM.create(libFile);
    var ch = Channel.create(nativeJvm, HelloTitle.class);
    var fac = ch.execute(HelloTitle.Text.class, new HelloTitle.Hello("Native"));
    assertEquals("Hello Mr. Native!", fac.msg());
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
