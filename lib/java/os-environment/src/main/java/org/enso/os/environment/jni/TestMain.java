package org.enso.os.environment.jni;

import java.io.File;
import java.io.FileWriter;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;

final class TestMain {
  static final Map<Long, String> CORRECT_RESULTS = new HashMap<>();

  private TestMain() {}

  public static void main(String... args) throws Exception {
    var out = new File(args[0]);
    var n = Integer.parseInt(args[1]);
    try (java.io.FileWriter os = new FileWriter(out)) {
      os.write(factorial(n).toString());
    }
  }

  static BigInteger factorial(long n) {
    var acc = BigInteger.valueOf(1);
    for (; ; ) {
      acc = acc.multiply(BigInteger.valueOf(n));
      if (--n == 0) {
        break;
      }
    }
    return acc;
  }

  @Persistable(id = 430607)
  record RequestFactorial(long n) implements Function<Channel<?>, Void> {
    @Override
    public Void apply(Channel<?> channel) {
      assert !channel.isMaster() : "Requesting factorial is handled in the slave only";
      var res = factorial(n).toString();
      channel.execute(Void.class, new ReportResult(n, res));
      return null;
    }
  }

  @Persistable(id = 430608)
  record ComputeFactorial(long n) implements Function<Object, BigInteger> {
    @Override
    public BigInteger apply(Object ignore) {
      var res = factorial(n);
      return res;
    }
  }

  @Persistable(id = 430606)
  record ReportResult(long key, String value) implements Function<Channel, Void> {
    @Override
    public Void apply(Channel otherVM) {
      var vm = System.getProperty("java.vm.name");
      assert "Substrate VM".equals(vm) : "Running in SVM again: " + vm;
      CORRECT_RESULTS.put(key, value);
      return null;
    }
  }

  @Persistable(id = 430609)
  record CountDownAndReturn(long value, long acc) implements Function<Channel<?>, Long> {
    @Override
    public Long apply(Channel<?> otherVM) {
      if (value <= 1) {
        return acc;
      } else {
        return otherVM.execute(Long.class, new CountDownAndReturn(value - 1, acc * value));
      }
    }
  }

  @Persistable(id = 430610)
  record CountDownAndThrow(long value, long acc) implements Function<Channel<?>, Void> {
    @Override
    public Void apply(Channel<?> otherVM) {
      if (value <= 1) {
        throw new IllegalStateException("" + acc);
      } else {
        return otherVM.execute(Void.class, new CountDownAndThrow(value - 1, acc * value));
      }
    }
  }
}
