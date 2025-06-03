package org.enso.os.environment.jni;

import java.io.File;
import java.io.FileWriter;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
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
  static final class RequestFactorial extends Channel.Message<Void> {
    private long n;

    RequestFactorial(long n) {
      super(Void.class);
      this.n = n;
    }

    @Override
    protected Void evaluate(Channel channel) throws Throwable {
      var res = factorial(n).toString();
      channel.execute(new ReportResult(n, res));
      return null;
    }

    long n() {
      return n;
    }
  }

  @Persistable(id = 430608)
  static final class ComputeFactorial extends Channel.Message<BigInteger> {
    private long n;

    ComputeFactorial(long n) {
      super(BigInteger.class);
      this.n = n;
    }

    @Override
    protected BigInteger evaluate(Channel channel) throws Throwable {
      var res = factorial(n);
      return res;
    }

    long n() {
      return n;
    }
  }

  @Persistable(id = 430606)
  static final class ReportResult extends Channel.Message<Void> {
    private final long key;
    private final String value;

    ReportResult(long key, String value) {
      super(Void.class);
      this.key = key;
      this.value = value;
    }

    public long key() {
      return key;
    }

    public String value() {
      return value;
    }

    @Override
    protected Void evaluate(Channel otherVM) throws Throwable {
      var vm = System.getProperty("java.vm.name");
      assert "Substrate VM".equals(vm) : "Running in SVM again: " + vm;
      CORRECT_RESULTS.put(key, value);
      return null;
    }
  }

  @Persistable(id = 430609)
  static final class CountDownAndReturn extends Channel.Message<Long> {
    private final long value;
    private final long acc;

    CountDownAndReturn(long value, long acc) {
      super(Long.class);
      this.value = value;
      this.acc = acc;
    }

    long value() {
      return value;
    }

    long acc() {
      return acc;
    }

    @Override
    protected Long evaluate(Channel otherVM) throws Throwable {
      if (value <= 1) {
        return acc;
      } else {
        return otherVM.execute(new CountDownAndReturn(value - 1, acc * value));
      }
    }
  }
}
