package org.enso.os.envitest;

import java.io.FileWriter;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;

/** Entry point for a "isolate library" to be loaded and communicated to via a {@link Channel}. */
public final class EnviMain extends Channel.Config {
  public static void main(String... args) throws Exception {
    try (java.io.FileWriter out = new FileWriter(args[0])) {
      out.write(args[1]);
    }
  }

  @Override
  public Persistance.Pool createPool(Channel<?> channel) {
    return Persistables.POOL;
  }

  @Persistable(id = 4332)
  public static record Hello(String msg) implements Function<Object, Hello> {
    @Override
    public Hello apply(Object obj) {
      return new Hello("Hello " + msg + "!");
    }
  }
}
