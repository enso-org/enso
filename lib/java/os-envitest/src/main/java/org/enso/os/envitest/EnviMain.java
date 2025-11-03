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

  /** This is a message that os-environment/test sends from "master SVM" to "slave SVM". */
  @Persistable(id = 4332)
  public static record Hello(String name) implements Function<Channel<EnviMain>, Text> {
    @Override
    public Text apply(Channel<EnviMain> ch) {
      // now we are running in "slave SVM" and sending a message back to "master SVM"
      var withTitle = ch.execute(Text.class, new Title(name));
      return new Text("Hello " + withTitle.msg() + "!");
    }
  }

  /** This is a message sent from "slave SVM" back to "master SVM". */
  @Persistable(id = 4333)
  public static record Title(String name) implements Function<Object, Text> {
    @Override
    public Text apply(Object obj) {
      return new Text("Mr. " + name);
    }
  }

  @Persistable(id = 4334)
  public static record Text(String msg) {}
}
