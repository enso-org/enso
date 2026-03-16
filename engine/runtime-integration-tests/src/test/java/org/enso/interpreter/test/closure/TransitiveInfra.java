package org.enso.interpreter.test.closure;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collection;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.enso.logger.ObservedMessage;
import org.enso.test.utils.ContextUtils;
import org.junit.Rule;
import org.junit.Test;
import org.slf4j.LoggerFactory;

public abstract class TransitiveInfra {
  protected abstract String moduleName();

  protected abstract String code();

  protected abstract int minimumOfModules();

  protected abstract int maximumOfModules();

  protected abstract Collection<String> disallowModules();

  @Rule public ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public final void executeAndCheckResolvedModules() throws Exception {
    var disallowed = disallowModules();
    var logger = LoggerFactory.getLogger("org.enso.compiler.phase.ImportResolver");
    var counters = new int[2];
    var log = new StringBuilder();
    Consumer<ObservedMessage> observe =
        (msg) -> {
          if (msg.getMessage().startsWith("ANALYZE")) {
            var moduleName = msg.getArguments().get(0).toString();
            if (moduleName.contains("Data.Text")) {
              System.err.println("Now!!!! analyzing: " + moduleName);
              try {
                java.lang.Thread.sleep(100);
              } catch (InterruptedException ex) {
                throw new IllegalStateException(ex);
              }
            }
          }
          if (msg.getMessage().startsWith("TRANSITIVE")) {
            counters[0]++;
            log.append(msg.getFormattedMessage()).append("\n");

            var moduleName = msg.getArguments().get(0).toString();
            var dependsOn = (Object[]) msg.getArguments().get(1);
            var containsForbidden =
                Stream.of(dependsOn)
                    .filter(
                        n -> {
                          return disallowed.contains(n.toString());
                        })
                    .findAny();
            if (containsForbidden.isPresent()) {
              fail(
                  "Module "
                      + containsForbidden.get()
                      + " is disallowed, but "
                      + moduleName
                      + " depends on it!\n"
                      + log);
            }
          }
        };
    try (var handle = ObservedMessage.observe(logger, observe)) {
      var actual = ctx.evalModule(code(), getClass().getSimpleName() + ".enso", "main");
      assertTrue("It is a type: " + actual, actual.isMetaObject());
      var name = actual.getMetaQualifiedName();
      assertEquals("Module imported", moduleName(), name);
    }

    if (minimumOfModules() > counters[0] || counters[0] > maximumOfModules()) {
      fail("Wrong number of loaded modules: " + counters[0] + "\n" + log);
    }
  }
}
