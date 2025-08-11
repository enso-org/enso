package org.enso.logger;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.function.Consumer;
import java.util.function.Supplier;
import org.slf4j.Logger;
import org.slf4j.event.Level;

/**
 * Represents a message logged in the system.Use {@link #observe} and/or {@link #collect} methods to
 * obtain these messages.
 *
 * @see Service
 */
public final class ObservedMessage {
  private final Level level;
  private final Instant instant;
  private final String msg;
  private final Object[] args;
  private final Supplier<String> formattedMsg;

  private ObservedMessage(
      Level level, Instant instant, String msg, Object[] args, Supplier<String> formattedMsg) {
    this.level = level;
    this.instant = instant;
    this.msg = msg;
    this.args = args;
    this.formattedMsg = formattedMsg;
  }

  /**
   * Starts observing provided logger.Delivers collected messages into provided {@code observer}
   * without any delay.
   *
   * @param logger the logger to observe
   * @param observer callback to feed with {@link ObservedMessage} instances
   * @return handle to stop and close the collecting process
   */
  public static AutoCloseable observe(Logger logger, Consumer<ObservedMessage> observer) {
    for (var observing : ServiceLoader.load(Service.class)) {
      var handle = observing.observe(logger, observer);
      if (handle != null) {
        return handle;
      }
    }
    throw new IllegalStateException("No observing service found for " + logger);
  }

  /**
   * Performs given {@code action} while collecting messages from the provided logger. Useful for
   * unit testing. Internaly it is using {@link #observe} method.
   *
   * @param logger the logger to observe
   * @param action action to execute while observing the logger
   * @return logged messages collected while running the {@code action}
   */
  public static List<ObservedMessage> collect(Logger logger, Runnable action) {
    var arr = new ArrayList<ObservedMessage>();
    try (var handle = observe(logger, arr::add)) {
      action.run();
    } catch (Exception ex) {
      throw new IllegalStateException(ex);
    }
    return arr;
  }

  /**
   * Logging level of the message.
   *
   * @return the level
   */
  public Level getLevel() {
    return level;
  }

  /**
   * When the message was logged.
   *
   * @return the instant of the message
   */
  public Instant getInstant() {
    return instant;
  }

  /**
   * Raw message. The actual message before being formatted.
   *
   * @return message
   * @see #getFormattedMessage
   */
  public String getMessage() {
    return msg;
  }

  /**
   * Arguments provided to the message.
   *
   * @return unmodifiable list of arguments provided to the {@link #getMessage()}
   */
  public List<Object> getArguments() {
    return List.of(args);
  }

  /**
   * Human friendly, formatted message.
   *
   * @return formatted version of {@link #getMessage}
   * @see #getMessage
   * @see #getArguments
   */
  public String getFormattedMessage() {
    return formattedMsg.get();
  }

  /**
   * Provides <em>observing capabilities</em> to used logging infrastructure. Different
   * implementations of {@link Logger} have different ways of being <em>observed</em>. If one is
   * providing such a logging implementation, register instance of this class into {@link
   * ServiceLoader}. Such a service will be consulted when {@link #observe} method is used.
   */
  public abstract static class Service {
    /**
     * Start observing provided logger. If the service recognizes the logger it should start
     * observing and return a <em>handle</em> to stop/close the observing any time later.
     *
     * @param logger the logger to observe
     * @param observer consumer to send observed messages to
     * @return non-{@code null} if the service recognizes the logger, return {@code null} if the
     *     service isn't able to start observing
     */
    protected abstract AutoCloseable observe(Logger logger, Consumer<ObservedMessage> observer);

    /**
     * Factory method to create instance of {@link ObservedMessage}.
     *
     * @param level defines {@link ObservedMessage#getLevel()}
     * @param at when the event happened
     * @param msg defines {@link ObservedMessage#getMessage()}
     * @param args defines {@link ObservedMessage#getArguments()}
     * @param formattedMsg defines {@link ObservedMessage#getFormattedMessage()}
     * @return new instance of a message
     */
    protected final ObservedMessage newMessage(
        Level level, Instant at, String msg, Object[] args, Supplier<String> formattedMsg) {
      return new ObservedMessage(level, at, msg, args, formattedMsg);
    }
  }
}
