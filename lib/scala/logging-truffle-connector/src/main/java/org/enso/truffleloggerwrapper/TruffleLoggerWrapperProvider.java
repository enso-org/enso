package org.enso.truffleloggerwrapper;

import org.slf4j.ILoggerFactory;
import org.slf4j.IMarkerFactory;
import org.slf4j.helpers.BasicMarkerFactory;
import org.slf4j.helpers.NOPMDCAdapter;
import org.slf4j.spi.MDCAdapter;
import org.slf4j.spi.SLF4JServiceProvider;

/**
 * Binds the SLF4J logger instance within the runtime to a logger which wraps the TruffleLogger.
 *
 * <p>This way, the standard SLF4J interface that is used in other subprojects, can also be used
 * within the runtime and its log messages are correctly passed to the TruffleLogger. Thus libraries
 * that are used both inside and outside of runtime can keep a simple interface.
 *
 * <p>The public interface of this class must conform to what is expected by an SLF4J backend. See
 * slf4j-simple for reference.
 */
@org.openide.util.lookup.ServiceProvider(service = SLF4JServiceProvider.class)
public class TruffleLoggerWrapperProvider implements SLF4JServiceProvider {

  public static String REQUESTED_API_VERSION = "2.0.9";

  private static final TruffleLoggerWrapperFactory factory = new TruffleLoggerWrapperFactory();
  private static final MDCAdapter adapter = new NOPMDCAdapter();
  private static final IMarkerFactory markerFactory = new BasicMarkerFactory();

  @Override
  public ILoggerFactory getLoggerFactory() {
    return factory;
  }

  @Override
  public IMarkerFactory getMarkerFactory() {
    return markerFactory;
  }

  @Override
  public MDCAdapter getMDCAdapter() {
    return adapter;
  }

  @Override
  public String getRequestedApiVersion() {
    return REQUESTED_API_VERSION;
  }

  @Override
  public void initialize() {}
}
