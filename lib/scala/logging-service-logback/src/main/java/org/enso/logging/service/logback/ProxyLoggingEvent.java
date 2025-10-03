package org.enso.logging.service.logback;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.IThrowableProxy;
import ch.qos.logback.classic.spi.LoggerContextVO;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Marker;
import org.slf4j.event.KeyValuePair;

public final class ProxyLoggingEvent implements ILoggingEvent {
  private final ILoggingEvent underlying;
  private final Map<String, String> mdc;

  public ProxyLoggingEvent(ILoggingEvent underlying, Map<String, String> extra) {
    assert extra != null;
    this.underlying = underlying;
    this.mdc = new HashMap<>();
    if (underlying.getMDCPropertyMap() != null) {
      this.mdc.putAll(underlying.getMDCPropertyMap());
    }
    this.mdc.putAll(extra);
  }

  @Override
  public String getThreadName() {
    return underlying.getThreadName();
  }

  @Override
  public Level getLevel() {
    return underlying.getLevel();
  }

  @Override
  public String getMessage() {
    return underlying.getMessage();
  }

  @Override
  public Object[] getArgumentArray() {
    return underlying.getArgumentArray();
  }

  @Override
  public String getFormattedMessage() {
    return underlying.getFormattedMessage();
  }

  @Override
  public String getLoggerName() {
    return underlying.getLoggerName();
  }

  @Override
  public LoggerContextVO getLoggerContextVO() {
    return underlying.getLoggerContextVO();
  }

  @Override
  public IThrowableProxy getThrowableProxy() {
    return underlying.getThrowableProxy();
  }

  @Override
  public StackTraceElement[] getCallerData() {
    return underlying.getCallerData();
  }

  @Override
  public boolean hasCallerData() {
    return underlying.hasCallerData();
  }

  @Override
  public List<Marker> getMarkerList() {
    return underlying.getMarkerList();
  }

  @Override
  public Map<String, String> getMDCPropertyMap() {

    return this.mdc;
  }

  @Override
  @SuppressWarnings("deprecation")
  public Map<String, String> getMdc() {
    return this.mdc;
  }

  @Override
  public long getTimeStamp() {
    return underlying.getTimeStamp();
  }

  @Override
  public int getNanoseconds() {
    return underlying.getNanoseconds();
  }

  @Override
  public long getSequenceNumber() {
    return underlying.getSequenceNumber();
  }

  @Override
  public List<KeyValuePair> getKeyValuePairs() {
    return underlying.getKeyValuePairs();
  }

  @Override
  public void prepareForDeferredProcessing() {
    underlying.prepareForDeferredProcessing();
  }
}
