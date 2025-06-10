package org.enso.runtime.parser.processor;

import javax.lang.model.element.Element;

/**
 * This exception should be contained only in IR element processing. It is caught in the main
 * processing loop in {@link IRProcessor}.
 */
public final class IRProcessingException extends RuntimeException {
  private final Element element;

  public IRProcessingException(String message, Element element, Throwable cause) {
    super(message, cause);
    this.element = element;
  }

  public IRProcessingException(String message, Element element) {
    super(message);
    this.element = element;
  }

  public Element getElement() {
    return element;
  }
}
